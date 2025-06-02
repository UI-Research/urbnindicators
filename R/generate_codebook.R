#' @title Document variables from \code{compile_acs_data()}
#' @description Define how variables produced via \code{urbnindicators::compile_acs_data()}
#' are calculated.
#' @details Generates a tibble of variable names and definitions that describe
#' how each variable was created.
#' @param .data The dataset returned from \code{urbnindicators::compile_acs_data()}.
#' @returns A tibble containing the names and definitions of  variables returned from
#' \code{urbnindicators::compile_acs_data()}.
#' @examples
#' \dontrun{
#' df = compile_acs_data(
#'   variables = list_acs_variables(year = 2022),
#'   years = c(2022),
#'   geography = "county",
#'   states = "NJ",
#'   counties = NULL,
#'   spatial = FALSE)
#' codebook = generate_codebook(.data = df)
#' }
#' @importFrom magrittr %>%

generate_codebook = function(.data)  {
    .data = .data %>%
      sf::st_drop_geometry()
    ####----Variable Crosswalk----####
    expression_list = rlang::enexpr(list_acs_variables) %>% as.list()
    list_acs_expression = expression_list[[2]][[4]][[3]]

    ## this covers all the manually named variables, does not include those selected via select_variables()
    variable_crosswalk = list_acs_expression %>%
      purrr::imap(
        function(x, y) {
          data.frame(
            x = as.character(x),
            y = as.character(y)) }) %>%
      dplyr::bind_rows() %>%
      dplyr::transmute(
        raw_variable_name = x,
        clean_variable_name = y %>% stringr::str_remove("_$")) %>%
      dplyr::filter(clean_variable_name != "")

    suppressMessages({suppressWarnings({
      census_variables = tidycensus::load_variables(year = 2022, dataset = "acs5")})})

    raw_variable_codes = list_acs_expression %>%
      as.character() %>%
      stringr::str_extract("B[0-9]{5}[_[0-9]{3}]?") %>%
      paste0(collapse = "|")

    ## this includes all select_variables()-selected variables
    dependencies = census_variables %>%
      dplyr::filter(stringr::str_detect(name, raw_variable_codes)) %>%
      dplyr::mutate(
        clean_names = paste0(
          concept, "_", # e.g., "tenure_white_alone_householder"
          label) %>% # e.g.,  "owner_occupied"
          stringr::str_to_lower() %>%
          stringr::str_remove_all("\\(|\\)") %>%
          stringr::str_replace_all(c(
            "\\!\\!" = "_",
            "\\:" = "_",
            "estimate_total" = "",
            "'" = "",
            '"' = "",
            "," = "",
            "_and_" = "_",
            " to " = "_",
            " " = "_",
            "_{2,}" = "_",
            "_$" = "",
            "\\." = "_",
            "__" =  "_",
            "-" = "_",
            "_$" = "",
            "_and_" = "_",
            "_or_" = "_",
            "_the_" = "_",
            "_for_" = "_",
            "_of_" = "_",
            "__" = "_",
            "native_hawaiian_other_pacific_islander" = "nhpi",
            "hispanic_latino" = "hispanic",
            "american_indian_alaska_native" = "aian",
            "black_african_american" = "black",
            "household_income_by_gross_rent_as_a_percentage_of_household_income_in_the_past_12_months" =
            "household_income_by_gross_rent_as_a_percentage_of_household_income")),
        clean_names = dplyr::if_else(label == "Estimate!!Total:", clean_names %>% paste0("_universe_"), paste0(clean_names, "_")),
        clean_names = clean_names %>% stringr::str_remove("_$"))

    ## this should cover all of the variables returned by list_acs_variables() / compile_acs_data()
    variable_name_crosswalk = dependencies %>%
      dplyr::select(raw_name = name, clean_name = clean_names) %>%
      dplyr::bind_rows(variable_crosswalk %>% dplyr::select(raw_name = raw_variable_name, clean_name = clean_variable_name)) %>%
      ## a handful of count columns end in "percent"; we've renamed them to clarify, and are adjusting the clean names accordingly
      ## in this crosswalk
      dplyr::mutate(
        clean_name = dplyr::case_when(
          stringr::str_detect(clean_name, "household_income_by_gross_rent.*percent$") ~ paste0(clean_name, "_count_estimate"),
          TRUE ~ clean_name))

    ####----Document Across Call (Function)----#####
    document_across_call = function(across_call) {
      # for testing:
      # across_call = mutate_call

      selection_term = across_call %>%
        rlang::call_args() %>%
        .[[".cols"]] %>%
        rlang::call_args() %>%
        as.character

      ## there are multiple variable selection criteria (across which to mutate)
      if (length(selection_term) > 1) {
        positive_matches = selection_term %>%
          purrr::discard(~ stringr::str_detect(.x, "-dplyr::matches")) %>%
          purrr::keep(~ stringr::str_detect(.x, "dplyr::matches")) %>%
          stringr::str_remove_all('dplyr::matches|\\(|\\)|"') %>%
          paste0(collapse = "|")
        negative_matches = selection_term %>%
          purrr::keep(~ stringr::str_detect(.x, "-dplyr::matches")) %>%
          stringr::str_remove_all('dplyr::matches|\\(|\\)|"|-') %>%
          paste0(collapse = "|")
        positive_columns = selection_term %>%
          purrr::discard(~ stringr::str_detect(.x, "dplyr::matches|-"))
        negative_columns = selection_term %>%
          purrr::discard(~ stringr::str_detect(.x, "dplyr::matches")) %>%
          purrr::keep(~ stringr::str_detect(.x, "-")) %>%
          stringr::str_remove_all("-")

        ## so that it doesn't match anything and also doesn't throw an error
        if (nchar(negative_matches) < 1) { negative_matches = "zZz" }

        input_columns = .data %>%
          dplyr::select(c(
            dplyr::matches(positive_matches),
            -dplyr::matches(negative_matches),
            -dplyr::matches("percent$|_M$"),
            dplyr::all_of(positive_columns),
            -dplyr::all_of(negative_columns))) %>%
          colnames

      } else {
        input_columns = .data %>%
          dplyr::select(dplyr::matches(selection_term)) %>%
          dplyr::select(-dplyr::matches("percent$|_M$")) %>%
          colnames
        }

      output_column_naming_syntax = across_call %>%
        as.character %>%
        .[4] %>%
        stringr::str_replace("\\.col", ".x")

      variable_type = dplyr::if_else(stringr::str_detect(output_column_naming_syntax, "percent"), "Percent", "Sum")
      output_columns = purrr::map_chr(input_columns, ~ stringr::str_glue(output_column_naming_syntax))

      denominator_columns = purrr::map_chr(
        input_columns,
        function(input) {
          ## in the case that the denominator varies (using get (cur_column() ...))
          if (stringr::str_detect(across_call %>% as.character %>% .[3], "dplyr::cur_column")) {
            ## extract the renaming syntax used to convert the input variable into
            ## the denominator
            denominator_function = across_call %>%
              as.character %>%
              .[3] %>%
              stringr::str_extract("cur_column\\(\\).*\\)\\)") %>%
              stringr::str_remove_all('cur_column\\(\\) %>%') %>%
              stringr::str_replace("\\){2,5}", "\\)") %>%
              rlang::parse_expr()

            ## For the below example:
            ## across(
            ##    .cols = matches("federal_poverty_limit.*below"),
            ##    .fns = ~safe_divide(.x, get(cur_column() %>% str_replace("below", "universe"))),
            #     .names = "{.col}_percent"))
            # denominator_function has three elements: str_replace, "below", and "universe"
            # in this case, we want to insert "input" as "string"; no subsetting required

            ## However, in this example, the .fns argument is more complex
            #   across(
            #   .cols = matches("year_structure_built_built_[0-9].*"),
            #   .fns = ~safe_divide(.x, get(cur_column() %>% str_replace("[0-9].*", "universe") %>% str_replace("built_", ""))),
            #   .names = "{.col}_percent")

            # denominator_function[[1]] = %>%
            # denominator_function[[2]] = str_replace("[0-9].*", "universe")
            # denominator_function[[3]] = str_replace("built_", "")

            ## we conditionally assign the string argument to the input value accordingly
            if (any(denominator_function[[1]] %>% as.character == "%>%")) { denominator_function[[2]]$string = input }
            else { denominator_function$string = input }

            denominator = eval(denominator_function)
          } else {
            denominator =
              across_call %>%
              as.character %>%
              .[3] %>%
              stringr::str_extract(",.*\\)") %>%
              stringr::str_remove_all(", |\\)|\\(") %>%
              stringr::str_replace(" - ", " MINUS ")
          }
          return(denominator)
        })

      codebook_dataframe = tibble::tibble(
          outputs = output_columns,
          inputs = input_columns,
          denominators = denominator_columns) %>%
        dplyr::mutate(
          denominators2 = dplyr::case_when(
            stringr::str_detect(denominators, "MINUS") ~ stringr::str_extract(denominators, "MINUS .*") %>% stringr::str_remove_all("MINUS "),
            TRUE ~ NA_character_),
          denominators1 = dplyr::case_when(
            stringr::str_detect(denominators, "MINUS") ~ stringr::str_extract(denominators, ".* MINUS") %>% stringr::str_remove(" MINUS"),
            TRUE ~ denominators)) %>%
        dplyr::select(-denominators) %>%
        tidyr::pivot_longer(cols = c(denominators1, denominators2), names_to = "denominator_type", values_to = "denominators") %>%
        dplyr::filter(!is.na(denominators)) %>%
        dplyr::left_join(
          variable_name_crosswalk %>%
            dplyr::select(inputs_raw = raw_name, clean_name),
          by = c("inputs" = "clean_name")) %>%
        dplyr::left_join(
          variable_name_crosswalk %>%
            dplyr::select(denominators_raw = raw_name, clean_name),
          by = c("denominators" = "clean_name")) %>%
        dplyr::group_by(outputs) %>%
        dplyr::mutate(
          count = dplyr::n()) %>%
        dplyr::ungroup()

      multiple_denominator_flag = codebook_dataframe %>% dplyr::filter(count > 1) %>% nrow() > 0

      codebook_dataframe %>%
        dplyr::rename(calculated_variable = outputs) %>%
        dplyr::mutate(
          dplyr::across(.cols = c(inputs_raw, denominators_raw), ~ dplyr::if_else(is.na(.x), "calculated variable", .x)),
          inputs_formatted = stringr::str_c(inputs, " (", inputs_raw, ")"),
          denominators_formatted = stringr::str_c(denominators, " (", denominators_raw, ")")) %>%
        dplyr::group_by(calculated_variable) %>%
        { if (multiple_denominator_flag)
            dplyr::summarize(.,
              inputs = dplyr::first(inputs_formatted),
              denominators = paste0(denominators_formatted, collapse = " - "))
          else
            dplyr::summarize(.,
              inputs = stringr::str_c(inputs_formatted, collapse = ", "),
              denominators = stringr::str_c(denominators_formatted, collapse = ",")) } %>%
        dplyr::mutate(
          calculated_variable = calculated_variable,
          variable_type = variable_type,
          definition = dplyr::case_when(
            variable_type == "Percent" ~ stringr::str_c("Numerator = ", inputs, ". ", "Denominator = ", denominators, "."),
            variable_type == "Sum" ~ stringr::str_c("Sum of: ", inputs, ", ", denominators, ".")))
    }

    ####----Get Across Variable Names (Function)----####
    get_across_variable_names = function(match_expression) {
      selected_columns = .data %>%
        dplyr::select(dplyr::matches(match_expression), -dplyr::matches("_M$|percent$")) %>%
        colnames
    }

    ####----Get rowSums Variables (Function)----####
    get_rowsums_variables = function(expression) {
      # expression = numerator
      if (any(expression %>% as.character %>% stringr::str_detect("select"))) {
        summed_variables = expression %>%
          rlang::call_args() %>%
          paste0(collapse = "") %>%
          stringr::str_extract("\\(.*\\)") %>%
          stringr::str_remove_all("\\(\\.,|\\)") %>%
          stringr::str_trim() %>%
          stringr::str_split(", ") %>%
          unlist()
      }

      if (any(expression %>% as.character %>% stringr::str_detect("across|matches"))) {
        summed_variables = expression %>%
          as.character() %>%
          purrr::keep(~ stringr::str_detect(.x, "across|matches")) %>%
          stringr::str_extract("matches.*") %>%
          stringr::str_remove_all('matches\\(|\\){1,4}$|"') %>%
          get_across_variable_names()
        }

      return(summed_variables)
    }

    ####----Define Codebook Variable (Function)----####
    # mutate_call of the form:
    # compile_acs_expression[[2]][[3]] %>% .[2:length(.)]

    define_codebook_variable = function(mutate_call, mutate_call_name) {

       # mutate_call = rlang::expr(
       #   safe_divide(
       #     rowSums(dplyr::select(., dplyr::matches("means_transportation_work_(bicycle|walked)$"))),
       #     (means_transportation_work_universe - means_transportation_work_worked_from_home))
       # )

      if (any((mutate_call[[1]] %>% as.character) == "safe_divide")) {
        numerator = rlang::call_args(mutate_call) %>% .[[1]]
        denominator = rlang::call_args(mutate_call) %>% .[[2]]

        ## addressing cases where either the numerator or denominator is the sum
        ## of multiple variables
        if (length(numerator %>% as.character %>% purrr::keep(~ stringr::str_detect(.x, "rowSums"))) > 0) {
          numerator = get_rowsums_variables(numerator) }

        if (length(denominator %>% as.character %>% purrr::keep(~ stringr::str_detect(.x, "rowSums"))) > 0) {
          denominator = get_rowsums_variables(denominator) }

        formatter = function(x) {
          ## in the case that the denominator contains a subtraction term
          if (stringr::str_detect(x, " - ")) {
            result = x %>%
              stringr::str_split(" - ") %>%
              unlist() %>%
              purrr::map_chr(
                function(x) {
                  raw_variable_code = variable_name_crosswalk %>% dplyr::filter(clean_name == x) %>% dplyr::pull(raw_name)
                  result = dplyr::if_else(
                    length(raw_variable_code) == 0,
                    paste0(x, " (calculated variable)"),
                    paste0(x, " (", raw_variable_code, ")")) }) %>%
              paste0(collapse = " - ")
            } else {
              raw_variable_code = variable_name_crosswalk %>% dplyr::filter(clean_name == x) %>% dplyr::pull(raw_name)
              result = dplyr::if_else(length(raw_variable_code) == 0, paste0(x, " (calculated variable)"), paste0(x, " (", raw_variable_code, ")"))
          }
          return(result)
        }

        numerator_formatted = numerator %>%
          as.character() %>%
          purrr::map_chr(formatter) %>%
          paste0(collapse = ", ")
        denominator_formatted = denominator %>%
          as.character() %>%
          purrr::keep(~ .x != "(") %>%
          purrr::map_chr(formatter) %>%
          paste0(collapse = ", ")

        result = tibble::tibble(
          calculated_variable = mutate_call_name,
          variable_type = "Percent",
          definition = paste0(
            "Numerator = ", numerator_formatted, ". ",
            "Denominator = ", denominator_formatted, ".") %>%
            stringr::str_replace_all("\\, \\.", "\\."))
      }

      else if (any(mutate_call[[1]] %>% as.character() == "across")) {
        result = document_across_call(mutate_call) }

      else if (any(mutate_call[[1]] %>% as.character() == "rowSums")) {
        summed_variables = mutate_call[2] %>%
          as.character() %>%
          stringr::str_extract("matches\\(.*?\\)") %>%
          stringr::str_remove_all('matches|\\(|\\)|"') %>%
          get_across_variable_names() %>%
          paste0(collapse = ", ")

        result = tibble::tibble(
          calculated_variable = mutate_call_name,
          variable_type = "Sum",
          definition = paste0("Sum of ", summed_variables, ". "))
        }

      ## case when there's a subtraction using rowSums-grouped sets of variables
      else if (mutate_call[[2]] %>% as.character %>% .[1] == "-") {
        rowsummed_variables = mutate_call %>%
          rlang::call_args() %>%
          as.character %>%
          stringr::str_split(" - ") %>% ## this creates a length-one list
          .[[1]] %>% ## comprising two character vectors of variables
          purrr::map_chr( ~
            stringr::str_extract(.x, "matches\\(.*?\\)") %>%
            stringr::str_remove_all('matches|\\(|\\)|"') %>%
            get_across_variable_names() %>%
            paste0(collapse = ", "))

        added_variables = rowsummed_variables[1]
        subtracted_variables = rowsummed_variables[2]

        result = tibble::tibble(
          calculated_variable = mutate_call_name,
          variable_type = "Sum",
          definition = paste0("Sum of ", added_variables, " MINUS ", subtracted_variables, ". ")) }

      ## case when there's just basic subtraction
      else if (mutate_call[[1]] %>% as.character == "-") {
        result = tibble::tibble(
          calculated_variable = mutate_call_name,
          variable_type = "Percent",
          definition = paste0("One minus ", as.character(mutate_call)[length(as.character(mutate_call))], ".")) }

      ## unidentified case
      else {
        result = tibble::tibble(
          calculated_variable = mutate_call_name,
          variable_type = "Error.",
          definition = paste0("Error. Unidentified variable definition. The call to create this variable was: ", as.character(mutate_call))) }

      return(result)
    }

    ## this is a list-type rendering of the call that calculates acs variables
    compile_acs_expression = rlang::enexpr(internal_compute_acs_variables) %>%
      as.list() %>% .[[2]] %>% .[[2]] %>% as.list()

    ## this traverses each of the dplyr::mutate() calls and generates variable documentation
    partial_documentation = c(
      ## mutate call one
      compile_acs_expression[[2]][[3]] %>% .[2:length(.)],
      ## mutate call two
      compile_acs_expression[[3]] %>% .[2:length(.)]) %>%
        purrr::map_dfr(
          function(mutate_chain) {
            mutate_chain %>%
              purrr::imap_dfr( ~ define_codebook_variable(mutate_call = .x, mutate_call_name = .y)) })

    uncalculated_variables = names(list_acs_variables()) %>% unique %>% stringr::str_remove("_$")

    ## some adjustments
    result1 = tibble::tibble(calculated_variable = uncalculated_variables) %>%
      dplyr::bind_rows(partial_documentation) %>%
      dplyr::bind_rows(tibble::tribble(
        ~calculated_variable, ~definition, ~variable_type,
        "data_source_year", "End year of five-year ACS period from which the estimates were queried.", "Metadata",
   #     "geography", "Geography at which estimates are specified.", "Metadata",
        "GEOID", "A federally-issued identifier of the geographic unit.", "Metadata",
        "NAME", "The name of the geographic unit.", "Metadata",
        "area_land_sq_kilometer", "Land area of the geographic unit, in square kilometers.", "Metadata",
        "area_water_sq_kilometer", "Water area of the geographic unit, in square kilometers.", "Metadata",
        "area_land_water_sq_kilometer", "Combined land and water area of the geographic unit, in square kilometers.", "Metadata",
        "population_density_land_sq_kilometer", "Rate. Numerator: total_population_universe (B01003_001). Denominator: area_land_sq_kilometer.", "Metadata",
        "geometry", "The spatial goemetry attributes of the geographic unit.", "Metadata")) %>%
      dplyr::mutate(
        definition = dplyr::case_when(
          calculated_variable %in% uncalculated_variables ~ "This is a raw ACS estimate.",
          !is.na(definition) ~ definition),
        variable_type = dplyr::case_when(
          stringr::str_detect(calculated_variable, "median.*income") ~ "Median ($)",
          stringr::str_detect(calculated_variable, "cost.*median") ~ "Median ($)",
          stringr::str_detect(calculated_variable, "median") ~ "Median",
          stringr::str_detect(calculated_variable, "average") ~ "Average",
          stringr::str_detect(calculated_variable, "quintile") ~ "Quintile ($)",
          stringr::str_detect(calculated_variable, "index") ~ "Index",
          is.na(variable_type) ~ "Count",
          .default = variable_type)) %>%
      dplyr::select(-c(inputs, denominators))

    result2 = purrr::map_dfr(
      result1 %>% dplyr::filter(stringr::str_detect(definition, "calculated variable")) %>% dplyr::pull(calculated_variable),
      function(variable) {
        numerator_calculated_variables = result1 %>%
          dplyr::filter(calculated_variable == !!variable) %>%
          dplyr::pull(definition) %>%
          stringr::str_remove_all("Numerator = | Denominator.*|\\.| |,") %>%
          stringr::str_split("variable\\)") %>%
          unlist() %>%
          purrr::keep(~ stringr::str_detect(.x, "\\(calculated")) %>%
          stringr::str_remove_all("\\(calculated|^.* - ") %>% ## this second group is for subtraction cases... not a great solution
          stringr::str_trim()

        denominator_calculated_variables = result1 %>%
          dplyr::filter(calculated_variable == !!variable) %>%
          dplyr::pull(definition) %>%
          stringr::str_remove_all("Numerator.*\\. Denominator = |\\.") %>%
          stringr::str_split("variable\\)") %>%
          unlist() %>%
          purrr::keep(~ stringr::str_detect(.x, "\\(calculated")) %>%
          stringr::str_remove_all("\\(calculated|^.* - ") %>% ## this second group is for subtraction cases... not a great solution
          stringr::str_trim()

        replacement_values_df = result1 %>%
          dplyr::filter(calculated_variable %in% c(numerator_calculated_variables, denominator_calculated_variables)) %>%
          dplyr::mutate(
            calculated_variable = stringr::str_c(calculated_variable, " (calculated variable)"),
            numerator = dplyr::if_else(calculated_variable %in% numerator_calculated_variables, 1, 0),
            denominator = dplyr::if_else(calculated_variable %in% denominator_calculated_variables, 1, 0),
            replacement = definition %>% stringr::str_remove_all("Sum of: |\\."))

        replacement_values_named_vector = replacement_values_df %>%
          dplyr::pull(replacement) %>%
          purrr::set_names(replacement_values_df$calculated_variable %>% stringr::str_replace_all(c("\\(" = "\\\\(", "\\)" = "\\\\)")))

        result2 = result1 %>%
          dplyr::filter(calculated_variable == !!variable) %>%
          dplyr::mutate(
            definition = dplyr::if_else(
              calculated_variable == !!variable,
              definition %>% stringr::str_replace_all(replacement_values_named_vector),
              definition)) })

    result3 = result1 %>%
      dplyr::filter(!(calculated_variable %in% result2$calculated_variable)) %>%
      dplyr::bind_rows(result2) %>%
      dplyr::mutate(
        calculated_variable = dplyr::if_else(
          stringr::str_detect(calculated_variable, "percent$") & variable_type == "Count",
          stringr::str_replace(calculated_variable, "percent$", "pct"),
          calculated_variable))

    return(result3)
}

utils::globalVariables(c(
  "variable_name", "domain", "variable_type", "definition", "x", "y", "clean_variable_name",
  "raw_variable_name", "raw_name", "clean_name", "variable_definition_year", "value",
  "calculated_variable", "replacement", "denominators", "inputs", "denominators1",
  "denominators2", "inputs_formatted", "denominators_formatted", "variable_crosswalk",
  "inputs_raw", "denominators_raw", "outputs", "count"))
