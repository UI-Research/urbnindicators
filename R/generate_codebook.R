#' @title Document variables from `compile_acs_data()`
#' @description `generate_codebook()` defines how variables produced via `compile_acs_data()`
#' are calculated.
#' @details Generates a tibble of variable names and definitions that describe
#' how each variable was created.
#' @param .data The dataset returned from `compile_acs_data()`.
#' @returns A tibble containing the names and definitions of  variables returned from
#' `compile_acs_data()`.
#' @examples
#' \dontrun{
#' df = compile_acs_data(
#'   variables = list_acs_variables(year = 2022),
#'   years = c(2022),
#'   geography = "county",
#'   states = "NJ",
#'   counties = NULL,
#'   retain_moes = TRUE,
#'   spatial = FALSE)
#' codebook = generate_codebook(.data = df)
#' }
#' @importFrom magrittr %>%
generate_codebook = function(.data)  {

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

    census_variables = tidycensus::load_variables(year = 2022, dataset = "acs5")

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
      dplyr::bind_rows(variable_crosswalk %>% dplyr::select(raw_name = raw_variable_name, clean_name = clean_variable_name))


    ####----Document Across Call (Function)----#####
    document_across_call = function(across_call) {

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
          colnames }

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
            denominator = across_call %>%
              as.character %>%
              .[3] %>%
              stringr::str_extract(",.*\\)") %>%
              stringr::str_remove_all(", |\\)")
          }
          return(denominator)
        })

      codebook_dataframe = tibble::tibble(
        outputs = output_columns,
        inputs = input_columns,
        denominators = denominator_columns) %>%
          dplyr::left_join(variable_name_crosswalk %>% dplyr::select(inputs_raw = raw_name, clean_name), by = c("inputs" = "clean_name")) %>%
          dplyr::left_join(variable_name_crosswalk %>% dplyr::select(denominators_raw = raw_name, clean_name), by = c("denominators" = "clean_name"))

      purrr::pmap_dfr(
        codebook_dataframe,
        function(outputs, inputs, denominators, inputs_raw, denominators_raw) {
          tibble::tibble(
            calculated_variable = outputs,
            variable_type = variable_type) %>%
            dplyr::mutate(
              definition = dplyr::case_when(
                variable_type == "Percent" ~ paste0(
                  "Numerator = ", inputs, " (", inputs_raw, "). ",
                  "Denominator = ", denominators, " (", denominators_raw, ")."),
                variable_type == "Sum" ~ paste0(
                  "Sum of: ", inputs, " (", inputs_raw, "), ",
                  denominators, " (" , denominators_raw, ").")) %>%
                stringr::str_replace_all("\\(NA\\)", "calculated variable")) })
    }

    ####----Get Across Variable Names (Function)----####
    get_across_variable_names = function(match_expression) {
      selected_columns = .data %>%
        dplyr::select(dplyr::matches(match_expression), -dplyr::matches("_M$|percent$")) %>%
        colnames
    }


    ####----Get rowSums Variables (Function)----####
    get_rowsums_variables = function(expression) {

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
          raw_variable_code = variable_name_crosswalk %>% dplyr::filter(clean_name == x) %>% dplyr::pull(raw_name)
          result = dplyr::if_else(length(raw_variable_code) == 0, paste0(x, " (calculated variable)"), paste0(x, " (", raw_variable_code, ")"))
        }

        numerator_formatted = numerator %>%
          as.character() %>%
          purrr::map_chr(formatter) %>%
          paste0(collapse = ", ")
        denominator_formatted = denominator %>%
          as.character() %>%
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

    uncalculated_variables = colnames(.data)[!(colnames(.data) %in% partial_documentation$calculated_variable)]

    ## some final adjustments
    result = tibble::tibble(calculated_variable = uncalculated_variables) %>%
      dplyr::bind_rows(partial_documentation) %>%
      dplyr::mutate(
        definition = dplyr::case_when(
          calculated_variable == "data_source_year" ~ "End year of five-year ACS period from which the estimates were queried.",
          calculated_variable == "geography" ~ "Geography at which estimates are specified.",
          calculated_variable == "GEOID" ~ "A federally-issued identifier of the geographic unit.",
          calculated_variable == "NAME" ~ "The name of the geographic unit.",
          calculated_variable == "area_land_sq_kilometer" ~ "Land area of the geographic unit, in square kilometers.",
          calculated_variable == "area_water_sq_kilometer" ~ "Water area of the geographic unit, in square kilometers.",
          calculated_variable == "area_land_water_sq_kilometer" ~ "Combined land and water area of the geographic unit, in square kilometers.",
          calculated_variable == "population_density_land_sq_kilometer" ~ "Rate. Numerator: total_population_universe (B01003_001). Denominator: area_land_sq_kilometer.",
          calculated_variable == "geometry" ~ "The spatial goemetry attributes of the geographic unit.",
          !is.na(definition) ~ definition,
          .default = "This is a raw ACS estimate."),
        variable_type = dplyr::case_when(
          calculated_variable %in% c("data_source_year", "GEOID", "NAME", "geometry", "geography") ~ "Metadata",
          stringr::str_detect(calculated_variable, "median.*income") ~ "Median ($)",
          stringr::str_detect(calculated_variable, "cost.*median") ~ "Median ($)",
          stringr::str_detect(calculated_variable, "median") ~ "Median",
          stringr::str_detect(calculated_variable, "average") ~ "Average",
          stringr::str_detect(calculated_variable, "quintile") ~ "Quintile ($)",
          stringr::str_detect(calculated_variable, "index") ~ "Index",
          is.na(variable_type) ~ "Count",
          .default = variable_type))

    return(result)
}

utils::globalVariables(c(
  "variable_name", "domain", "variable_type", "definition", "x", "y", "clean_variable_name",
  "raw_variable_name", "raw_name", "clean_name", "variable_definition_year", "value"))
