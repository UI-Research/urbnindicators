#' @title Document variables from \code{compile_acs_data()}
#' @description Define how variables produced via \code{urbnindicators::compile_acs_data()}
#' are calculated.
#' @details Generates a tibble of variable names and definitions that describe
#' how each variable was created.
#' @param .data The dataset returned from \code{urbnindicators::compile_acs_data()}.
#' @param resolved_tables A character vector of resolved table names from the
#'   table registry. When NULL (default), all registered tables are used.
#' @param auto_table_entries A list of auto-generated table entries from
#'   \code{build_auto_table_entry()}. Default is an empty list.
#' @param user_definitions A list of user-supplied DSL definition objects
#'   (e.g., from \code{define_percent()}). Default is an empty list.
#' @returns A tibble containing the names and definitions of  variables returned from
#' \code{urbnindicators::compile_acs_data()}.
#' @examples
#' \dontrun{
#' df = compile_acs_data(
#'   years = c(2024),
#'   geography = "county",
#'   states = "NJ",
#'   counties = NULL,
#'   spatial = FALSE) %>%
#'   dplyr::select(-dplyr::matches("_M$"))
#' codebook = generate_codebook(.data = df)
#' }
#' @importFrom magrittr %>%
#' @keywords internal

generate_codebook = function(.data, resolved_tables = NULL, auto_table_entries = list(), user_definitions = list(), year = 2024) {

  .data = .data %>%
    sf::st_drop_geometry()

  if (is.null(resolved_tables)) {
    resolved_tables = names(.table_registry$tables)
  }

  ####----Variable Crosswalk----####
  ## Build crosswalk from registry: clean_name -> raw ACS code
  crosswalk_rows = purrr::map(resolved_tables, function(table_name) {
    table_entry = get_table(table_name)
    if (is.null(table_entry) || is.null(table_entry[["raw_variables"]])) return(NULL)

    raw_variable_codes = table_entry[["raw_variables"]]
    clean_names = names(raw_variable_codes) %>% stringr::str_remove("_$")
    data.frame(
      raw_name = as.character(raw_variable_codes),
      clean_name = clean_names,
      stringsAsFactors = FALSE)
  }) %>% purrr::compact()

  ## Also build crosswalk for select_variables()-sourced variables using
  ## the census codebook
  suppressMessages({suppressWarnings({
    census_variables = tidycensus::load_variables(year = year, dataset = "acs5")
  })})

  ## Collect all ACS table codes from the registry
  all_acs_tables = purrr::map(resolved_tables, function(table_name) {
    table_entry = get_table(table_name)
    if (is.null(table_entry)) return(NULL)

    codes = c()
    if (!is.null(table_entry[["acs_tables"]])) {
      codes = c(codes, table_entry[["acs_tables"]])
    }
    if (!is.null(table_entry[["raw_variable_source"]]) && table_entry[["raw_variable_source"]][["type"]] == "select_variables") {
      selection_codes = purrr::map_chr(table_entry[["raw_variable_source"]][["calls"]], function(selection_call) {
        selection_call[["pattern"]] %>% stringr::str_extract("B[0-9]{5}")
      })
      codes = c(codes, selection_codes)
    }
    codes
  }) %>% purrr::compact() %>% unlist() %>% unique()

  if (length(all_acs_tables) > 0) {
    raw_variable_codes = paste0(all_acs_tables, collapse = "|")

    dependencies = census_variables %>%
      dplyr::filter(stringr::str_detect(name, raw_variable_codes)) %>%
      clean_acs_names() %>%
      dplyr::mutate(clean_names = clean_names %>% stringr::str_remove("_$"))

    crosswalk_rows[[length(crosswalk_rows) + 1]] = data.frame(
      raw_name = dependencies$name,
      clean_name = dependencies$clean_names,
      stringsAsFactors = FALSE)
  }

  ## Add auto-table raw variables to crosswalk
  if (length(auto_table_entries) > 0) {
    auto_crosswalk_rows = purrr::map(auto_table_entries, function(auto_entry) {
      raw_variable_codes = auto_entry[["raw_variables"]]
      clean_names_vec = names(raw_variable_codes) %>% stringr::str_remove("_$")
      data.frame(
        raw_name = as.character(raw_variable_codes),
        clean_name = clean_names_vec,
        stringsAsFactors = FALSE)
    })
    crosswalk_rows = c(crosswalk_rows, auto_crosswalk_rows)
  }

  variable_name_crosswalk = dplyr::bind_rows(crosswalk_rows) %>%
    dplyr::distinct(clean_name, .keep_all = TRUE)

  ####----Expand Codebook Entries from Registry----####
  partial_documentation = purrr::map(
    resolved_tables,
    function(table_name) {
      table_entry = get_table(table_name)
      if (is.null(table_entry) || is.null(table_entry[["definitions"]]) || length(table_entry[["definitions"]]) == 0) {
        return(tibble::tibble(calculated_variable = character(0),
                              variable_type = character(0),
                              definition = character(0),
                              numerator_vars = list(),
                              numerator_subtract_vars = list(),
                              denominator_vars = list(),
                              denominator_subtract_vars = list()))
      }
      purrr::map(
        table_entry[["definitions"]],
        ~ expand_codebook_entry(entry = .x, .data = .data, crosswalk = variable_name_crosswalk)) %>% purrr::list_rbind()
    }) %>% purrr::list_rbind()

  ## Expand auto-table definitions into codebook rows
  if (length(auto_table_entries) > 0) {
    auto_documentation = purrr::map(auto_table_entries, function(auto_entry) {
      if (is.null(auto_entry[["definitions"]]) || length(auto_entry[["definitions"]]) == 0) {
        return(tibble::tibble(calculated_variable = character(0),
                              variable_type = character(0),
                              definition = character(0),
                              numerator_vars = list(),
                              numerator_subtract_vars = list(),
                              denominator_vars = list(),
                              denominator_subtract_vars = list()))
      }
      purrr::map(
        auto_entry[["definitions"]],
        ~ expand_codebook_entry(entry = .x, .data = .data, crosswalk = variable_name_crosswalk)) %>% purrr::list_rbind()
    }) %>% purrr::list_rbind()

    partial_documentation = dplyr::bind_rows(partial_documentation, auto_documentation)
  }

  ## Expand user-supplied definitions into codebook rows
  if (length(user_definitions) > 0) {
    user_documentation = purrr::map(
      user_definitions,
      ~ expand_codebook_entry(entry = .x, .data = .data, crosswalk = variable_name_crosswalk)) %>% purrr::list_rbind()

    partial_documentation = dplyr::bind_rows(partial_documentation, user_documentation)
  }

  ####----Raw Variables----####
  ## collect all raw variable clean names from the resolved tables
  raw_variable_names = variable_name_crosswalk$clean_name %>%
    unique() %>%
    ## only include those that actually appear in .data
    purrr::keep(~ .x %in% colnames(.data))

  ####----Assemble Codebook----####
  ## raw variables: populate list columns with empty vectors
  raw_rows = tibble::tibble(
    calculated_variable = raw_variable_names,
    numerator_vars = purrr::map(raw_variable_names, ~ character(0)),
    numerator_subtract_vars = purrr::map(raw_variable_names, ~ character(0)),
    denominator_vars = purrr::map(raw_variable_names, ~ character(0)),
    denominator_subtract_vars = purrr::map(raw_variable_names, ~ character(0)))

  ## metadata entries: populate list columns with empty vectors
  metadata_names = c("data_source_year", "GEOID", "NAME", "area_land_sq_kilometer",
                     "area_water_sq_kilometer", "area_land_water_sq_kilometer", "geometry")
  metadata_rows = tibble::tribble(
    ~calculated_variable, ~definition, ~variable_type,
    "data_source_year", "End year of five-year ACS period from which the estimates were queried.", "Metadata",
    "GEOID", "A federally-issued identifier of the geographic unit.", "Metadata",
    "NAME", "The name of the geographic unit.", "Metadata",
    "area_land_sq_kilometer", "Land area of the geographic unit, in square kilometers.", "Metadata",
    "area_water_sq_kilometer", "Water area of the geographic unit, in square kilometers.", "Metadata",
    "area_land_water_sq_kilometer", "Combined land and water area of the geographic unit, in square kilometers.", "Metadata",
    "geometry", "The spatial geometry attributes of the geographic unit.", "Metadata") %>%
    dplyr::mutate(
      numerator_vars = purrr::map(metadata_names, ~ character(0)),
      numerator_subtract_vars = purrr::map(metadata_names, ~ character(0)),
      denominator_vars = purrr::map(metadata_names, ~ character(0)),
      denominator_subtract_vars = purrr::map(metadata_names, ~ character(0)))

  result1 = raw_rows %>%
    dplyr::bind_rows(partial_documentation) %>%
    dplyr::bind_rows(metadata_rows) %>%
    dplyr::mutate(
      definition = dplyr::case_when(
        calculated_variable %in% raw_variable_names ~ "This is a raw ACS estimate.",
        !is.na(definition) ~ definition),
      variable_type = dplyr::case_when(
        stringr::str_detect(calculated_variable, "median.*income") ~ "Median ($)",
        stringr::str_detect(calculated_variable, "cost.*median") ~ "Median ($)",
        stringr::str_detect(calculated_variable, "median") ~ "Median",
        stringr::str_detect(calculated_variable, "average") ~ "Average",
        stringr::str_detect(calculated_variable, "quintile") ~ "Quintile ($)",
        stringr::str_detect(calculated_variable, "index") ~ "Index",
        is.na(variable_type) ~ "Count",
        .default = variable_type))

  ####----Add SE Calculation Type and Aggregation Strategy----####
  result1 = result1 %>%
    dplyr::mutate(
      numerator_count = purrr::map_int(numerator_vars, length) +
        purrr::map_int(numerator_subtract_vars, length),
      denominator_count = purrr::map_int(denominator_vars, length) +
        purrr::map_int(denominator_subtract_vars, length),
      se_calculation_type = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ "raw",
        variable_type == "Metadata" ~ "metadata",
        variable_type %in% c("Median ($)", "Median", "Average", "Quintile ($)", "Index") ~ "weighted_average",
        stringr::str_detect(definition, "^One minus") ~ "one_minus",
        variable_type == "Sum" ~ "sum",
        numerator_count == 1 & denominator_count == 1 ~ "simple_percent",
        numerator_count > 1 & denominator_count == 1 ~ "complex_numerator",
        numerator_count == 1 & denominator_count > 1 ~ "complex_denominator",
        numerator_count > 1 & denominator_count > 1 ~ "complex_both",
        TRUE ~ "unknown")) %>%
    dplyr::select(-numerator_count, -denominator_count) %>%
    ensure_aggregation_strategy()

  return(result1)
}

## Add aggregation_strategy column to a codebook tibble based on variable_type.
## Shared by generate_codebook(), interpolate_acs(), and .aggregate_to_target().
ensure_aggregation_strategy = function(codebook) {
  if ("aggregation_strategy" %in% colnames(codebook)) return(codebook)
  codebook %>%
    dplyr::mutate(
      aggregation_strategy = dplyr::case_when(
        variable_type %in% c("Count", "Sum") ~ "sum",
        variable_type == "Percent" ~ "recalculate_percent",
        variable_type %in% c("Median ($)", "Median", "Average", "Quintile ($)", "Index") ~ "weighted_average",
        variable_type == "Metadata" ~ "metadata",
        TRUE ~ "unknown"))
}

utils::globalVariables(c(
  "variable_name", "domain", "variable_type", "definition", "x", "y", "clean_variable_name",
  "raw_variable_name", "raw_name", "clean_name", "variable_definition_year", "value",
  "calculated_variable", "replacement", "denominators", "inputs", "denominators1",
  "denominators2", "inputs_formatted", "denominators_formatted", "variable_crosswalk",
  "inputs_raw", "denominators_raw", "outputs", "count",
  "numerator_vars", "numerator_subtract_vars", "denominator_vars", "denominator_subtract_vars",
  "numerator_count", "denominator_count", "se_calculation_type", "aggregation_strategy"))
