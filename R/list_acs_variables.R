#' @title Easily rename ACS variables
#' @description Given the name of an ACS variable (or a string that matches one or more such variables),
#' generate a named character vector of original variable names and more meaningful names.
#' @param variable_name A named vector (intended for use with named ACS variables).
#' @param census_codebook An object returned from \code{tidycensus::load_variables()}.
#' @returns A named character vector containing the variables that matched
#'    \code{variable_name} from the \code{census_codebook}, with semantically-meaningful names
#'    derived from metadata fields contained in \code{census_codebook}.
#' @examples
#' \dontrun{
#' codebook = tidycensus::load_variables(dataset = "acs5", year = 2022)
#' select_variables_by_name("B16005_", census_codebook = codebook)
#' }
#' @export
#' @importFrom magrittr %>%
select_variables_by_name = function(variable_name, census_codebook) {

  variables = census_codebook %>%
    dplyr::filter(stringr::str_detect(name, !!variable_name)) %>%
    clean_acs_names()

  selected_variables = variables %>% dplyr::pull(name)
  names(selected_variables) = variables %>% dplyr::pull(clean_names)

  return(selected_variables)
}

#' @title Easily filter ACS variables
#' @description Filter the the results of \code{select_variables_by_name()} based on their `match_type` relative
#' to `match_string`.
#' @param variable_vector A named vector (intended for use with named ACS variables).
#' @param match_string A string on which to filter (or not filter) elements in `variable_vector`.
#' @param match_type Whether to include (`match_type = "positive"`) or exclude
#'    (`match_type = "negative"`) matching elements.
#' @returns The elements from `variable_vector` that do/don't match `match_string`.
#' @examples
#' \dontrun{
#' codebook = tidycensus::load_variables(dataset = "acs5", year = 2022)
#' selected_variables = select_variables_by_name("B16005_", census_codebook = codebook)
#' filter_variables(
#'   variable_vector = selected_variables,
#'   match_string = "universe_$|native_$|foreign_born_$|only|very_well",
#'   match_type = "positive")
#' }
#' @export
filter_variables = function(variable_vector, match_string, match_type = "positive") {
  if (match_type == "positive") {
    variable_vector[stringr::str_detect(names(variable_vector), match_string)] }
  else if (match_type == "negative") {
    variable_vector[!stringr::str_detect(names(variable_vector), match_string)] }
  else {
    stop("`match_type` must be 'positive' or 'negative'.") }
}

#' @title Return ACS variables codes and names
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [list_variables()] instead to see available variables, or pass
#' `tables` to [compile_acs_data()].
#' @param year The year for which variable names should be selected.
#' @param tables An optional character vector of table names from the table
#'   registry (e.g., \code{c("race", "snap")}). When provided, only variables
#'   for the specified tables are returned. Use \code{list_tables()} to see
#'   available table names.
#' @returns `NULL`, invisibly. Previously returned a named vector of variable codes.
#' @examples
#' \dontrun{
#' list_acs_variables()
#' }
#' @export
list_acs_variables = function(year = "2022", tables = NULL) {
  lifecycle::deprecate_warn(
    when = "0.1.0",
    what = "list_acs_variables()",
    with = "list_variables()",
    details = "Use `list_variables()` to see available variables, or pass `tables` to `compile_acs_data()`."
  )
  invisible(NULL)
}

#' @title Browse the ACS codebook with clean variable names
#' @description Returns a tibble of ACS variables for the given year, with the
#'   parent table code, raw variable code, and a cleaned snake_case name.
#'   Useful for finding the table code to pass to
#'   \code{compile_acs_data(tables = ...)}.
#' @param year A four-digit year for the five-year ACS estimates (default 2022).
#' @param table An optional ACS table code (e.g., \code{"B22003"}) to filter
#'   results to a single table.
#' @returns A tibble with columns \code{table} (parent ACS table code),
#'   \code{variable_raw} (ACS variable code), and \code{variable_clean}
#'   (snake_case name produced by the package).
#' @examples
#' \dontrun{
#' ## Browse all variables
#' get_acs_codebook()
#'
#' ## Filter to a specific table
#' get_acs_codebook(table = "B22003")
#'
#' ## Search for variables by keyword
#' get_acs_codebook() %>% dplyr::filter(stringr::str_detect(variable_clean, "snap"))
#' }
#' @export
get_acs_codebook = function(year = 2022, table = NULL) {
  suppressWarnings({suppressMessages({
    census_variables = tidycensus::load_variables(year = year, dataset = "acs5")
  })})

  if (!is.null(table)) {
    pattern = paste0("^", table, "_")
    census_variables = census_variables %>%
      dplyr::filter(stringr::str_detect(name, pattern))
    if (nrow(census_variables) == 0) {
      stop(paste0("No variables found for table '", table, "' in year ", year, "."))
    }
  }

  census_variables %>%
    clean_acs_names() %>%
    dplyr::transmute(
      table = stringr::str_extract(name, "^[BC][0-9]{5}[A-I]?(?:PR)?"),
      variable_raw = name,
      variable_clean = stringr::str_remove(clean_names, "_$"))
}

utils::globalVariables(c("name", "concept", "label", "clean_names"))
