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
#' `tables`/`indicators` to [compile_acs_data()].
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
    details = "Use `list_variables()` to see available variables, or pass `tables`/`indicators` to `compile_acs_data()`."
  )
  invisible(NULL)
}

utils::globalVariables(c("name", "concept", "label", "clean_names"))
