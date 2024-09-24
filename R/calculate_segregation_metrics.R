#' @title Calculate segregation at multiple geographies
#' @description Calculate multi-group segregation metrics using the Mutual Information Index (M)
#' @details Given data at a smaller geography (e.g., tract), `calculate_segregation_metrics()`
#'    returns Mutual Information Index (M) values and associated p-values for a perfectly
#'    nested larger geography (e.g., a county or state) as well as decomposed values for the
#'    smaller geography (e.g., tract). Note that all segregation calculations rely on `segregation`
#'    and users should refer to that package at https://github.com/elbersb/segregation for
#'    further implementation details.
#' @param data A dataframe containing a `GEOID` column and the required input measures,
#'    e.g., of race or income, at a single geography (e.g., tract). The `GEOID` column must
#'    be a character column, and each `GEOID` must be unique. If data are formatted wide, there must be
#'    at least two columns in addition to `GEOID`. If data are formatted long, there must be a single
#'    columd in addition to `GEOID`. inputted data cannot contain other measures.For example:
#'    tibble::tribble(
#'        ~GEOID, ~race_nonhispanic_white_alone, ~race_nonhispanic_black_alone,
#'        "37001020100", 2835, 1035,
#'        "37001020200", 1205, 1321)
#' @param data_format Describe the structure of the inputted data. One of "wide" or "long". Data are
#'    returned in the same format in which they are passed to the function.
#' @param nesting_geography_geoid_length The length of the `GEOID` that identifies nesting
#'    geographies. For example, if smaller_geography_data is defined at the tract
#'    level (with a GEOID of length 11), then `nesting_geography_geoid_length = 5` would
#'    return segregation metrics for counties (which have a GEOID of length 5) and
#'    for tracts (relative to other tracts within the same county).
#' @seealso Functions used for underlying segregation calculations are from the `segregation` package.
#' @returns A dataframe comprising segregation estimates and associated p-values at both
#'    geographic levels.
#' @examples
#' \dontrun{
#' variables = c(
#'   race_nonhispanic_white_alone_ = "B03002_003",
#'   race_nonhispanic_black_alone_ = "B03002_004",
#'   race_nonhispanic_native_alone_ = "B03002_005",
#'   race_nonhispanic_asian_alone_ = "B03002_006",
#'   race_nonhispanic_nhpi_alone_ = "B03002_007")
#' df_long = tidycensus::get_acs(
#'   geography = "tract",
#'   state = "SC",
#'   variables = variables,
#'   output = "tidy") %>%
#'   # can only include a GEOID column and segregation-related measures
#'   dplyr::select(-c(NAME, matches("_M$")))
#' calculate_segregation_metrics(
#'   data = df_long,
#'   data_format = "long",
#'   nesting_geography_geoid_length = 5)
#' }
#' @export
#' @importFrom magrittr %>%
calculate_segregation_metrics = function(data, data_format, nesting_geography_geoid_length) {

  ## There are only two formal options for this parameter; "tidy" is also accepted as a synonym for "long"
  stopifnot(data_format %in% c("wide", "long", "tidy"))

  if (data_format == "tidy") {
    warning("data_format == `tidy` is not a formal argument option for the data_format parameter and has been translated to data_format == `long`.")
    data_format = "long" }

  ## Provided data must contain a GEOID column.
  stopifnot("GEOID" %in% colnames(data))

  geoid_length = data %>% dplyr::pull(GEOID) %>% .[1] %>% nchar

  ## The nesting geography GEOID must be shorter than the provided GEOID
  stopifnot(nesting_geography_geoid_length < geoid_length)

  if (data_format == "wide") {
    df_segregation = data %>%
      tidyr::pivot_longer(
        cols = -GEOID,
        names_to = "variable",
        values_to = "estimate") %>%
      dplyr::mutate(nesting_geography_geoid = stringr::str_sub(GEOID, 1, nesting_geography_geoid_length)) }
  if (data_format == "long") {
    df_segregation = data %>%
      dplyr::mutate(nesting_geography_geoid = stringr::str_sub(GEOID, 1, nesting_geography_geoid_length)) }

  segregation_larger = segregation::mutual_within(
      data = df_segregation,
      group = "variable",
      unit = "GEOID",
      weight = "estimate",
      within = "nesting_geography_geoid",
      wide = T) %>%
    dplyr::as_tibble() %>%
    dplyr::select(nesting_geography_geoid, segregation_large_geography = H, p_large_geography = p)

  ## in some cases, there is only a single smaller geography within the encompassing
  ## larger geography. segregation::mutual_local() throws an error in such cases;
  ## this purrr::possibly()-wrapped function instead returns NA values for that
  ## smaller geography
  possible_mutual_local = purrr::possibly(
    segregation::mutual_local,
    otherwise = data.frame(GEOID = NA_character_, ls = NA_real_, p = NA_real_))

  segregation_smaller = purrr::map_dfr(
    df_segregation$nesting_geography_geoid %>% unique,
    ~ possible_mutual_local(
        data = df_segregation %>% dplyr::filter(nesting_geography_geoid == .x),
        group = "variable",
        unit = "GEOID",
        weight = "estimate",
        wide = T) %>%
      dplyr::as_tibble()) %>%
      dplyr::select(GEOID, segregation_small_geography = ls, p_small_geography = p)

  small_segregation_results = data %>%
    dplyr::select(GEOID) %>%
    dplyr::distinct() %>%
    dplyr::left_join(segregation_smaller)

  number_error_geographies = small_segregation_results %>%
    dplyr::filter(is.na(segregation_small_geography)) %>%
    nrow()

  if (number_error_geographies > 0) {
    input_data_missingness = data %>%
      dplyr::filter(dplyr::if_any(.cols = -GEOID, ~is.na(.x))) %>%
      nrow()

    warning(paste0("Segregation results are missing for ", number_error_geographies, " geographies.
The input data contained missing values for ", input_data_missingness, " observations.
The remaining ", number_error_geographies - input_data_missingness, " observations may be missing because there was only
a single smaller geography within the larger geography (e.g., a
county comprising a single tract).")) }

  if (data_format == "long") {
    segregation_results = dplyr::bind_rows(
      segregation_larger %>%
        dplyr::rename(
          GEOID = nesting_geography_geoid,
          segregation = segregation_large_geography,
          segregation_p = p_large_geography),
      small_segregation_results %>%
        dplyr::rename(
          segregation = segregation_small_geography,
          segregation_p = p_small_geography)) }

  if (data_format == "wide") {
    segregation_results = small_segregation_results %>%
      dplyr::mutate(GEOID_larger = stringr::str_sub(GEOID, 1, nesting_geography_geoid_length)) %>%
      dplyr::left_join(segregation_larger, by = c("GEOID_larger" = "nesting_geography_geoid")) %>%
      dplyr::select(-GEOID_larger) }

  return(segregation_results)
}

utils::globalVariables(c(
  "nesting_geography_geoid", "H", "p", "segregation_small_geography", "GEOID_larger",
  "segregation_large_geography", "p_large_geography", "p_small_geography"))
