#' Calculates two levels of segregation: a multi-group segregation metric
#' for each larger geographic unit supplied, as well as a decomposed version of the
#' same metric for each smaller geographic unit. Only geographies that are perfectly
#' nested within one another, e.g., tracts within counties, are supported. Note
#' that all segregation calculations rely on `segregation` and users should refer
#' to that package at https://github.com/elbersb/segregation for further implementation
#' details.
#'
#' @param data A dataframe containing a `GEOID` column and the required input measures,
#'    e.g., of race or income, at a single geography (e.g., tract), formatted wide.
#'    the GEOID column must be a character column, and each GEOID must be unique.
#'    inputted data cannot contain other measures. for example: tibble::tribble(
#'        ~GEOID, ~race_nonhispanic_white_alone, ~race_nonhispanic_black_alone,
#'        "37001020100", 2835, 1035,
#'        "37001020200", 1205, 1321)
#' @param nesting_geography_geoid_length The length of the GEOID that identifies nesting
#'    geographies. For example, if smaller_geography_data is defined at the tract
#'    level (with a GEOID of length 11), then nesting_geography_geoid_length = 5 would
#'    return segregation metrics for counties (which have a GEOID of length 5) and
#'    for tracts (relative to other tracts within the same county).
#' @seealso Functions used for underlying segregation calculations are from the `segregation` package.
#' @returns A dataframe comprising segregation estimates and associated p-values at both
#'    geographic levels. Segregation is measured using the Mutual Information Index
#'    (M) at the larger geography and a decomposed version thereof at the smaller
#'    geography level.
#' @examples
#' variables = c(
#'   race_nonhispanic_white_alone_ = "B03002_003",
#'   race_nonhispanic_black_alone_ = "B03002_004",
#'   race_nonhispanic_native_alone_ = "B03002_005",
#'   race_nonhispanic_asian_alone_ = "B03002_006",
#'   race_nonhispanic_nhpi_alone_ = "B03002_007")
#' test_data = tidycensus::get_acs(
#'   geography = "tract",
#'   state = "CT",
#'   variables = variables,
#'   output = "wide") %>%
#'   ## can only include a dataframe with a GEOID column and segregation-related measures
#'    dplyr::select(-NAME)
#' calculate_segregation_metrics(data = test_data, nesting_geography_geoid_length = 5)
#' @export
#' @importFrom magrittr %>%
calculate_segregation_metrics = function(data, nesting_geography_geoid_length) {

  ## Provided data must contain a GEOID column.
  stopifnot("GEOID" %in% colnames(data))

  ## All GEOIDs must be the same length
  stopifnot(
    data %>%
      dplyr::mutate(geoid_length = nchar(GEOID)) %>%
      dplyr::pull(geoid_length) %>%
      unique %>% length == 1)

  geoid_length = data %>% dplyr::pull(GEOID) %>% .[1] %>% nchar

  ## All GEOIDs must be unique
  stopifnot(
    (data %>%
      dplyr::pull(GEOID) %>%
      unique %>%
      length) == data %>% nrow)

  ## The nesting geography GEOID must be shorter than the provided GEOID
  stopifnot(nesting_geography_geoid_length < geoid_length)

  df_segregation = data %>%
    tidyr::pivot_longer(
      cols = -GEOID,
      names_to = "variable",
      values_to = "estimate") %>%
    dplyr::mutate(nesting_geography_geoid = stringr::str_sub(GEOID, 1, nesting_geography_geoid_length))

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
      dplyr::as_tibble() %>%
      dplyr::select(GEOID, segregation_small_geography = ls, p_small_geography = p))

  small_segregation_results = data %>%
    dplyr::select(GEOID) %>%
    dplyr::left_join(segregation_smaller)

  number_error_geographies = small_segregation_results %>%
    dplyr::filter(is.na(segregation_small_geography)) %>%
    nrow

  if (number_error_geographies > 0) {
    warning(paste0("Segregation results are missing for ", number_error_geographies, " geographies.
      This is likely because there was only a single smaller geography within the larger
      geography (e.g., a county comprising a single tract).")) }

  segregation_results = segregation_smaller %>%
    dplyr::mutate(GEOID_larger = stringr::str_sub(GEOID, 1, nesting_geography_geoid_length)) %>%
    dplyr::left_join(segregation_larger, by = c("GEOID_larger" = "nesting_geography_geoid")) %>%
    dplyr::select(-GEOID_larger)

  return(segregation_results)
}

utils::globalVariables(c(
  "nesting_geography_geoid", "H", "p", "segregation_small_geography", "GEOID_larger"))
