#' @importFrom magrittr %>%

#' @title Calculate a simple standard error
#' @details Create a standard error at the 90% level from a margin of error
#' @param .moe A margin of error, or a vector thereof
#' @returns A 90% standard error
se_simple = function(moe) {
  se = moe / 1.645
  return(se)
}

#' @title Calculate a pooled standard error for a summed or subtracted estimate
#' @details For an estimate derived by adding or subtracting multiple estimates, calculate the pooled standard error
#' @param ... The unquoted names of the margin of error variables to be added or subtracted
#' @returns A pooled 90% standard error
se_sum = function(...) {
  dots = list(...)

  ## depending on how / where this is called, the dots may be a list of lists
  ## where the top-level list is unneeded and only has a length of one
  if (length(dots) == 1) { dots = dots[[1]]}

  ## convert margins of error to standard errors
  ## square each standard error
  ## sum (rowwise) standard errors
  ## take the square root of the summed standard errors
  se = dots %>%
    purrr::map(se_simple) %>%
    purrr::map(~ .x ^2) %>%
    purrr::pmap_dbl(sum) %>%
    purrr::map_dbl(sqrt)

  return(se)
}

#' @title Calculate a pooled standard  for a proportion or ratio estimate
#' @details For an estimate derived by dividing multiple estimates, calculate the pooled standard error
#' @param estimate_numerator The estimate of the numerator
#' @param estimate_denominator The estimate of the denominator
#' @param moe_numerator The margin of error of the numerator
#' @param moe_denominator The margin of error of the denominator
#' @param se_numerator The standard error of the numerator
#' @param se_denominator The standard error of the denominator
#' @param type The type of estimate being calculated, either "proportion" or "ratio"
#' @returns A pooled 90% standard error
se_proportion_ratio = function(
    estimate_numerator,
    estimate_denominator,
    moe_numerator = NULL,
    moe_denominator = NULL,
    se_numerator = NULL,
    se_denominator = NULL,
    type = "proportion") {

  if (all(is.null(c(moe_numerator, se_numerator))) | all(is.na(c(moe_denominator, se_denominator)))) {
    stop("A margin of error or standard error must be provided for both the numerator and the denominator.")
  }

  if( ((!is.null(moe_numerator) & !is.null(se_numerator))) | (!is.null(moe_denominator) & !is.null(se_denominator))) {
    stop("Only one of a margin of error or a standard error can be provided for the numerator and denominator.")
  }

  if (!is.null(moe_numerator)) { se_numerator = se_simple(moe_numerator) }
  if (!is.null(moe_denominator)) { se_denominator = se_simple(moe_denominator) }

  ## squared standard error of the numerator
  radical_term_one = se_numerator %>% `^`(2)

  ## squared numerator over the squared denominator times the squared standard error of the denominator
  radical_term_two = ( (estimate_numerator ^ 2) / (estimate_denominator ^ 2) ) *
    (se_denominator %>% `^`(2))

  ## if the value under the radical is negative, use the formula for ratio standard errors
  ## i.e., add the radical terms, rather than subtract them
  se = dplyr::if_else(
    radical_term_one < radical_term_two | type == "ratio",
    ((1 / estimate_denominator) * sqrt( radical_term_one + radical_term_two )),
    ((1 / estimate_denominator) * sqrt( radical_term_one - radical_term_two )))

}

#' @title Calculate a coefficient of variation
#' @details
#' @param estimate The estimate
#' @param se The standard error
#' @returns A coefficient of variation at the 90% level
cv = function(estimate, se) {
  cv = se / estimate * 100

  ## when the estimate is zero, this produces an infinite value
  ## replacing this with a zero
  cv = dplyr::if_else(is.infinite(cv), 0, cv)

  return(cv)
}

# temp = tibble::tibble(
#   estimate1 = c(1, 2, 3, 4, 5),
#   estimate2 = c(2, 3, 4, 5, 6),
#   estimate3 = c(3, 4, 5, 6, 7),
#   moe1 = c(.5, .5, 1, 3, 4),
#   moe2 = c(1, 1, 2, 4, 5),
#   moe3 = c(.75, 1, .5, 2, 2.3))
#
# temp %>%
#   dplyr::mutate(
#     #se_sum_1_2 = se_sum_difference(moe1, moe2),
#     #se_sum_1_2_3 = se_sum_difference(moe1, moe2, moe3),
#     se_proportion_ratio_1_2 = se_proportion_ratio(
#       estimate_numerator = estimate1,
#       estimate_denominator = estimate2,
#       moe_numerator = moe1,
#       moe_denominator = moe2))#,
#     #cv1 = cv(sum(estimate1, estimate2), se_sum_1_2),
#     #cv_proportion_1_2 = cv((estimate1 / estimate2), se_proportion_ratio_1_2))

#' @title Calculate Coefficients of Variation (CVs)
#' @details Create CVs for all ACS estimates and derived indicators
#' @param .data The dataset returned from `compile_acs_data()`.
#' @returns A modified dataframe that includes newly calculated indicators.
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
#' internal_compute_acs_variables(.data = df)
#' }
calculae_covs = function(.data) {

  ## testing / development only

  .data = compile_acs_data(
    variables = list_acs_variables(year = 2022),
    years = c(2022),
    geography = "county",
    states = "NJ",
    counties = NULL,
    retain_moes = TRUE,
    spatial = FALSE)

  ## the attached to the default compile_acs_data() return
  codebook = .data %>%
    attr("codebook")

  ## helper function -- pull out the numerator and denominator from the definition
  extract_definition_terms = function(.definition, .type) {
    .definition %>%
      stringr::str_extract_all(paste0(.type, " = .*?\\.")) %>%
      stringr::str_remove_all("Numerator = |Denominator = |\\.") %>%
      stringr::str_remove_all("\\(.*?\\)") %>%
      stringr::str_trim() %>% stringr::str_squish() %>%
      stringr::str_replace_all(" ,", ",")
  }

  ## modified codebook prepared for calculating CVs
  codebook1 = codebook %>%
    dplyr::mutate(
      numerator_variable_count = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_real_,
        variable_type %in% c("Metadata", "Sum") ~ NA_real_,
        TRUE ~ stringr::str_extract(definition, "Numerator = .*\\.") %>% stringr::str_count(",") + 1),
      denominator_variable_count = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_real_,
        variable_type %in% c("Metadata", "Sum") ~ NA_real_,
        TRUE ~ stringr::str_extract(definition, "Denominator = .*\\.") %>% stringr::str_count(",") + 1),
      numerator = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_character_,
        variable_type %in% c("Metadata", "Sum") ~ NA_character_,
        TRUE ~ extract_definition_terms(definition, .type = "Numerator")),
      denominator = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_character_,
        variable_type %in% c("Metadata", "Sum") ~ NA_character_,
        TRUE ~ extract_definition_terms(definition, .type = "Denominator")),
      ## these variables do not have associated MOEs when returned from the Census API
      no_moe_flag = dplyr::case_when(
        calculated_variable %in% c("sex_by_age_universe", "race_universe") ~ 1,
        TRUE ~ 0),
      calculated_variable_dependency_flag = dplyr::if_else(
        stringr::str_detect(definition, "calculated variable"), 1, 0),
      moe_type = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ "raw",
        stringr::str_detect(definition, "minus") ~ "minus",
        stringr::str_detect(definition, "sum") & variable_type == "Sum" ~ "sum",
        numerator_variable_count == 1 & denominator_variable_count == 1 ~ "simple percent",
        numerator_variable_count == 1 & denominator_variable_count > 1 ~ "complex denominator percent",
        numerator_variable_count > 1 & denominator_variable_count == 1 ~ "complex numerator percent",
        numerator_variable_count > 1 & denominator_variable_count > 1 ~ "complex numerator and complex denominator percent",
        TRUE ~ "unspecified"))

  ####----Variable Typologies----####

  ## variable_type == "Percent"
  ##    - simple: one numerator / one denominator
  ##        - example: snap_received_percent = snap_received / snap_universe
  ##    - simple minus: 1 - simple percent
  ##        - example: race_personofcolor_percent = 1 - race_nonhispanic_white_alone_percent
  ##        - denominator of the simple percent - minus the numerator of the simple percent / denominator of the simple percent
  ##    - numerator sum: multiple numerators / one denominator
  ##        - example:
  ##    - denominator sum: one numerator / multiple denominators
  ##        - example:
  ##    - complex numerator and denominator: multiple numerators / multiple operations to calculate denominator
  ##        - example: cost_burdened_30percentormore_allincomes_percent

  ## baseline - raw ACS variable



  ####----Simple Percentages----####

  ## for variables without a returned MOE, we calculate the coefficient of variation
  ## using the margin of error of the numerator only, rather than the pooled margin of error
  ## typically calculated

  ## All relying only on ACS estimate variables (no derived or "calculated" variables
  ## used to calculate the numerator or denominator)
  simple_percent_no_calculated_variables_codebook = codebook1 %>%
    dplyr::filter(
      variable_type == "Percent",
      numerator_variable_count == 1,
      denominator_variable_count == 1,
      !stringr::str_detect(definition, "calculated variable"),
      no_moe_flag == 0)
  ##
  simple_percent_no_calculated_variables_covs = .data %>%
    dplyr::transmute(
      dplyr::across(
        .cols = any_of(simple_percent_no_calculated_variables_codebook$calculated_variable),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          se_proportion_ratio(
            estimate_numerator = get(
              simple_percent_no_calculated_variables_codebook %>%
                dplyr::filter(calculated_variable == current_column) %>%
                dplyr::pull(numerator)),
            estimate_denominator = get(
              simple_percent_no_calculated_variables_codebook %>%
                dplyr::filter(calculated_variable == current_column) %>%
                dplyr::pull(denominator)),
            moe_numerator = get(
              simple_percent_no_calculated_variables_codebook %>%
                dplyr::filter(calculated_variable == current_column) %>%
                dplyr::pull(numerator) %>%
                stringr::str_remove_all("_count_estimate") %>%
                paste0("_M")),
            moe_denominator = get(
              simple_percent_no_calculated_variables_codebook %>%
                dplyr::filter(calculated_variable == current_column) %>%
                dplyr::pull(denominator) %>%
                stringr::str_remove_all("_count_estimate") %>%
                paste0("_M")))
          },
        .names = "{.col}_cov"),
      dplyr::across(
        .cols = dplyr::matches("_cov$"),
        .fns = ~ cv(
          estimate = get(dplyr::cur_column() %>% stringr::str_remove("_cov$")),
          se = .x))
        )


  ####----Numerator Sums----####
  numerator_sum_codebook = codebook1 %>%
    dplyr::filter(
      variable_type == "Percent",
      numerator_variable_count > 1,
      denominator_variable_count == 1,
      !stringr::str_detect(definition, "calculated variable"),
      no_moe_flag == 0)

  .data %>%
    dplyr::mutate(
      across(
        .cols = any_of(numerator_sum_codebook$calculated_variable),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          numerator_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(numerator) %>%
            stringr::str_remove_all("_count_estimate") %>%
            stringr::str_split(", ") %>%
            unlist()

          denominator_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(denominator) %>%
            stringr::str_remove_all("_count_estimate") %>%
            stringr::str_split(", ") %>%
            unlist()

          if (length(denominator_variables) != 1) {
            stop("The denominator must be a single variable.")
          }

          if (any(!is.na(numerator_variables))) {
            print("numerator_variables exists.")
          }

          se = se_proportion_ratio(
            estimate_numerator = rowSums(dplyr::select(., all_of(numerator_variables))),
            estimate_denominator = get(denominator_variables),
            se_numerator = se_sum(purrr::imap(numerator_variables, ~ .data[[numerator_variables[.y]]])),
            moe_denominator = get(denominator_variables %>% stringr::str_remove_all("_count_estimate") %>% paste0("_M")))

          return(se)

          }))

  ####----Denominator Sums----####

}
