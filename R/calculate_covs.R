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
#' @importFrom magrittr %>%

se_simple = function(moe) {
  se = moe / 1.645
  return(se)
}

tidycensus::moe_sum

## this formula can be read as the the square root of the summed standard errors
## i.e.: sqrt(sum(map_dbl(moes, ~ se_simple(.x) ^2 )))
##
se_sum_difference = function(moe, estimate) {

  se = sqrt(sum(purrr::map_dbl(moe, ~ se_simple(.x) ^2 )))
  return(se)
}

#moes = (rlang::!!! rlang::syms(c("sex_by_age_by_disability_status_male_18_34_years_with_a_disability_M", "sex_by_age_by_disability_status_male_35_64_years_with_a_disability_M")))
special_function = function(x) {
  moes = !!! rlang::syms(c("sex_by_age_by_disability_status_male_18_34_years_with_a_disability_M", "sex_by_age_by_disability_status_male_35_64_years_with_a_disability_M"))
  estimates = !!! rlang::syms(c("sex_by_age_by_disability_status_male_18_34_years_with_a_disability", "sex_by_age_by_disability_status_male_35_64_years_with_a_disability"))

  print(moes)
  print(estimates)

  return (se_sum_difference(moe = moes, estimate = estimates)) }
df %>%
  dplyr::rowwise() %>%
  dplyr::transmute(
    dplyr::across(disability_percent, ~ special_function(.x))

se_ratio = function(value_numer, value_denom) {
  (1 / value_denom) * ( sqrt( (se_direct(value_numer) ^ 2) + ( ((value_numer ^ 2) / (value_denom ^ 2)) * ((se_direct(value_denom) ^ 2)) ) ) )
}

## note from Census documentation here: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2018_ACS_Accuracy_Document_Worked_Examples.pdf?
## "if the value under the radical is negative, use the ratio standard error formula instead
se_perc = function(value_numer, value_denom, moe_numer, moe_denom) {
  radical_term_one = (se_direct(moe_numer)^2)
  radical_term_two = ( ( (value_numer^2) / (value_denom^2) ) * (se_direct(moe_denom)^2) )

  ## if the value under the radical is negative, use the ratio SE formula
  se = if_else(
    radical_term_one < radical_term_two,
    se_ratio(value_numer = value_numer, value_denom = value_denom),
    ((1 / value_denom) * sqrt( radical_term_one - radical_term_two )))

  return(se)
}


cv = function(estimate, se) {
  sv = se / estimate * 100

  ## when the estimate is zero, this produces an infinite value
  ## replacing this with a zero
  cv = dplyr::if_else(is.infinite(cv), 0, cv)

  return(CV)
}

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
      numerator_variable_count == 0,
      denominator_variable_count == 0,
      !stringr::str_detect(definition, "calculated variable"),
      no_moe_flag == 0)
  ##
  simple_percent_no_calculated_variables_covs = .data %>%
    dplyr::transmute(
      dplyr::across(
        .cols = any_of(simple_percent_no_calculated_variables_codebook$calculated_variable),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          tidycensus::moe_prop(
            num = get(
              simple_percent_no_calculated_variables_codebook %>%
                dplyr::filter(calculated_variable == current_column) %>%
                dplyr::pull(numerator)),
            denom = get(
              simple_percent_no_calculated_variables_codebook %>%
                dplyr::filter(calculated_variable == current_column) %>%
                dplyr::pull(denominator)),
            moe_num = get(
              simple_percent_no_calculated_variables_codebook %>%
                dplyr::filter(calculated_variable == current_column) %>%
                dplyr::pull(numerator) %>%
                stringr::str_remove_all("_count_estimate") %>%
                paste0("_M")),
            moe_denom = get(
              simple_percent_no_calculated_variables_codebook %>%
                dplyr::filter(calculated_variable == current_column) %>%
                dplyr::pull(denominator) %>%
                stringr::str_remove_all("_count_estimate") %>%
                paste0("_M")))
          },
        .names = "{.col}_cov"),
      dplyr::across(
        .cols = dplyr::matches("_cov$"),
        .fns = ~ .x / 1.645 / get(dplyr::cur_column() %>% stringr::str_remove("_cov$")) * 100))

  ####----Numerator Sums----####
  df %>%
    dplyr::select(disability_percent) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      across(
        .cols = everything(),
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

          print(paste0("Current column: ", current_column))
          print(paste0("Numerator: ", numerator_variables))
          print(paste0("Denominator: ", denominator_variables))

          numerator_moe = tidycensus::moe_sum(
            moe = !!! rlang::syms(numerator_variables %>% paste0("_M")),
            estimate = !!! rlang::syms(numerator_variables),
            na.rm = FALSE)

          # percent_moe = tidycensus::moe_prop(
          #   num = sum(get(numerator_variables), na.rm = TRUE),
          #   denom = get(denominator_variables),
          #   moe_num = numerator_moe,
          #   moe_denom = get(denominator_variables %>% paste0("_M")))
          }))

  df %>%
    dplyr::rowwise() %>%
    dplyr::transmute(
      moe_sum_disability = tidycensus::moe_sum(
        moe = !!! rlang::syms(c("sex_by_age_by_disability_status_male_18_34_years_with_a_disability_M", "sex_by_age_by_disability_status_male_35_64_years_with_a_disability_M")),
        estimate = !!! rlang::syms(c("sex_by_age_by_disability_status_male_18_34_years_with_a_disability", "sex_by_age_by_disability_status_male_35_64_years_with_a_disability")),
        na.rm = FALSE))
  tidycensus::moe_sum

      dplyr::select(definition, numerator, denominator)
  ## variables with subtraction in the denominator are not accurately reflected in the codebook
  codebook1 %>%
    dplyr::filter(!stringr::str_detect(definition, "raw")) %>%
    dplyr::slice(20) %>%
    dplyr::pull(definition)
  ####----Denominator Sums----####

}
