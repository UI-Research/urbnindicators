#' @importFrom magrittr %>%

#' @title Calculate a simple standard error
#' @details Create a standard error at the 90% level from a 90% margin of error
#' @param moe A margin of error, or a vector thereof
#' @returns A 90% standard error
se_simple = function(moe) {
  se = purrr::map_dbl(moe,  ~ .x / 1.645)
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
  if (length(dots) == 1) { dots = dots[[1]] }

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

  if (!is.null(moe_numerator)) { se_numerator = purrr::map_dbl(moe_numerator, se_simple) }
  if (!is.null(moe_denominator)) { se_denominator = purrr::map_dbl(moe_denominator, se_simple) }

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

  return(se)
}

#' @title Calculate a coefficient of variation
#' @details Return a coefficient of variation at the 90% level
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

## helper function -- pull out the numerator and denominator from the definition
extract_definition_terms = function(.definition, .type) {
  .definition %>%
    stringr::str_extract_all(paste0(.type, " .*?\\.")) %>%
    stringr::str_remove_all("Numerator = |Denominator = |Sum of: |\\.") %>%
    stringr::str_remove_all("\\(.*?\\)") %>%
    stringr::str_trim() %>% stringr::str_squish() %>%
    stringr::str_replace_all(" ,", ",")
}

#' @title Calculate Coefficients of Variation (CVs)
#' @details Create CVs for all ACS estimates and derived indicators
#' @param .df The dataset returned from \code{compile_acs_data()}.
#'  The argument to this parameter must have an attribute named `codebook` (as is)
#'  true of results from \code{compile_acs_data()}.
#' @returns A modified dataframe that includes newly calculated indicators.
#' @examples
#' \dontrun{
#' df = compile_acs_data(
#'   variables = list_acs_variables(year = 2022),
#'   years = c(2022),
#'   geography = "county",
#'   states = "NJ",
#'   counties = NULL,
#'   spatial = FALSE)
#' cvs = calculate_cvs(df) %>%
#'  dplyr::select(matches("_cv$"))
#' }
#
# df = compile_acs_data(
#   variables = list_acs_variables(year = 2022),
#   years = c(2022),
#   geography = "county",
#   states = "NJ",
#   counties = NULL,
#   spatial = FALSE)

calculate_cvs = function(.df) {

  ## the codebook attached to the default compile_acs_data() return
  codebook = .df %>% attr("codebook")

  ## modified codebook prepared for calculating CVs
  codebook1 = codebook %>%
    dplyr::distinct(calculated_variable, .keep_all = TRUE) %>%
    dplyr::filter(!stringr::str_detect(calculated_variable, "_M$")) %>%
    dplyr::mutate(
      numerator_variable_count = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_real_,
        variable_type %in% c("Metadata") ~ NA_real_,
        TRUE ~ stringr::str_extract(definition, "Numerator = .*\\.") %>% stringr::str_count(",") + 1),
      denominator_variable_count = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_real_,
        variable_type %in% c("Metadata", "Sum") ~ NA_real_,
        TRUE ~ stringr::str_extract(definition, "Denominator = .*\\.") %>% stringr::str_count(",|-") + 1),
      numerator = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_character_,
        variable_type %in% c("Metadata") ~ NA_character_,
        variable_type == "Sum" ~ extract_definition_terms(definition, .type = "Sum"),
        TRUE ~ extract_definition_terms(definition, .type = "Numerator")),
      denominator = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_character_,
        variable_type %in% c("Metadata", "Sum") ~ NA_character_,
        TRUE ~ extract_definition_terms(definition, .type = "Denominator")),
      ## these variables do not have associated MOEs when returned from the Census API
      no_moe_flag = dplyr::case_when(
        denominator %in% c("sex_by_age_universe", "race_universe") ~ 1,
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

  simple_percent_no_calculated_variables_codebook = codebook1 %>%
    dplyr::filter(
      variable_type == "Percent",
      numerator_variable_count == 1,
      denominator_variable_count == 1,
      !stringr::str_detect(definition, "calculated variable"),
      no_moe_flag == 0)

  numerator_sum_codebook = codebook1 %>%
    dplyr::filter(
      variable_type == "Percent",
      numerator_variable_count > 1,
      denominator_variable_count == 1,
      !stringr::str_detect(definition, "calculated variable"),
      no_moe_flag == 0)

  denominator_sum_codebook = codebook1 %>%
    dplyr::filter(
      variable_type == "Percent",
      numerator_variable_count == 1,
      denominator_variable_count > 1,
      !stringr::str_detect(definition, "calculated variable"),
      no_moe_flag == 0)

  numerator_denominator_sum_codebook = codebook1 %>%
    dplyr::filter(
      variable_type == "Percent",
      numerator_variable_count > 1,
      denominator_variable_count > 1,
      !stringr::str_detect(definition, "calculated variable"),
      no_moe_flag == 0)

  summed_variable_codebook = codebook1 %>%
    dplyr::filter(
      variable_type == "Sum",
      !stringr::str_detect(definition, "calculated variable"),
      no_moe_flag == 0)

  calculated_variables_dependencies_codebook = codebook1 %>%
    dplyr::filter(calculated_variable_dependency_flag == 1)

  ## percents where there's no MOE for the denominator
  no_moe_codebook = codebook1 %>%
    dplyr::filter(no_moe_flag == 1, variable_type == "Percent")

  ## simple SE calculation variables
  raw_variables_codebook = codebook1 %>%
    dplyr::filter(
      stringr::str_detect(definition, "This is a raw ACS estimate."),
      !(calculated_variable %in% c("total_population_universe", "sex_by_age_universe", "race_universe")))

  df_cvs = .df %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      ## derived sum variables - calculate MOEs that can be used in subsequent calculations
      dplyr::across(
        .cols = dplyr::any_of(summed_variable_codebook$calculated_variable),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          numerator_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(numerator) %>%
            stringr::str_remove_all("_count_estimate") %>%
            stringr::str_split(", ") %>%
            unlist() %>%
            paste0("_M")

          se = se_sum(purrr::map(numerator_variables, ~ .df %>% dplyr::pull(.x)))
          moe = se * 1.645

          return(moe) },
        .names = "{.col}_M"),
      ## raw ACS variables
      dplyr::across(
        .cols = dplyr::any_of(c(
          ## true raw ACS variables
          raw_variables_codebook$calculated_variable %>% stringr::str_remove("_count_estimate"),
          ## summed raw ACS variables with derived MOEs calculated above
          summed_variable_codebook$calculated_variable)),
        .fns = ~ se_simple(get(dplyr::cur_column() %>% paste0("_M"))),
        .names = "{.col}_cv"),
      ## percent variables with a denominator that doesn't have an MOE
      ## these are treated as if there is no denominator
      dplyr::across(
        .cols = dplyr::any_of(no_moe_codebook$calculated_variable),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          numerator_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(numerator) %>%
            stringr::str_remove_all("_count_estimate") %>%
            stringr::str_split(", ") %>%
            unlist() %>%
            paste0("_M")

          se = se_sum(purrr::map(numerator_variables, ~ .df %>% dplyr::pull(.x)))

          return(se) },
        .names = "{.col}_cv"),
      ## simple percent variables: one numerator, one denominator
      dplyr::across(
        .cols = dplyr::any_of(simple_percent_no_calculated_variables_codebook$calculated_variable),
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
                paste0("_M"))) },
        .names = "{.col}_cv"),
      ## percents with summed/subtracted numerators, one denominator
      dplyr::across(
        .cols = dplyr::any_of(numerator_sum_codebook$calculated_variable),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          numerator_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(numerator) %>%
            stringr::str_remove_all("_count_estimate") %>%
            stringr::str_split(", ") %>%
            unlist() %>%
            paste0("_M")

          denominator_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(denominator) %>%
            stringr::str_remove_all("_count_estimate") %>%
            stringr::str_split(", ") %>%
            unlist() %>%
            paste0("_M")

          se = se_proportion_ratio(
            estimate_numerator = rowSums(dplyr::select(., dplyr::all_of(numerator_variables))),
            estimate_denominator = get(denominator_variables),
            se_numerator = se_sum(purrr::map(numerator_variables, ~ .df %>% dplyr::pull(.x))),
            se_denominator = se_sum(purrr::map(denominator_variables, ~ .df %>% dplyr::pull(.x))))

          return(se) },
        .names = "{.col}_cv"),
      ## convert all "_cv" suffixed columns to true coefficients of variation
      ## (preceding calculations yield standard errors)
      dplyr::across(
        .cols = dplyr::matches("_cv$"),
        .fns = ~ cv(
          estimate = get(dplyr::cur_column() %>% stringr::str_remove("_cv$")),
          se = .x)))

   return(df_cvs)
}

utils::globalVariables(c(
  "calculated_variable", "numerator_variable_count", "denominator_variable_count",
  "no_moe_flag", "calculated_variable_dependency_flag", "numerator", "denominator"))
