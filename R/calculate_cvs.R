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
#' @param ... The unquoted names of the margin of error and corresponding estimate variables to be added or subtracted
#' @returns A pooled 90% standard error
se_sum = function(...) {
  dots = list(...)

  moes = dots[[1]] %>%
    as.data.frame() %>%
    stats::setNames(paste0("x", seq_along(.))) %>%
    dplyr::mutate(
      observation = dplyr::row_number(),
      type = "moe")
  estimates = dots[[2]] %>%
    as.data.frame() %>%
    stats::setNames(paste0("x", seq_along(.))) %>%
    dplyr::mutate(
      observation = dplyr::row_number(),
      type = "estimate")

  data_long = rbind(moes, estimates) %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(
      cols = -c(observation, type),
      names_to = "name",
      values_to = "value")  %>%
    tidyr::pivot_wider(
      names_from = type,
      values_from = value)

  ## if there are multiple zero-estimate observations that are summed,
  ## Census recommends taking only the largest MOE corresponding to these estimates:
  se =
    dplyr::bind_rows(
      data_long %>% dplyr::filter(estimate != 0),
      data_long %>% dplyr::filter(is.na(estimate)),
      data_long %>%
        dplyr::filter(estimate == 0) %>%
        dplyr::slice_max(order_by = moe, by = "observation", n = 1)) %>%
    dplyr::group_split(observation) %>%
    purrr::map(~ .x %>% dplyr::pull(moe)) %>%
    purrr::map(se_simple) %>%
    purrr::map(~ .x ^2) %>%
    purrr::map(sum) %>%
    purrr::map_dbl(sqrt)

  stopifnot(nrow(moes) == length(se))

  return(se)
}

#' @title Calculate a pooled standard error for a proportion or ratio
#' @details For an estimate derived using division--whether the resulting estimate is a proportion or a ratio--calculate the pooled standard error. While there are convenience parameters that support both SEs and MOEs for the numerator and denominator, only one of these--either SEs or MOEs--can be supplied--the other must be left NULL.
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

  if (all(is.null(moe_numerator), is.null(se_numerator)) | all(is.null(moe_denominator), is.null(se_denominator))) {
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
  ## replacing this with an NA value
  cv = dplyr::if_else(is.infinite(cv), NA, cv)

  return(cv)
}

## helper function -- pull out the numerator and denominator from the definition
extract_definition_terms = function(.definition, .type) {
  .definition %>%
    stringr::str_extract_all(paste0(.type, " .*?\\.")) %>%
    stringr::str_remove_all("Numerator = |Denominator = |Sum of: |\\.") %>%
    stringr::str_remove_all("\\(.*?\\)") %>%
    stringr::str_trim() %>% stringr::str_squish() %>%
    stringr::str_replace_all(c(" ," = ",", " -" = ","))
}

#' @title Calculate coefficients of variation
#' @details Create CVs for all ACS estimates and derived indicators
#' @param .df The dataset returned from \code{compile_acs_data()}.
#'  The argument to this parameter must have an attribute named `codebook` (as is
#'  true of results from \code{compile_acs_data())}.
#' @returns A modified dataframe that includes newly calculated indicators.
#' @keywords internal
calculate_cvs = function(.df) {
  warning("Coefficients of variation and related calculated measures of error such
          as margins of error (for derived variables) and standard errors are
          experimental features and should be used with  caution.
          Such measures are respectively suffixed with `_CV`, `_M`, and `_SE`.")

  ## the codebook attached to the default compile_acs_data() return
  codebook = .df %>% attr("codebook")

  ## source: https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes/2024-02.html
  ## these are the variables (at least for 2023) that at times have controlled
  ## estimates. for these variables, if the MOE in the raw data is missing, we
  ## set the MOE equal to 0, as controlled estimates have no sampling error.
  ## (note that group quarters estimates are also controlled at the state and national
  ## levels, but this package does not return group quarters estimates)
  controlled_variables = c(
    "total_population_universe", "sex_by_age_universe", "race_universe",
    "race_hispanic_allraces", "race_nonhispanic_allraces") %>% stringr::str_c("_M")
  
  .df = .df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(controlled_variables),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x) ))

  ## modified codebook prepared for calculating CVs
  codebook1 = codebook %>%
    dplyr::distinct(calculated_variable, .keep_all = TRUE) %>%
    dplyr::filter(!stringr::str_detect(calculated_variable, "_M$")) %>%
    dplyr::mutate(
      numerator_count = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_real_,
        variable_type %in% c("Metadata", "Sum") ~ NA_real_,
        # Extract numerator definition and count comma-separated variables
        TRUE ~ stringr::str_extract(definition, "Numerator = .*\\.") %>% stringr::str_count(",") + 1),
      denominator_count = dplyr::case_when(
        definition == "This is a raw ACS estimate." ~ NA_real_,
        variable_type %in% c("Metadata", "Sum") ~ NA_real_,
        # Extract denominator definition and count comma or dash-separated variables
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
      moe_type = dplyr::case_when(
        stringr::str_detect(definition,"This is a raw ACS estimate") ~ "raw",
        stringr::str_detect(definition, "minus") ~ "minus",
        stringr::str_detect(definition, "(Sum|sum)") & variable_type == "Sum" ~ "sum",
        numerator_count == 1 & denominator_count == 1 ~ "simple percent",
        numerator_count == 1 & denominator_count > 1 ~ "complex denominator percent",
        numerator_count > 1 & denominator_count == 1 ~ "complex numerator percent",
        numerator_count > 1 & denominator_count > 1 ~ "complex numerator and complex denominator percent",
        TRUE ~ "unspecified"),
      variable_class = dplyr::case_when(
        ## for example: snap_received_percent
        moe_type == "simple percent" ~ "simple percent, no calculated variables",
        ## for example: disability_percent
        moe_type == "complex numerator percent" ~ "numerator sum percent",
        ## for example: means_transportation_work_bicycle_percent
        moe_type == "complex denominator percent" ~ "denominator sum percent",
        ## for example: cost_burdened_30percentormore_allincomes_percent
        moe_type == "complex numerator and complex denominator percent" ~ "numerator and denominator sum percent",
        ## for example: age_10_14_years
        moe_type == "sum" ~ "sum",
        ## basic SE calculations with se_simple
        ## for example: snap_universe
        moe_type == "raw" ~ "raw",
        moe_type == "minus" ~ "one minus percentage",
        TRUE ~ "non-ACS variable"))

  ## all variables for which to calculate CVs
  cv_variables = codebook1 %>%
    dplyr::filter(!variable_class %in% c("non-ACS variable", "no MOE count")) %>%
    dplyr::pull(calculated_variable)

  ## a named list of of variables, with each list element named by the
  ## variable class it encompasses
  variable_class_groups = codebook1 %>%
    dplyr::filter(calculated_variable %in% cv_variables) %>%
    dplyr::group_split(variable_class)
  variable_class_names = variable_class_groups %>%
    purrr::map_chr(~ .x %>% dplyr::pull(variable_class) %>% unique())
  variable_classes = variable_class_groups %>%
    purrr::map(~ .x %>% dplyr::pull(calculated_variable)) %>%
    stats::setNames(variable_class_names)

  df_cvs1 = .df %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      ## derived sum variables - calculate MOEs that can be used in subsequent calculations
      ## for example: age_10_14_years
      dplyr::across(
        .cols = dplyr::any_of(variable_classes$sum),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          numerator_estimate_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(numerator) %>%
            stringr::str_remove_all("_count_estimate") %>%
            stringr::str_split(", ") %>%
            unlist()

          numerator_moe_variables = numerator_estimate_variables %>%
            paste0("_M")

          # Calculate pooled standard error for sum, then convert to MOE
          se = se_sum(
            purrr::map(numerator_moe_variables, ~ .df %>% dplyr::pull(.x)),
            purrr::map(numerator_estimate_variables, ~ .df %>% dplyr::pull(.x)))

          moe = se * 1.645

          return(moe) },
        .names = "{.col}_M"))

  df_cvs1 = df_cvs1 %>%
    dplyr::mutate(
      ## count variables
      ## raw ACS variables
      dplyr::across(
        .cols = dplyr::any_of(cv_variables),
        .fns = function(x) {

          current_column = dplyr::cur_column()

          ## this effectively returns error calculations for the underlying variable
          ## under the assumption that the `1 - ` operation has no effect on the calculation
          ## of error
          if (current_column %in% variable_classes[["one minus percentage"]]) {
            current_column = codebook1 %>%
              dplyr::filter(calculated_variable == current_column) %>%
              dplyr::pull(definition) %>%
              stringr::str_remove_all("One minus |\\.") }

          ## variable metadata from the codebook
          codebook_variable = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::mutate(
              dplyr::across(
                .cols = c(numerator, denominator),
                .fns = ~ .x %>%
                  stringr::str_remove_all("_count_estimate") %>%
                  stringr::str_split(", ")))
          numerator_estimate_variables = codebook_variable %>%
            dplyr::pull(numerator) %>% unlist() %>% 
            stringr::str_replace_all("percent$", "pct")
          numerator_moe_variables = numerator_estimate_variables %>%
            stringr::str_c("_M")
          denominator_estimate_variables = codebook_variable %>%
            dplyr::pull(denominator) %>% unlist() %>% 
            stringr::str_replace_all("percent$", "pct")
          denominator_moe_variables = denominator_estimate_variables %>%
            stringr::str_c("_M")

          ## for variables where we already have an MOE, this is simple:
          ## we have already calculated MOEs for derived sum variables as well
          if (current_column %in% c(c(variable_classes$raw, variable_classes$sum) %>% stringr::str_remove("_count_estimate"))) {
            SE = se_simple(get(dplyr::cur_column() %>% paste0("_M"))) }

          ## for simple percent variables with one numerator, one denominator:
          if (current_column %in% variable_classes[["simple percent, no calculated variables"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = get(numerator_estimate_variables),
              estimate_denominator = get(denominator_estimate_variables),
              moe_numerator = get(numerator_moe_variables),
              moe_denominator = get(denominator_moe_variables)) }

          ## for percents with summed/subtracted numerators, one denominator
          if (current_column %in% variable_classes[["numerator sum percent"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = rowSums(dplyr::select(., dplyr::all_of(numerator_estimate_variables))),
              estimate_denominator = get(denominator_estimate_variables),
              se_numerator = se_sum(
                purrr::map(numerator_moe_variables, ~ df_cvs1 %>% dplyr::pull(.x)),
                purrr::map(numerator_estimate_variables, ~ df_cvs1 %>% dplyr::pull(.x))),
              se_denominator = se_simple(
                purrr::map(denominator_moe_variables, ~ df_cvs1 %>% dplyr::pull(.x)) %>% unlist())) }

          ## for percents with one numerator, summed/subtracted denominators
          if (current_column %in% variable_classes[["denominator sum percent"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = get(numerator_estimate_variables),
              estimate_denominator = rowSums(dplyr::select(., dplyr::all_of(denominator_estimate_variables))),
              se_numerator = se_simple(
                purrr::map(numerator_moe_variables, ~ df_cvs1 %>% dplyr::pull(.x)) %>% unlist()),
              se_denominator = se_sum(
                purrr::map(denominator_moe_variables, ~ df_cvs1 %>% dplyr::pull(.x)),
                purrr::map(denominator_estimate_variables, ~ df_cvs1 %>% dplyr::pull(.x)))) }

          ## for percents with summed numerators and summed denominators
          if (current_column %in% variable_classes[["numerator and denominator sum percent"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = rowSums(dplyr::select(., dplyr::all_of(numerator_estimate_variables))),
              estimate_denominator = rowSums(dplyr::select(., dplyr::all_of(denominator_estimate_variables))),
              se_numerator = se_sum(
                purrr::map(numerator_moe_variables, ~ df_cvs1 %>% dplyr::pull(.x)),
                purrr::map(numerator_estimate_variables, ~ df_cvs1 %>% dplyr::pull(.x))),
              se_denominator = se_sum(
                purrr::map(denominator_moe_variables, ~ df_cvs1 %>% dplyr::pull(.x)),
                purrr::map(denominator_estimate_variables, ~ df_cvs1 %>% dplyr::pull(.x)))) }

          return(SE) },
        .names = "{.col}_SE")) %>%
    dplyr::mutate(
      ## create coefficients of variation from standard errors
      dplyr::across(
        .cols = dplyr::matches("_SE$"),
        .fns = ~ cv(
          estimate = get(dplyr::cur_column() %>% stringr::str_remove("_SE$")),
          se = .x),
        .names = "{.col %>% stringr::str_remove('_SE')}_CV"))

  moe_variables = df_cvs1 %>%
    dplyr::select(dplyr::matches("_M$")) %>%
    colnames() %>%
    stringr::str_remove("_M$")
  se_variables = df_cvs1 %>%
    dplyr::select(dplyr::matches("_SE$")) %>%
    colnames() %>%
    stringr::str_remove("_SE$")

  # Variables with SE but not MOE occur when we calculate SEs directly
  # (e.g., for complex percentages) but didn't create corresponding MOEs
  df_cvs = df_cvs1 %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(se_variables[!se_variables %in% moe_variables] %>% stringr::str_c("_SE")),
        .fns = ~ .x * 1.645,
        .names = "{.col %>% stringr::str_remove('_SE$')}_M"),
      ## reduce number of digits
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        .fns = ~ round(.x, digits = 4)))

  ####----quick tests for making updates to the script----####

  # moe_variables = df_cvs %>% dplyr::select(matches("_M$")) %>% colnames() %>% stringr::str_remove("_M$")
  # se_variables = df_cvs %>% dplyr::select(matches("_SE$")) %>% colnames() %>% stringr::str_remove("_SE$")

  ## should be none
  # se_variables[!se_variables %in% moe_variables]
  # moe_variables[!moe_variables %in% se_variables]

  # df_cvs %>%
  #   dplyr::select(dplyr::matches("_SE|_M|_CV")) %>%
  #   tidyr::pivot_longer(dplyr::everything()) %>%
  #   dplyr::summarize(
  #     mean = mean(value),
  #     min = min(value),
  #     median = median(value),
  #     max = max(value))

  ####----####
  return(df_cvs)
}

utils::globalVariables(c(
  "calculated_variable", "numerator_variable_count", "denominator_variable_count",
  "numerator", "denominator", "observation", "type", "estimate", "moe", "variable_class"))
