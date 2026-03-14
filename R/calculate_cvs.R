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
        dplyr::slice_max(order_by = moe, by = "observation", n = 1, with_ties = FALSE)) %>%
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

  se = dplyr::if_else(estimate_denominator == 0, NA_real_, se)

  return(se)
}

#' @title Calculate standard error for a product of two estimates
#' @details Calculate the standard error for an estimate derived by multiplying
#'   two estimates together. For example, multiplying a proportion by a population
#'   count to get a subgroup count. Formula from Census Bureau ACS Accuracy
#'   documentation: SE(X*Y) = sqrt((X*SE(Y))^2 + (Y*SE(X))^2).
#' @param estimate_x The first estimate (X)
#' @param estimate_y The second estimate (Y)
#' @param se_x The standard error of estimate X (or NULL if providing MOE)
#' @param se_y The standard error of estimate Y (or NULL if providing MOE)
#' @param moe_x The margin of error of estimate X (or NULL if providing SE)
#' @param moe_y The margin of error of estimate Y (or NULL if providing SE)
#' @returns The standard error of the product X*Y
#' @keywords internal
se_product = function(
    estimate_x,
    estimate_y,
    se_x = NULL,
    se_y = NULL,
    moe_x = NULL,
    moe_y = NULL) {

  if (is.null(se_x) && is.null(moe_x)) {
    stop("Either se_x or moe_x must be provided for estimate X.")
  }
  if (is.null(se_y) && is.null(moe_y)) {
    stop("Either se_y or moe_y must be provided for estimate Y.")
  }

  if (!is.null(moe_x) && is.null(se_x)) {
    se_x = se_simple(moe_x)
  }
  if (!is.null(moe_y) && is.null(se_y)) {
    se_y = se_simple(moe_y)
  }

  se = sqrt(
    ((estimate_x)^2 * (se_y)^2) +
    ((estimate_y)^2 * (se_x)^2))

  return(se)
}

#' @title Calculate standard error for population-weighted mean
#' @details Calculate the standard error for a population-weighted average,
#'   used when aggregating median or average variables across geographies.
#'   Uses a multi-step approach following Census Bureau guidance:
#'   1. Calculate SE for each product (value * weight) using se_product()
#'   2. Calculate SE for the sum of products (numerator) using se_sum()
#'   3. Calculate SE for the sum of weights (denominator) using se_sum()
#'   4. Calculate SE for the ratio using se_proportion_ratio(type = "ratio")
#' @param values A numeric vector of values being averaged (e.g., median incomes)
#' @param weights A numeric vector of population weights
#' @param se_values Standard errors for the values (or NULL if providing moe_values)
#' @param se_weights Standard errors for the weights (or NULL if providing moe_weights)
#' @param moe_values Margins of error for the values (or NULL if providing se_values)
#' @param moe_weights Margins of error for the weights (or NULL if providing se_weights)
#' @returns The standard error of the weighted mean
#' @keywords internal
se_weighted_mean = function(
    values,
    weights,
    se_values = NULL,
    se_weights = NULL,
    moe_values = NULL,
    moe_weights = NULL) {

  if (is.null(se_values) && is.null(moe_values)) {
    stop("Either se_values or moe_values must be provided.")
  }
  if (is.null(se_weights) && is.null(moe_weights)) {
    stop("Either se_weights or moe_weights must be provided.")
  }

  if (!is.null(moe_values) && is.null(se_values)) {
    se_values = se_simple(moe_values)
  }
  if (!is.null(moe_weights) && is.null(se_weights)) {
    se_weights = se_simple(moe_weights)
  }

  ## remove observations where any component is NA
  valid_idx = !is.na(values) & !is.na(weights) & !is.na(se_values) & !is.na(se_weights)
  values = values[valid_idx]
  weights = weights[valid_idx]
  se_values = se_values[valid_idx]
  se_weights = se_weights[valid_idx]

  if (length(values) == 0) {
    return(NA_real_)
  }

  ## Step 1: SE for each product (value_i * weight_i)
  product_estimates = values * weights
  product_ses = se_product(
    estimate_x = values,
    estimate_y = weights,
    se_x = se_values,
    se_y = se_weights)

  ## Step 2: SE for the sum of products (numerator)
  numerator_estimate = sum(product_estimates)
  numerator_se = se_sum(
    as.list(product_ses * 1.645),
    as.list(product_estimates))

  ## Step 3: SE for the sum of weights (denominator)
  denominator_estimate = sum(weights)
  denominator_se = se_sum(
    as.list(se_weights * 1.645),
    as.list(weights))

  ## Step 4: SE for the ratio (numerator / denominator)
  se = se_proportion_ratio(
    estimate_numerator = numerator_estimate,
    estimate_denominator = denominator_estimate,
    se_numerator = numerator_se,
    se_denominator = denominator_se,
    type = "ratio")

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

#' @title Calculate margins of error for derived variables
#' @details Calculates margins of error for all derived ACS estimates. Standard
#'   errors are computed internally as an intermediate step but are not included
#'   in the returned dataframe. Uses pre-parsed codebook columns
#'   (numerator_vars, denominator_vars, se_calculation_type) to determine how
#'   to calculate standard errors.
#' @param .df The dataset returned from \code{compile_acs_data()}.
#'  The argument to this parameter must have an attribute named `codebook` (as is
#'  true of results from \code{compile_acs_data())}.
#' @returns A modified dataframe that includes margins of error (suffixed
#'   \code{_M}) for derived variables.
#' @keywords internal
calculate_moes = function(.df) {
  warning("Margins of error for derived variables are experimental features
          and should be used with caution. Such measures are suffixed with `_M`.")

  ## the codebook attached to the default compile_acs_data() return
  codebook = .df %>% attr("codebook")

  ## source: https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes/2024-02.html
  ## these are the variables (at least for 2023) that at times have controlled
  ## estimates. for these variables, if the MOE in the raw data is missing, we
  ## set the MOE equal to 0, as controlled estimates have no sampling error.
  controlled_variables = c(
    "total_population_universe", "sex_by_age_universe", "race_universe",
    "race_hispanic_allraces", "race_nonhispanic_allraces") %>% stringr::str_c("_M")

  .df = .df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(controlled_variables),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x)))

  ## Use pre-parsed codebook columns
  codebook1 = codebook %>%
    dplyr::distinct(calculated_variable, .keep_all = TRUE) %>%
    dplyr::filter(!stringr::str_detect(calculated_variable, "_M$"))

  ## all variables for which to calculate CVs
  cv_variables = codebook1 %>%
    dplyr::filter(!se_calculation_type %in% c("metadata", "unknown", "weighted_average")) %>%
    dplyr::pull(calculated_variable)

  ## a named list of variables, grouped by SE calculation type
  se_type_groups = codebook1 %>%
    dplyr::filter(calculated_variable %in% cv_variables) %>%
    dplyr::group_split(se_calculation_type)
  se_type_names = se_type_groups %>%
    purrr::map_chr(~ .x %>% dplyr::pull(se_calculation_type) %>% unique())
  se_types = se_type_groups %>%
    purrr::map(~ .x %>% dplyr::pull(calculated_variable)) %>%
    stats::setNames(se_type_names)

  ## Step 1: calculate MOEs for derived sum variables
  df_with_sum_moes = .df %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(se_types$sum),
        .fns = function(x) {
          current_column = dplyr::cur_column()

          ## Get pre-parsed numerator variables from codebook
          numerator_estimate_variables = codebook1 %>%
            dplyr::filter(calculated_variable == current_column) %>%
            dplyr::pull(numerator_vars) %>%
            unlist()

          numerator_moe_variables = numerator_estimate_variables %>%
            paste0("_M")

          se = se_sum(
            purrr::map(numerator_moe_variables, ~ .df %>% dplyr::pull(.x)),
            purrr::map(numerator_estimate_variables, ~ .df %>% dplyr::pull(.x)))

          moe = se * 1.645

          return(moe)},
        .names = "{.col}_M"))

  ## Step 2: calculate SEs for all variables (using df_with_sum_moes which has derived MOEs)
  df_with_ses = df_with_sum_moes %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(cv_variables),
        .fns = function(x) {

          current_column = dplyr::cur_column()
          original_column = current_column

          ## for "one minus" variables, use the underlying variable for error calculation
          if (current_column %in% se_types[["one_minus"]]) {
            current_column = codebook1 %>%
              dplyr::filter(calculated_variable == current_column) %>%
              dplyr::pull(numerator_vars) %>%
              unlist()}

          ## Get pre-parsed variables from codebook
          codebook_row = codebook1 %>%
            dplyr::filter(calculated_variable == current_column)

          numerator_estimate_variables = codebook_row %>%
            dplyr::pull(numerator_vars) %>%
            unlist()
          numerator_moe_variables = numerator_estimate_variables %>%
            stringr::str_c("_M")
          denominator_estimate_variables = codebook_row %>%
            dplyr::pull(denominator_vars) %>%
            unlist()
          denominator_moe_variables = denominator_estimate_variables %>%
            stringr::str_c("_M")

          ## Get pre-parsed subtract variables from codebook
          ## SE of a difference uses the same formula as SE of a sum:
          ## SE(A - B) = sqrt(SE_A^2 + SE_B^2)
          numerator_subtract_estimate_variables = codebook_row %>%
            dplyr::pull(numerator_subtract_vars) %>%
            unlist()
          numerator_subtract_moe_variables = numerator_subtract_estimate_variables %>%
            stringr::str_c("_M")
          denominator_subtract_estimate_variables = codebook_row %>%
            dplyr::pull(denominator_subtract_vars) %>%
            unlist()
          denominator_subtract_moe_variables = denominator_subtract_estimate_variables %>%
            stringr::str_c("_M")

          ## combine additive and subtractive variables for SE calculations
          all_numerator_estimate_variables = c(numerator_estimate_variables, numerator_subtract_estimate_variables)
          all_numerator_moe_variables = c(numerator_moe_variables, numerator_subtract_moe_variables)
          all_denominator_estimate_variables = c(denominator_estimate_variables, denominator_subtract_estimate_variables)
          all_denominator_moe_variables = c(denominator_moe_variables, denominator_subtract_moe_variables)

          ## for variables where we already have an MOE, this is simple
          if (current_column %in% c(se_types$raw, se_types$sum)) {
            SE = se_simple(get(current_column %>% paste0("_M")))

          ## for simple percent variables with one numerator, one denominator
          } else if (current_column %in% se_types[["simple_percent"]]) {
            SE = se_proportion_ratio(
              estimate_numerator = get(numerator_estimate_variables),
              estimate_denominator = get(denominator_estimate_variables),
              moe_numerator = get(numerator_moe_variables),
              moe_denominator = get(denominator_moe_variables))

          ## for percents with summed/subtracted numerators, one denominator
          } else if (current_column %in% se_types[["complex_numerator"]]) {
            numerator_estimate = rowSums(dplyr::select(., dplyr::all_of(numerator_estimate_variables)))
            if (length(numerator_subtract_estimate_variables) > 0) {
              numerator_estimate = numerator_estimate -
                rowSums(dplyr::select(., dplyr::all_of(numerator_subtract_estimate_variables)))
            }
            SE = se_proportion_ratio(
              estimate_numerator = numerator_estimate,
              estimate_denominator = get(denominator_estimate_variables),
              se_numerator = se_sum(
                purrr::map(all_numerator_moe_variables, ~ df_with_sum_moes %>% dplyr::pull(.x)),
                purrr::map(all_numerator_estimate_variables, ~ df_with_sum_moes %>% dplyr::pull(.x))),
              se_denominator = se_simple(
                purrr::map(denominator_moe_variables, ~ df_with_sum_moes %>% dplyr::pull(.x)) %>% unlist()))

          ## for percents with one numerator, summed/subtracted denominators
          } else if (current_column %in% se_types[["complex_denominator"]]) {
            denominator_estimate = rowSums(dplyr::select(., dplyr::all_of(denominator_estimate_variables)))
            if (length(denominator_subtract_estimate_variables) > 0) {
              denominator_estimate = denominator_estimate -
                rowSums(dplyr::select(., dplyr::all_of(denominator_subtract_estimate_variables)))
            }
            SE = se_proportion_ratio(
              estimate_numerator = get(numerator_estimate_variables),
              estimate_denominator = denominator_estimate,
              se_numerator = se_simple(
                purrr::map(numerator_moe_variables, ~ df_with_sum_moes %>% dplyr::pull(.x)) %>% unlist()),
              se_denominator = se_sum(
                purrr::map(all_denominator_moe_variables, ~ df_with_sum_moes %>% dplyr::pull(.x)),
                purrr::map(all_denominator_estimate_variables, ~ df_with_sum_moes %>% dplyr::pull(.x))))

          ## for percents with summed numerators and summed denominators
          } else if (current_column %in% se_types[["complex_both"]]) {
            numerator_estimate = rowSums(dplyr::select(., dplyr::all_of(numerator_estimate_variables)))
            if (length(numerator_subtract_estimate_variables) > 0) {
              numerator_estimate = numerator_estimate -
                rowSums(dplyr::select(., dplyr::all_of(numerator_subtract_estimate_variables)))
            }
            denominator_estimate = rowSums(dplyr::select(., dplyr::all_of(denominator_estimate_variables)))
            if (length(denominator_subtract_estimate_variables) > 0) {
              denominator_estimate = denominator_estimate -
                rowSums(dplyr::select(., dplyr::all_of(denominator_subtract_estimate_variables)))
            }
            SE = se_proportion_ratio(
              estimate_numerator = numerator_estimate,
              estimate_denominator = denominator_estimate,
              se_numerator = se_sum(
                purrr::map(all_numerator_moe_variables, ~ df_with_sum_moes %>% dplyr::pull(.x)),
                purrr::map(all_numerator_estimate_variables, ~ df_with_sum_moes %>% dplyr::pull(.x))),
              se_denominator = se_sum(
                purrr::map(all_denominator_moe_variables, ~ df_with_sum_moes %>% dplyr::pull(.x)),
                purrr::map(all_denominator_estimate_variables, ~ df_with_sum_moes %>% dplyr::pull(.x))))

          } else {
            stop(paste0("Unhandled SE calculation type for variable: ", original_column))
          }

          return(SE)},
        .names = "{.col}_SE"))

  moe_variables = df_with_ses %>%
    dplyr::select(dplyr::matches("_M$")) %>%
    colnames() %>%
    stringr::str_remove("_M$")
  se_variables = df_with_ses %>%
    dplyr::select(dplyr::matches("_SE$")) %>%
    colnames() %>%
    stringr::str_remove("_SE$")

  ## Convert SEs to MOEs for variables that don't already have one
  df_moes = df_with_ses %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(se_variables[!se_variables %in% moe_variables] %>% stringr::str_c("_SE")),
        .fns = ~ .x * 1.645,
        .names = "{.col %>% stringr::str_remove('_SE$')}_M"),
      ## reduce number of digits
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        .fns = ~ round(.x, digits = 4))) %>%
    ## drop intermediate SE columns
    dplyr::select(-dplyr::matches("_SE$"))

  return(df_moes)
}

utils::globalVariables(c(
  "calculated_variable", "observation", "type", "estimate", "moe",
  "se_calculation_type", "numerator_vars", "numerator_subtract_vars",
  "denominator_vars", "denominator_subtract_vars"))
