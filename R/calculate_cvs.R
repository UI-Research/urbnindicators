#' @importFrom magrittr %>%

#' @title Calculate a simple standard error
#' @details Create a standard error at the 90% level from a 90% margin of error
#' @param moe A margin of error, or a vector thereof
#' @returns A 90% standard error
#' @keywords internal
se_simple = function(moe) {
  moe / 1.645
}

#' @title Calculate a pooled standard error for a summed or subtracted estimate
#' @details For an estimate derived by adding or subtracting multiple estimates,
#'    calculate the pooled standard error. Inputs can be supplied as 90% margins
#'    of error (\code{from = "moe"}, the default) or as standard errors
#'    (\code{from = "se"}).
#' @param errors A list of vectors. Either MOEs or SEs for each component
#'    (controlled by \code{from}).
#' @param estimates A list of vectors. The corresponding estimates for each
#'    component. Used for the Census Bureau zero-estimate rule.
#' @param from Either \code{"moe"} (default) or \code{"se"}. Tells \code{se_sum}
#'    whether \code{errors} contains MOEs or SEs.
#' @returns A pooled 90% standard error
#' @keywords internal
se_sum = function(errors, estimates, from = "moe") {
  if (!from %in% c("moe", "se")) {
    cli::cli_abort('{.arg from} must be {.val moe} or {.val se}.')
  }

  err_mat = do.call(cbind, errors)
  est_mat = do.call(cbind, estimates)

  se_mat = if (from == "moe") err_mat / 1.645 else err_mat

  ## Census zero-estimate rule: among components with estimate == 0 in a row,
  ## keep only the one with the largest SE; zero out the rest.
  is_zero = (est_mat == 0)
  if (any(is_zero, na.rm = TRUE)) {
    n_zeros_per_row = rowSums(is_zero, na.rm = TRUE)
    rows_apply = n_zeros_per_row > 1
    if (any(rows_apply, na.rm = TRUE)) {
      ## Mask: only zero-estimate cells are candidates for the argmax.
      masked = se_mat
      masked[!is_zero] = -Inf
      masked[is.na(masked)] = -Inf
      argmax_col = max.col(masked, ties.method = "first")

      n_rows = nrow(se_mat)
      n_cols = ncol(se_mat)
      argmax_mat = matrix(FALSE, n_rows, n_cols)
      argmax_mat[cbind(seq_len(n_rows), argmax_col)] = TRUE
      rows_mask = matrix(rows_apply, n_rows, n_cols)

      zero_out = is_zero & rows_mask & !argmax_mat
      zero_out[is.na(zero_out)] = FALSE
      se_mat[zero_out] = 0
    }
  }

  sqrt(rowSums(se_mat^2))
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
#' @keywords internal
se_proportion_ratio = function(
    estimate_numerator,
    estimate_denominator,
    moe_numerator = NULL,
    moe_denominator = NULL,
    se_numerator = NULL,
    se_denominator = NULL,
    type = "proportion") {

  if (all(is.null(moe_numerator), is.null(se_numerator)) | all(is.null(moe_denominator), is.null(se_denominator))) {
    cli::cli_abort("A margin of error or standard error must be provided for both the numerator and the denominator.")
  }

  if( ((!is.null(moe_numerator) & !is.null(se_numerator))) | (!is.null(moe_denominator) & !is.null(se_denominator))) {
    cli::cli_abort("Only one of a margin of error or a standard error can be provided for the numerator and denominator.")
  }

  if (!is.null(moe_numerator)) { se_numerator = se_simple(moe_numerator) }
  if (!is.null(moe_denominator)) { se_denominator = se_simple(moe_denominator) }

  ## Avoid producing Inf/NaN intermediates when the denominator is 0; mask such
  ## rows to NA up front so the math runs only on well-defined inputs.
  denom_safe = dplyr::if_else(estimate_denominator == 0, NA_real_, estimate_denominator)

  ## squared standard error of the numerator
  radical_term_one = se_numerator^2

  ## squared numerator over the squared denominator times the squared standard error of the denominator
  radical_term_two = (estimate_numerator^2 / denom_safe^2) * se_denominator^2

  ## If the value under the radical is negative, or the numerator exceeds the
  ## denominator (p > 1, so the "proportion" is degenerate), use the formula
  ## for ratio standard errors -- i.e., add the radical terms, rather than
  ## subtract them. Census Bureau guidance: the proportion SE formula should
  ## not be applied when p > 1.
  se = dplyr::if_else(
    radical_term_one < radical_term_two | type == "ratio" |
      abs(estimate_numerator) > abs(estimate_denominator),
    (1 / denom_safe) * sqrt(radical_term_one + radical_term_two),
    (1 / denom_safe) * sqrt(radical_term_one - radical_term_two))

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
    cli::cli_abort("Either {.arg se_x} or {.arg moe_x} must be provided for estimate X.")
  }
  if (is.null(se_y) && is.null(moe_y)) {
    cli::cli_abort("Either {.arg se_y} or {.arg moe_y} must be provided for estimate Y.")
  }

  if (!is.null(moe_x) && is.null(se_x)) {
    se_x = se_simple(moe_x)
  }
  if (!is.null(moe_y) && is.null(se_y)) {
    se_y = se_simple(moe_y)
  }

  se = sqrt(
    (estimate_x^2 * se_y^2) +
    (estimate_y^2 * se_x^2))

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
    cli::cli_abort("Either {.arg se_values} or {.arg moe_values} must be provided.")
  }
  if (is.null(se_weights) && is.null(moe_weights)) {
    cli::cli_abort("Either {.arg se_weights} or {.arg moe_weights} must be provided.")
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
    as.list(product_ses),
    as.list(product_estimates),
    from = "se")

  ## Step 3: SE for the sum of weights (denominator)
  denominator_estimate = sum(weights)
  denominator_se = se_sum(
    as.list(se_weights),
    as.list(weights),
    from = "se")

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
#' @details Return a coefficient of variation reflecting the ration of the SE to the estimate
#' @param estimate The estimate
#' @param se The standard error (SE)
#' @returns A coefficient of variation
#' @keywords internal
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

  ## Helper: pull a row from codebook1 for a given calculated variable.
  codebook_row_for = function(var_name) {
    codebook1 %>% dplyr::filter(calculated_variable == var_name)
  }

  ## Helper: extract a list-column value as a character vector.
  unlist_listcol = function(row, col_name) {
    if (nrow(row) == 0) return(character(0))
    val = row[[col_name]]
    if (is.list(val)) val = val[[1]]
    if (is.null(val)) return(character(0))
    as.character(val)
  }

  ## Step 1: calculate MOEs for derived sum variables. Uses purrr::reduce to
  ## mutate one column at a time so each iteration has direct access to the
  ## current data frame (no get() / no bare-dot magic).
  df_with_sum_moes = .df %>% sf::st_drop_geometry()

  df_with_sum_moes = purrr::reduce(se_types$sum %||% character(0), function(df, current_column) {
    row = codebook_row_for(current_column)
    numerator_estimate_variables = unlist_listcol(row, "numerator_vars")
    numerator_moe_variables = paste0(numerator_estimate_variables, "_M")

    if (!all(c(numerator_estimate_variables, numerator_moe_variables) %in% colnames(df))) {
      return(df)
    }

    se = se_sum(
      purrr::map(numerator_moe_variables, ~ df[[.x]]),
      purrr::map(numerator_estimate_variables, ~ df[[.x]]))

    df[[paste0(current_column, "_M")]] = se * 1.645
    df
  }, .init = df_with_sum_moes)

  ## Step 2: calculate SEs for each variable in cv_variables. Iterate
  ## explicitly so each column lookup happens in normal R scope.
  df_with_ses = purrr::reduce(cv_variables, function(df, original_column) {
    current_column = original_column

    ## for "one minus" variables, use the underlying variable for error calculation
    if (current_column %in% (se_types[["one_minus"]] %||% character(0))) {
      current_column = unlist_listcol(codebook_row_for(current_column), "numerator_vars")
    }

    row = codebook_row_for(current_column)
    numerator_estimate_variables = unlist_listcol(row, "numerator_vars")
    numerator_moe_variables = paste0(numerator_estimate_variables, "_M")
    denominator_estimate_variables = unlist_listcol(row, "denominator_vars")
    denominator_moe_variables = paste0(denominator_estimate_variables, "_M")

    numerator_subtract_estimate_variables = unlist_listcol(row, "numerator_subtract_vars")
    numerator_subtract_moe_variables = paste0(numerator_subtract_estimate_variables, "_M")
    denominator_subtract_estimate_variables = unlist_listcol(row, "denominator_subtract_vars")
    denominator_subtract_moe_variables = paste0(denominator_subtract_estimate_variables, "_M")

    all_numerator_estimate_variables = c(numerator_estimate_variables, numerator_subtract_estimate_variables)
    all_numerator_moe_variables = c(numerator_moe_variables, numerator_subtract_moe_variables)
    all_denominator_estimate_variables = c(denominator_estimate_variables, denominator_subtract_estimate_variables)
    all_denominator_moe_variables = c(denominator_moe_variables, denominator_subtract_moe_variables)

    se = NULL

    if (current_column %in% c(se_types$raw %||% character(0), se_types$sum %||% character(0))) {
      moe_col = paste0(current_column, "_M")
      if (moe_col %in% colnames(df)) {
        se = se_simple(df[[moe_col]])
      }
    } else if (current_column %in% (se_types[["simple_percent"]] %||% character(0))) {
      se = se_proportion_ratio(
        estimate_numerator = df[[numerator_estimate_variables]],
        estimate_denominator = df[[denominator_estimate_variables]],
        moe_numerator = df[[numerator_moe_variables]],
        moe_denominator = df[[denominator_moe_variables]])
    } else if (current_column %in% (se_types[["complex_numerator"]] %||% character(0))) {
      numerator_estimate = rowSums(df[, numerator_estimate_variables, drop = FALSE])
      if (length(numerator_subtract_estimate_variables) > 0) {
        numerator_estimate = numerator_estimate -
          rowSums(df[, numerator_subtract_estimate_variables, drop = FALSE])
      }
      se = se_proportion_ratio(
        estimate_numerator = numerator_estimate,
        estimate_denominator = df[[denominator_estimate_variables]],
        se_numerator = se_sum(
          purrr::map(all_numerator_moe_variables, ~ df[[.x]]),
          purrr::map(all_numerator_estimate_variables, ~ df[[.x]])),
        se_denominator = se_simple(df[[denominator_moe_variables]]))
    } else if (current_column %in% (se_types[["complex_denominator"]] %||% character(0))) {
      denominator_estimate = rowSums(df[, denominator_estimate_variables, drop = FALSE])
      if (length(denominator_subtract_estimate_variables) > 0) {
        denominator_estimate = denominator_estimate -
          rowSums(df[, denominator_subtract_estimate_variables, drop = FALSE])
      }
      se = se_proportion_ratio(
        estimate_numerator = df[[numerator_estimate_variables]],
        estimate_denominator = denominator_estimate,
        se_numerator = se_simple(df[[numerator_moe_variables]]),
        se_denominator = se_sum(
          purrr::map(all_denominator_moe_variables, ~ df[[.x]]),
          purrr::map(all_denominator_estimate_variables, ~ df[[.x]])))
    } else if (current_column %in% (se_types[["complex_both"]] %||% character(0))) {
      numerator_estimate = rowSums(df[, numerator_estimate_variables, drop = FALSE])
      if (length(numerator_subtract_estimate_variables) > 0) {
        numerator_estimate = numerator_estimate -
          rowSums(df[, numerator_subtract_estimate_variables, drop = FALSE])
      }
      denominator_estimate = rowSums(df[, denominator_estimate_variables, drop = FALSE])
      if (length(denominator_subtract_estimate_variables) > 0) {
        denominator_estimate = denominator_estimate -
          rowSums(df[, denominator_subtract_estimate_variables, drop = FALSE])
      }
      se = se_proportion_ratio(
        estimate_numerator = numerator_estimate,
        estimate_denominator = denominator_estimate,
        se_numerator = se_sum(
          purrr::map(all_numerator_moe_variables, ~ df[[.x]]),
          purrr::map(all_numerator_estimate_variables, ~ df[[.x]])),
        se_denominator = se_sum(
          purrr::map(all_denominator_moe_variables, ~ df[[.x]]),
          purrr::map(all_denominator_estimate_variables, ~ df[[.x]])))
    } else {
      cli::cli_abort("Unhandled SE calculation type for variable: {.val {original_column}}")
    }

    if (!is.null(se)) {
      df[[paste0(original_column, "_SE")]] = se
    }
    df
  }, .init = df_with_sum_moes)

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

`%||%` = function(a, b) if (is.null(a)) b else a

utils::globalVariables(c(
  "calculated_variable",
  "se_calculation_type", "numerator_vars", "numerator_subtract_vars",
  "denominator_vars", "denominator_subtract_vars"))
