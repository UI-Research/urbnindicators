#' @importFrom magrittr %>%
#' @importFrom rlang .data

## Internal workhorse for aggregating data to target geographies.
## interpolate_acs() calls this function after preparing data.
## For fractional allocation mode, extensive variables are pre-multiplied
## by crosswalk weights before this function is called.
##
## @param data_no_geom Data frame without geometry. For interpolation, count
##   estimates, count MOEs, area variables, and the weight_variable (+ its MOE)
##   have already been multiplied by the crosswalk allocation weight.
## @param target_col Character. Column name to group by (the target geography ID).
## @param weight_variable Character. Column name used for population-weighted
##   averages of intensive variables.
## @param codebook Data frame. The codebook attribute from compile_acs_data() output.
## @param resolved_tables Character vector. Table names for re-running definitions.
## @returns Data frame aggregated to target geographies with estimates and MOEs.
## @keywords internal
.aggregate_to_target = function(
    data_no_geom,
    target_col,
    weight_variable,
    codebook,
    resolved_tables) {

  codebook = ensure_aggregation_strategy(codebook)

  ####----Classify Variables (using pre-parsed codebook columns)----####
  sum_variables = codebook %>%
    dplyr::filter(aggregation_strategy == "sum") %>%
    dplyr::pull(calculated_variable)

  percent_variables = codebook %>%
    dplyr::filter(aggregation_strategy == "recalculate_percent") %>%
    dplyr::pull(calculated_variable)

  weighted_avg_variables = codebook %>%
    dplyr::filter(aggregation_strategy == "weighted_average") %>%
    dplyr::pull(calculated_variable)

  ## Filter to variables that actually exist in the data
  sum_variables = sum_variables[sum_variables %in% colnames(data_no_geom)]
  percent_variables = percent_variables[percent_variables %in% colnames(data_no_geom)]
  weighted_avg_variables = weighted_avg_variables[weighted_avg_variables %in% colnames(data_no_geom)]

  ####----Warn about NAs in columns being aggregated----####
  ## Aggregations propagate NA (na.rm = FALSE); if any input column has NA,
  ## the corresponding target geography will be NA in the result. Surface this
  ## once per call so users can filter or impute upstream if they want different
  ## behavior.
  aggregated_cols = c(
    sum_variables,
    weighted_avg_variables,
    paste0(sum_variables, "_M"),
    paste0(weighted_avg_variables, "_M"))
  aggregated_cols = aggregated_cols[aggregated_cols %in% colnames(data_no_geom)]
  na_cols = aggregated_cols[purrr::map_lgl(
    data_no_geom[aggregated_cols], ~ any(is.na(.x)))]
  if (length(na_cols) > 0) {
    preview = paste0("`", utils::head(na_cols, 5), "`", collapse = ", ")
    more = if (length(na_cols) > 5) paste0(", and ", length(na_cols) - 5, " more") else ""
    cli::cli_warn(c(
      "Input has NA values in {length(na_cols)} column{?s} being aggregated: {preview}{more}.",
      "i" = "These NAs will propagate to the corresponding target geographies.",
      "i" = "Filter or impute upstream if a different behavior is desired."))
  }

  ####----Aggregate Sum Variables----####
  aggregated_sums = data_no_geom %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(target_col, "data_source_year")))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(sum_variables), ~ sum(.x)),
      .groups = "drop")

  ## Calculate MOEs for summed variables using se_sum()
  sum_vars_with_moe = sum_variables[paste0(sum_variables, "_M") %in% colnames(data_no_geom)]

  aggregated_sum_moes = purrr::reduce(sum_vars_with_moe, function(acc, var) {
    moe_var = paste0(var, "_M")

    var_moes = data_no_geom %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(target_col, "data_source_year")))) %>%
      dplyr::group_split() %>%
      purrr::map(function(group_df) {
        group_keys = group_df %>%
          dplyr::distinct(dplyr::across(dplyr::all_of(c(target_col, "data_source_year"))))

        se = se_sum(as.list(group_df[[moe_var]]), as.list(group_df[[var]]))

        group_keys %>%
          dplyr::mutate(!!moe_var := se * 1.645)
      }) %>% purrr::list_rbind()

    acc %>%
      dplyr::left_join(var_moes, by = c(target_col, "data_source_year"))
  }, .init = data_no_geom %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(target_col, "data_source_year")))))

  ####----Aggregate Weighted Average Variables----####
  if (length(weighted_avg_variables) > 0) {
    weight_moe_variable = paste0(weight_variable, "_M")
    has_weight_moe = weight_moe_variable %in% colnames(data_no_geom)

    if (!has_weight_moe) {
      cli::cli_warn("MOE column {.var {weight_moe_variable}} not found for weight variable. SE calculations for weighted averages will be skipped.")
    }

    aggregated_weighted = data_no_geom %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(target_col, "data_source_year")))) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(weighted_avg_variables),
          ~ sum(.x * .data[[weight_variable]]) / sum(.data[[weight_variable]])),
        .groups = "drop")

    ## Calculate SEs for weighted averages
    if (has_weight_moe) {
      weighted_vars_with_moe = weighted_avg_variables[
        paste0(weighted_avg_variables, "_M") %in% colnames(data_no_geom)]

      aggregated_weighted_ses = purrr::reduce(weighted_vars_with_moe, function(acc, var) {
        moe_var = paste0(var, "_M")
        se_col_name = paste0(var, "_SE")

        var_ses = data_no_geom %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c(target_col, "data_source_year")))) %>%
          dplyr::group_split() %>%
          purrr::map(function(group_df) {
            group_keys = group_df %>%
              dplyr::distinct(dplyr::across(dplyr::all_of(c(target_col, "data_source_year"))))

            se_result = tryCatch({
              se_weighted_mean(
                values = group_df[[var]],
                weights = group_df[[weight_variable]],
                moe_values = group_df[[moe_var]],
                moe_weights = group_df[[weight_moe_variable]])
            }, error = function(e) NA_real_)

            group_keys %>%
              dplyr::mutate(!!se_col_name := se_result)
          }) %>% purrr::list_rbind()

        acc %>%
          dplyr::left_join(var_ses, by = c(target_col, "data_source_year"))
      }, .init = data_no_geom %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(c(target_col, "data_source_year")))))

      ## Convert SEs to MOEs
      se_col_names = paste0(weighted_avg_variables, "_SE")
      se_col_names = se_col_names[se_col_names %in% colnames(aggregated_weighted_ses)]
      moe_new_names = stringr::str_replace(se_col_names, "_SE$", "_M")

      aggregated_weighted_moes = aggregated_weighted_ses %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(se_col_names),
            ~ .x * 1.645,
            .names = "{stringr::str_replace(.col, '_SE$', '_M')}")) %>%
        dplyr::select(dplyr::all_of(c(target_col, "data_source_year", moe_new_names)))
    } else {
      aggregated_weighted_ses = NULL
      aggregated_weighted_moes = NULL
    }
  } else {
    aggregated_weighted = NULL
    aggregated_weighted_ses = NULL
    aggregated_weighted_moes = NULL
  }

  ####----Handle Metadata Variables (Area)----####
  area_variables = c("area_land_sq_kilometer", "area_water_sq_kilometer", "area_land_water_sq_kilometer")
  area_variables = area_variables[area_variables %in% colnames(data_no_geom)]

  if (length(area_variables) > 0) {
    aggregated_areas = data_no_geom %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(target_col, "data_source_year")))) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(area_variables), ~ sum(.x)),
        .groups = "drop")
  } else {
    aggregated_areas = data_no_geom %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(c(target_col, "data_source_year"))))
  }

  ####----Combine Results----####
  result = aggregated_sums %>%
    dplyr::left_join(aggregated_sum_moes, by = c(target_col, "data_source_year")) %>%
    dplyr::left_join(aggregated_areas, by = c(target_col, "data_source_year"))

  if (!is.null(aggregated_weighted)) {
    result = result %>%
      dplyr::left_join(aggregated_weighted, by = c(target_col, "data_source_year"))
    if (!is.null(aggregated_weighted_moes)) {
      result = result %>%
        dplyr::left_join(aggregated_weighted_moes, by = c(target_col, "data_source_year"))
    }
  }

  ####----Recalculate Population Density----####
  if ("total_population_universe" %in% colnames(result) && "area_land_sq_kilometer" %in% colnames(result)) {
    result = result %>%
      dplyr::mutate(
        population_density_land_sq_kilometer = safe_divide(total_population_universe, area_land_sq_kilometer))
  }

  ####----Recalculate Percent Variables via Registry Definitions----####
  ## Save MOE columns (execute_definitions doesn't use them, but regex patterns
  ## in resolve_regex_columns exclude _M$ already, so this is a safety measure)
  moe_cols = result %>%
    dplyr::select(dplyr::all_of(c(target_col, "data_source_year")),
                  dplyr::matches("_M$"))

  ## Strip MOE columns before re-running definitions
  result_for_defs = result %>%
    as.data.frame() %>%
    dplyr::select(-dplyr::matches("_M$"))

  ## Re-run definitions for each resolved table to recalculate percentages
  result_for_defs = purrr::reduce(resolved_tables, function(.data, table_name) {
    table_entry = get_table(table_name)
    if (!is.null(table_entry) && !is.null(table_entry[["definitions"]]) && length(table_entry[["definitions"]]) > 0) {
      execute_definitions(.data, table_entry[["definitions"]])
    } else {
      .data
    }
  }, .init = result_for_defs)

  ## Re-attach MOE columns
  result = result_for_defs %>%
    dplyr::left_join(moe_cols, by = c(target_col, "data_source_year"))

  ####----Calculate SEs/MOEs for Percent Variables----####
  has_parsed_columns = all(c("numerator_vars", "numerator_subtract_vars",
                             "denominator_vars", "denominator_subtract_vars") %in% colnames(codebook))

  if (length(percent_variables) > 0 && has_parsed_columns) {
    percent_components = codebook %>%
      dplyr::filter(calculated_variable %in% percent_variables) %>%
      dplyr::select(
        calculated_variable,
        numerator_add = numerator_vars,
        numerator_subtract = numerator_subtract_vars,
        denominator_add = denominator_vars,
        denominator_subtract = denominator_subtract_vars)

    ## Helper to calculate SE/MOE for a single percent variable
    calculate_percent_se = function(df, component_row) {
      if (nrow(df) == 0) return(df)

      var_name = component_row$calculated_variable
      if (!var_name %in% colnames(df)) return(df)

      num_add = component_row$numerator_add[[1]]
      num_sub = component_row$numerator_subtract[[1]]
      denom_add = component_row$denominator_add[[1]]
      denom_sub = component_row$denominator_subtract[[1]]

      num_est_cols = c(num_add, num_sub)
      num_est_cols = num_est_cols[nchar(num_est_cols) > 0]
      denom_est_cols = c(denom_add, denom_sub)
      denom_est_cols = denom_est_cols[nchar(denom_est_cols) > 0]

      if (length(num_est_cols) == 0 || length(denom_est_cols) == 0) return(df)

      num_moe_cols = paste0(num_est_cols, "_M")
      denom_moe_cols = paste0(denom_est_cols, "_M")

      all_required = c(num_est_cols, denom_est_cols, num_moe_cols, denom_moe_cols)
      if (!all(all_required %in% colnames(df))) return(df)

      num_est = if (length(num_add) > 0) {
        rowSums(as.matrix(dplyr::select(df, dplyr::all_of(num_add))))
      } else { rep(0, nrow(df)) }
      if (length(num_sub) > 0) {
        num_est = num_est - rowSums(as.matrix(dplyr::select(df, dplyr::all_of(num_sub))))
      }

      denom_est = if (length(denom_add) > 0) {
        rowSums(as.matrix(dplyr::select(df, dplyr::all_of(denom_add))))
      } else { rep(0, nrow(df)) }
      if (length(denom_sub) > 0) {
        denom_est = denom_est - rowSums(as.matrix(dplyr::select(df, dplyr::all_of(denom_sub))))
      }

      num_se = tryCatch({
        if (length(num_est_cols) > 0) {
          se_sum(
            purrr::map(num_moe_cols, ~ df[[.x]]),
            purrr::map(num_est_cols, ~ df[[.x]]))
        } else { rep(0, nrow(df)) }
      }, error = function(e) rep(NA_real_, nrow(df)))

      denom_se = tryCatch({
        if (length(denom_est_cols) > 0) {
          se_sum(
            purrr::map(denom_moe_cols, ~ df[[.x]]),
            purrr::map(denom_est_cols, ~ df[[.x]]))
        } else { rep(0, nrow(df)) }
      }, error = function(e) rep(NA_real_, nrow(df)))

      if (all(is.na(num_se)) || all(is.na(denom_se))) return(df)

      percent_se = tryCatch({
        se_proportion_ratio(
          estimate_numerator = num_est,
          estimate_denominator = denom_est,
          se_numerator = num_se,
          se_denominator = denom_se)
      }, error = function(e) rep(NA_real_, nrow(df)))

      df %>%
        dplyr::mutate(
          !!paste0(var_name, "_M") := percent_se * 1.645)
    }

    result = purrr::reduce(
      seq_len(nrow(percent_components)),
      function(df, i) calculate_percent_se(df, percent_components[i, ]),
      .init = result)
  }

  return(result)
}


#' @title Aggregate or interpolate ACS data to custom geographies
#' @description Aggregate or interpolate ACS data from source geographies to
#'    user-defined target geographies. Supports two modes:
#'
#'    **Complete nesting** (`weight = NULL`): Each source geography maps entirely
#'    to one target geography. Count variables are summed, percentages are
#'    recalculated from summed components, and intensive variables (medians,
#'    averages) are computed as population-weighted averages.
#'
#'    **Fractional allocation** (`weight = "column_name"`): Source geographies
#'    can be split across multiple targets using crosswalk weights. Count
#'    variables and MOEs are multiplied by the weight before summing.
#'    Percentages are recalculated from interpolated components. Intensive
#'    variables use the allocated population as weights.
#'
#'    In fractional-allocation mode, area variables
#'    (`area_land_sq_kilometer`, `area_water_sq_kilometer`,
#'    `area_land_water_sq_kilometer`) and `population_density_land_sq_kilometer`
#'    (and its MOE) are set to `NA` in the result. Crosswalk weights typically
#'    represent a population share rather than an area share, so scaling
#'    source-geography areas by the weight is not meaningful. If you need area
#'    or density for the target geographies, join your own area measurements
#'    onto the returned data frame.
#'
#'    MOE propagation uses Census Bureau approximation formulas throughout.
#'    Crosswalk weights are treated as constants (no sampling error).
#'
#'    **NA handling**: Missing values (`NA`) in aggregated input columns
#'    propagate to the corresponding target geographies — i.e., if any source
#'    geography within a target has `NA` for a column being summed or averaged,
#'    the target's value for that column will be `NA`. A one-time warning is
#'    issued listing the affected columns. Filter or impute NAs upstream if a
#'    different behavior is desired.
#' @param .data A dataframe returned from \code{compile_acs_data()}.
#'    Must have a codebook attribute attached.
#' @param target_geoid Character. Column name for target geography identifiers.
#'    Must exist in \code{.data} or in \code{crosswalk}. The result renames
#'    this column to \code{GEOID}.
#' @param weight Character or \code{NULL}. When \code{NULL} (default), assumes
#'    complete nesting where each source geography maps entirely to one target.
#'    When a column name is provided, performs fractional allocation using that
#'    column as weights. Weights should sum to approximately 1 per source
#'    geography.
#' @param crosswalk A data frame containing the crosswalk mapping. Optional in
#'    both modes. When provided, joined to \code{.data} via \code{source_geoid}
#'    before processing. Must include columns for \code{source_geoid} and
#'    \code{target_geoid} (and \code{weight} if fractional allocation is used).
#' @param source_geoid Character. Column name for source geography identifiers.
#'    Must exist in \code{.data} (and in \code{crosswalk} if provided).
#'    Default is \code{"GEOID"}.
#' @param weight_variable Character. Variable name used for population-weighted
#'    averages of intensive variables (medians, averages, etc.).
#'    Default is \code{"total_population_universe"}.
#' @returns A dataframe aggregated to target geographies with recalculated
#'    estimates, MOEs, and percentages. A modified codebook is attached as an
#'    attribute.
#' @examples
#' \dontrun{
#' # First, create tract-level data
#' tract_data = compile_acs_data(
#'   tables = c("race", "snap"),
#'   years = 2022,
#'   geography = "tract",
#'   states = "DC"
#' )
#'
#' # Complete nesting: each tract belongs to exactly one neighborhood
#' tract_data$neighborhood = c("Downtown", "Downtown", "Uptown", ...)
#' neighborhood_data = interpolate_acs(
#'   .data = tract_data,
#'   target_geoid = "neighborhood"
#' )
#'
#' # Fractional allocation with a crosswalk
#' crosswalk = data.frame(
#'   GEOID = c("11001000100", "11001000100", "11001000201"),
#'   neighborhood = c("Downtown", "Chinatown", "Downtown"),
#'   alloc_weight = c(0.6, 0.4, 1.0)
#' )
#'
#' neighborhood_data = interpolate_acs(
#'   .data = tract_data,
#'   target_geoid = "neighborhood",
#'   weight = "alloc_weight",
#'   crosswalk = crosswalk
#' )
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
interpolate_acs = function(
    .data,
    target_geoid,
    weight = NULL,
    crosswalk = NULL,
    source_geoid = "GEOID",
    weight_variable = "total_population_universe") {

  ####----Input Validation----####
  codebook = attr(.data, "codebook")
  if (is.null(codebook)) {
    cli::cli_abort("Input data must have a {.var codebook} attribute. Use output from {.fun compile_acs_data}.")
  }

  if (!source_geoid %in% colnames(.data)) {
    cli::cli_abort("Column {.var {source_geoid}} not found in {.arg .data}.")
  }

  if (!weight_variable %in% colnames(.data)) {
    cli::cli_abort("Weight variable {.var {weight_variable}} not found in {.arg .data}.")
  }

  ## Get resolved tables from attribute (for re-running definitions)
  resolved_tables = attr(.data, "resolved_tables")
  if (is.null(resolved_tables)) {
    resolved_tables = names(.table_registry$tables)
  }

  ## Drop geometry if present
  if (inherits(.data, "sf")) {
    .data = sf::st_drop_geometry(.data)
  }

  ## Join crosswalk if provided
  if (!is.null(crosswalk)) {
    if (!is.data.frame(crosswalk)) {
      cli::cli_abort("{.arg crosswalk} must be a data frame.")
    }
    if (!source_geoid %in% colnames(crosswalk)) {
      cli::cli_abort("Column {.var {source_geoid}} not found in {.arg crosswalk}.")
    }
    if (!target_geoid %in% colnames(crosswalk)) {
      cli::cli_abort("Column {.var {target_geoid}} not found in {.arg crosswalk}.")
    }

    xwalk_cols = c(source_geoid, target_geoid)
    if (!is.null(weight)) {
      if (!weight %in% colnames(crosswalk)) {
        cli::cli_abort("Column {.var {weight}} not found in {.arg crosswalk}.")
      }
      xwalk_cols = c(xwalk_cols, weight)
    }

    .data = .data %>%
      dplyr::inner_join(
        crosswalk %>% dplyr::select(dplyr::all_of(xwalk_cols)),
        by = source_geoid)
  } else {
    if (!target_geoid %in% colnames(.data)) {
      cli::cli_abort("Column {.var {target_geoid}} not found in {.arg .data}. Provide a crosswalk or add the column.")
    }
    if (!is.null(weight) && !weight %in% colnames(.data)) {
      cli::cli_abort("Column {.var {weight}} not found in {.arg .data}. Provide a crosswalk or add the column.")
    }
  }

  ## Warn about NA values in target_geoid
  na_count = sum(is.na(.data[[target_geoid]]))
  if (na_count > 0) {
    cli::cli_warn("{na_count} row{?s} ha{?s/ve} NA values in {.var {target_geoid}} and will be excluded from aggregation.")
  }

  ## Filter out NA target_geoids
  data_filtered = .data %>%
    dplyr::filter(!is.na(!!rlang::sym(target_geoid)))

  codebook = ensure_aggregation_strategy(codebook)

  if (!is.null(weight)) {
    ####----Fractional Allocation Mode----####

    ## Validate weight values
    weight_values = data_filtered[[weight]]
    if (!is.numeric(weight_values)) {
      cli::cli_abort("Weight column {.var {weight}} must be numeric.")
    }
    if (any(weight_values < 0, na.rm = TRUE)) {
      cli::cli_abort("Weight column {.var {weight}} must contain non-negative values.")
    }

    ## Check if weights sum to ~1 per source geography and year
    weight_sums = data_filtered %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(source_geoid, "data_source_year")))) %>%
      dplyr::summarise(.weight_sum = sum(!!rlang::sym(weight), na.rm = TRUE), .groups = "drop")

    tolerance = 0.01
    non_unity = weight_sums %>%
      dplyr::filter(abs(.weight_sum - 1) > tolerance)
    if (nrow(non_unity) > 0) {
      cli::cli_warn(c(
        "{nrow(non_unity)} source geograph{?y/ies} ha{?s/ve} weights that do not sum to 1 (tolerance: {tolerance}).",
        "i" = "Range of weight sums: [{round(min(non_unity$.weight_sum), 4)}, {round(max(non_unity$.weight_sum), 4)}]."))
    }

    ####----Classify Variables for Pre-allocation----####
    sum_variables = codebook %>%
      dplyr::filter(aggregation_strategy == "sum") %>%
      dplyr::pull(calculated_variable)
    sum_variables = sum_variables[sum_variables %in% colnames(data_filtered)]

    ## MOE columns for count variables
    sum_moe_variables = paste0(sum_variables, "_M")
    sum_moe_variables = sum_moe_variables[sum_moe_variables %in% colnames(data_filtered)]

    ####----Pre-allocate: multiply extensive variables by crosswalk weight----####
    ## MOE(w * X) = w * MOE(X) when w is a constant (no sampling error in the
    ## crosswalk weight). This is the standard assumption for Census crosswalks.
    ## Area variables are intentionally NOT apportioned by the crosswalk weight
    ## here — the weight typically represents a population share, not an area
    ## share, so scaling area by a population weight is meaningless. Area and
    ## population density columns are set to NA in the aggregated result; users
    ## should supply their own area measurements for target geographies if
    ## density-dependent calculations are needed.
    data_allocated = data_filtered %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(sum_variables),
          ~ .x * !!rlang::sym(weight)),
        dplyr::across(
          dplyr::all_of(sum_moe_variables),
          ~ .x * !!rlang::sym(weight)))

    ## Weight variable and its MOE — only allocate if not already in sum_variables.
    ## total_population_universe is typically a sum variable, so it's already
    ## pre-multiplied above. This handles the edge case where the user specifies
    ## a weight_variable that isn't a count.
    weight_moe_variable = paste0(weight_variable, "_M")
    if (!weight_variable %in% sum_variables) {
      data_allocated = data_allocated %>%
        dplyr::mutate(
          !!weight_variable := !!rlang::sym(weight_variable) * !!rlang::sym(weight))
      if (weight_moe_variable %in% colnames(data_allocated)) {
        data_allocated = data_allocated %>%
          dplyr::mutate(
            !!weight_moe_variable := !!rlang::sym(weight_moe_variable) * !!rlang::sym(weight))
      }
    }

    data_for_agg = data_allocated
    codebook_tag = "interpolated"
  } else {
    ####----Complete Nesting Mode (no weights)----####
    data_for_agg = data_filtered
    codebook_tag = "aggregated"
  }

  ####----Aggregate via shared workhorse----####
  result = .aggregate_to_target(
    data_no_geom = data_for_agg,
    target_col = target_geoid,
    weight_variable = weight_variable,
    codebook = codebook,
    resolved_tables = resolved_tables)

  ####----Blank out area and density in fractional allocation mode----####
  ## Source-geography areas do not aggregate meaningfully to target geographies
  ## under fractional allocation (the weight is usually a population share, not
  ## an area share). Population density, which depends on area, is likewise not
  ## meaningful. Users should bring their own area measurements for target
  ## geographies if needed.
  if (!is.null(weight)) {
    na_cols = c("area_land_sq_kilometer", "area_water_sq_kilometer",
                "area_land_water_sq_kilometer",
                "population_density_land_sq_kilometer")
    na_cols = na_cols[na_cols %in% colnames(result)]
    if (length(na_cols) > 0) {
      result = result %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(na_cols), ~ NA_real_))
    }
  }

  ####----Rename target_geoid to GEOID for Consistency----####
  result = result %>%
    dplyr::rename(GEOID = !!rlang::sym(target_geoid))

  ####----Update Codebook----####
  if (codebook_tag == "aggregated") {
    updated_codebook = codebook %>%
      dplyr::mutate(
        definition = dplyr::case_when(
          aggregation_strategy == "sum" ~
            paste0(definition, " [Aggregated via direct summation.]"),
          aggregation_strategy == "recalculate_percent" ~
            paste0(definition, " [Percentage recalculated from summed components.]"),
          aggregation_strategy == "weighted_average" ~
            paste0(definition, " [Aggregated via population-weighted average using ", weight_variable, ".]"),
          TRUE ~ definition))
  } else {
    area_and_density_vars = c(
      "area_land_sq_kilometer", "area_water_sq_kilometer",
      "area_land_water_sq_kilometer",
      "population_density_land_sq_kilometer")
    updated_codebook = codebook %>%
      dplyr::mutate(
        definition = dplyr::case_when(
          calculated_variable %in% area_and_density_vars ~
            paste0(definition, " [Interpolated: set to NA; source areas are not apportioned by the crosswalk weight. Supply your own area measurements for target geographies.]"),
          aggregation_strategy == "sum" ~
            paste0(definition, " [Interpolated: allocated by crosswalk weight, then summed.]"),
          aggregation_strategy == "recalculate_percent" ~
            paste0(definition, " [Interpolated: percentage recalculated from interpolated components.]"),
          aggregation_strategy == "weighted_average" ~
            paste0(definition, " [Interpolated: population-weighted average using allocated ", weight_variable, ".]"),
          TRUE ~ definition))
  }

  updated_codebook = updated_codebook %>%
    dplyr::select(calculated_variable, variable_type, definition, dplyr::everything())

  attr(result, "codebook") = updated_codebook

  return(result)
}

utils::globalVariables(c(
  ":=", "variable_type", "aggregation_strategy", "calculated_variable",
  "total_population_universe", "area_land_sq_kilometer",
  "population_density_land_sq_kilometer",
  "data_source_year",
  "numerator_vars", "numerator_subtract_vars", "denominator_vars",
  "denominator_subtract_vars", ".weight_sum"))
