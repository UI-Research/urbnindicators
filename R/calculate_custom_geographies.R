#' @title Aggregate ACS data to custom geographies
#' @description Aggregate tract-level ACS data to user-defined custom geographies
#'    by properly handling different variable types (counts, percentages, medians, etc.)
#'    and recalculating all error measures appropriately.
#' @param .data A dataframe returned from \code{compile_acs_data()} at the tract level.
#'    Must have a codebook attribute attached.
#' @param group_id Character. The name of a column in \code{.data} that contains
#'    the custom geography identifiers to aggregate to.
#' @param spatial Logical. If TRUE, dissolve tract geometries to create custom geography
#'    boundaries using \code{sf::st_union()}. Default is FALSE.
#' @param weight_variable Character. The variable name to use for population-weighted
#'    averages of non-aggregatable variables. Default is "total_population_universe".
#' @returns A dataframe aggregated to custom geographies with recalculated estimates,
#'    MOEs, SEs, and CVs. A modified codebook is attached as an attribute.
#' @examples
#' \dontrun{
#' # First, create tract-level data
#' tract_data = compile_acs_data(
#'   years = 2022,
#'   geography = "tract",
#'   states = "DC"
#' )
#'
#' # Add a custom geography column (e.g., from a crosswalk)
#' tract_data_with_neighborhoods = tract_data %>%
#'   dplyr::left_join(neighborhood_crosswalk, by = "GEOID")
#'
#' # Aggregate to custom geographies
#' neighborhood_data = calculate_custom_geographies(
#'   .data = tract_data_with_neighborhoods,
#'   group_id = "neighborhood_id",
#'   spatial = TRUE
#' )
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
calculate_custom_geographies = function(
    .data,
    group_id,
    spatial = FALSE,
    weight_variable = "total_population_universe") {

  ####----Input Validation----####
  codebook = attr(.data, "codebook")
  if (is.null(codebook)) {
    stop("Input data must have a codebook attribute. Use output from compile_acs_data().")
  }

  if (!group_id %in% colnames(.data)) {
    stop(paste0("Column '", group_id, "' not found in .data."))
  }

  if (!weight_variable %in% colnames(.data)) {
    stop(paste0("Weight variable '", weight_variable, "' not found in .data."))
  }

  ## Get resolved tables from attribute (for re-running definitions)
  resolved_tables = attr(.data, "resolved_tables")
  if (is.null(resolved_tables)) {
    resolved_tables = names(.table_registry$tables)
  }

  ## Warn about NA values in group_id
  na_count = sum(is.na(.data[[group_id]]))
  if (na_count > 0) {
    message(paste0(
      "Warning: ", na_count, " rows have NA values in '", group_id,
      "' and will be excluded from aggregation."))
  }

  ## Check for geometry
  has_geometry = inherits(.data, "sf")

  ## Filter out NA group_ids
  data_filtered = .data %>%
    dplyr::filter(!is.na(!!rlang::sym(group_id)))

  if (has_geometry) {
    data_no_geom = sf::st_drop_geometry(data_filtered)
  } else {
    data_no_geom = data_filtered
  }

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

  ## MOE variables for summed counts
  sum_moe_variables = paste0(sum_variables, "_M")
  sum_moe_variables = sum_moe_variables[sum_moe_variables %in% colnames(data_no_geom)]

  ####----Aggregate Sum Variables----####
  aggregated_sums = data_no_geom %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_id, "data_source_year")))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(sum_variables), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop")

  ## Calculate MOEs for summed variables using se_sum()
  aggregated_sum_moes = data_no_geom %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(group_id, "data_source_year"))))

  for (var in sum_variables) {
    moe_var = paste0(var, "_M")
    if (!moe_var %in% colnames(data_no_geom)) next

    var_moes = data_no_geom %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_id, "data_source_year")))) %>%
      dplyr::group_split() %>%
      purrr::map(function(group_df) {
        group_keys = group_df %>%
          dplyr::distinct(dplyr::across(dplyr::all_of(c(group_id, "data_source_year"))))

        estimates = group_df[[var]]
        moes = group_df[[moe_var]]

        se = se_sum(as.list(moes), as.list(estimates))

        group_keys %>%
          dplyr::mutate(!!moe_var := se * 1.645)
      }) %>% purrr::list_rbind()

    aggregated_sum_moes = aggregated_sum_moes %>%
      dplyr::left_join(var_moes, by = c(group_id, "data_source_year"))
  }

  ####----Aggregate Weighted Average Variables----####
  if (length(weighted_avg_variables) > 0) {
    weight_moe_variable = paste0(weight_variable, "_M")
    has_weight_moe = weight_moe_variable %in% colnames(data_no_geom)

    if (!has_weight_moe) {
      warning(paste0("MOE column '", weight_moe_variable, "' not found for weight variable. ",
                     "SE calculations for weighted averages will be skipped."))
    }

    aggregated_weighted = data_no_geom %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_id, "data_source_year")))) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(weighted_avg_variables),
          ~ sum(.x * .data[[weight_variable]], na.rm = TRUE) / sum(.data[[weight_variable]], na.rm = TRUE)),
        .groups = "drop")

    ## Calculate SEs for weighted averages
    if (has_weight_moe) {
      aggregated_weighted_ses = data_no_geom %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(c(group_id, "data_source_year"))))

      for (var in weighted_avg_variables) {
        moe_var = paste0(var, "_M")
        if (!moe_var %in% colnames(data_no_geom)) next

        se_col_name = paste0(var, "_SE")

        var_ses = data_no_geom %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c(group_id, "data_source_year")))) %>%
          dplyr::group_split() %>%
          purrr::map(function(group_df) {
            group_keys = group_df %>%
              dplyr::distinct(dplyr::across(dplyr::all_of(c(group_id, "data_source_year"))))

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

        aggregated_weighted_ses = aggregated_weighted_ses %>%
          dplyr::left_join(var_ses, by = c(group_id, "data_source_year"))
      }

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
        dplyr::select(dplyr::all_of(c(group_id, "data_source_year", moe_new_names)))
    } else {
      aggregated_weighted_ses = NULL
      aggregated_weighted_moes = NULL
    }
  } else {
    aggregated_weighted = NULL
    aggregated_weighted_ses = NULL
    aggregated_weighted_moes = NULL
  }

  ####----Handle Metadata Variables----####
  area_variables = c("area_land_sq_kilometer", "area_water_sq_kilometer", "area_land_water_sq_kilometer")
  area_variables = area_variables[area_variables %in% colnames(data_no_geom)]

  if (length(area_variables) > 0) {
    aggregated_areas = data_no_geom %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_id, "data_source_year")))) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(area_variables), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop")
  } else {
    aggregated_areas = data_no_geom %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(c(group_id, "data_source_year"))))
  }

  ####----Handle Geometry (if spatial = TRUE)----####
  if (spatial && has_geometry) {
    aggregated_geometry = data_filtered %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_id, "data_source_year")))) %>%
      dplyr::summarize(geometry = sf::st_union(geometry), .groups = "drop")
  }

  ####----Combine Results----####
  result = aggregated_sums %>%
    dplyr::left_join(aggregated_sum_moes, by = c(group_id, "data_source_year")) %>%
    dplyr::left_join(aggregated_areas, by = c(group_id, "data_source_year"))

  if (!is.null(aggregated_weighted)) {
    result = result %>%
      dplyr::left_join(aggregated_weighted, by = c(group_id, "data_source_year"))
    if (!is.null(aggregated_weighted_moes)) {
      result = result %>%
        dplyr::left_join(aggregated_weighted_moes, by = c(group_id, "data_source_year"))
    }
  }

  ####----Recalculate Population Density----####
  if ("total_population_universe" %in% colnames(result) && "area_land_sq_kilometer" %in% colnames(result)) {
    result = result %>%
      dplyr::mutate(
        population_density_land_sq_kilometer = safe_divide(total_population_universe, area_land_sq_kilometer))

    if ("total_population_universe_M" %in% colnames(result)) {
      result = result %>%
        dplyr::mutate(
          population_density_land_sq_kilometer_SE = se_simple(total_population_universe_M) / area_land_sq_kilometer,
          population_density_land_sq_kilometer_M = population_density_land_sq_kilometer_SE * 1.645,
          population_density_land_sq_kilometer_CV = cv(population_density_land_sq_kilometer, population_density_land_sq_kilometer_SE))
    }
  }

  ####----Recalculate Percent Variables via Registry Definitions----####
  ## Save MOE columns (execute_definitions doesn't use them, but regex patterns
  ## in resolve_regex_columns exclude _M$ already, so this is a safety measure)
  moe_cols = result %>%
    dplyr::select(dplyr::all_of(c(group_id, "data_source_year")),
                  dplyr::matches("_M$"))

  ## Strip MOE/SE/CV columns before re-running definitions
  result_for_defs = result %>%
    as.data.frame() %>%
    dplyr::select(-dplyr::matches("_M$|_SE$|_CV$"))

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
    dplyr::left_join(moe_cols, by = c(group_id, "data_source_year"))

  ####----Calculate SEs/MOEs for Percent Variables----####
  if (length(percent_variables) > 0) {
    percent_components = codebook %>%
      dplyr::filter(calculated_variable %in% percent_variables) %>%
      dplyr::select(
        calculated_variable,
        numerator_add = numerator_vars,
        numerator_subtract = numerator_subtract_vars,
        denominator_add = denominator_vars,
        denominator_subtract = denominator_subtract_vars)

    ## Helper to calculate SE/MOE/CV for a single percent variable
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
        rowSums(as.matrix(dplyr::select(df, dplyr::all_of(num_add))), na.rm = TRUE)
      } else { rep(0, nrow(df)) }
      if (length(num_sub) > 0) {
        num_est = num_est - rowSums(as.matrix(dplyr::select(df, dplyr::all_of(num_sub))), na.rm = TRUE)
      }

      denom_est = if (length(denom_add) > 0) {
        rowSums(as.matrix(dplyr::select(df, dplyr::all_of(denom_add))), na.rm = TRUE)
      } else { rep(0, nrow(df)) }
      if (length(denom_sub) > 0) {
        denom_est = denom_est - rowSums(as.matrix(dplyr::select(df, dplyr::all_of(denom_sub))), na.rm = TRUE)
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
          !!paste0(var_name, "_SE") := percent_se,
          !!paste0(var_name, "_M") := percent_se * 1.645,
          !!paste0(var_name, "_CV") := cv(df[[var_name]], percent_se))
    }

    result = purrr::reduce(
      seq_len(nrow(percent_components)),
      function(df, i) calculate_percent_se(df, percent_components[i, ]),
      .init = result)
  }

  ####----Calculate SEs and CVs for Sum Variables----####
  existing_sum_moe_cols = paste0(sum_variables, "_M")
  existing_sum_moe_cols = existing_sum_moe_cols[existing_sum_moe_cols %in% colnames(result)]

  if (length(existing_sum_moe_cols) > 0) {
    result = result %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(existing_sum_moe_cols),
          ~ se_simple(.x),
          .names = "{stringr::str_replace(.col, '_M$', '_SE')}"))

    existing_sum_vars = stringr::str_remove(existing_sum_moe_cols, "_M$")
    existing_sum_vars = existing_sum_vars[existing_sum_vars %in% colnames(result)]

    if (length(existing_sum_vars) > 0) {
      result = purrr::reduce(
        existing_sum_vars,
        function(df, var_name) {
          se_col = paste0(var_name, "_SE")
          cv_col = paste0(var_name, "_CV")
          if (se_col %in% colnames(df) && var_name %in% colnames(df)) {
            df %>%
              dplyr::mutate(!!cv_col := cv(df[[var_name]], df[[se_col]]))
          } else { df }
        },
        .init = result)
    }
  }

  ####----Attach Geometry if Spatial----####
  if (spatial && has_geometry) {
    result = result %>%
      as.data.frame() %>%
      dplyr::left_join(
        aggregated_geometry %>% dplyr::select(dplyr::all_of(c(group_id, "data_source_year"))),
        by = c(group_id, "data_source_year")) %>%
      sf::st_as_sf()
  }

  ####----Rename Group ID to GEOID for Consistency----####
  result = result %>%
    dplyr::rename(GEOID = !!rlang::sym(group_id))

  ####----Update Codebook----####
  updated_codebook = codebook %>%
    dplyr::mutate(
      definition = dplyr::case_when(
        aggregation_strategy == "sum" ~
          paste0(definition, " [Aggregated via direct summation.]"),
        aggregation_strategy == "recalculate_percent" ~
          paste0(definition, " [Percentage recalculated from summed components.]"),
        aggregation_strategy == "weighted_average" ~
          paste0(definition, " [Aggregated via population-weighted average using ", weight_variable, ".]"),
        TRUE ~ definition)) %>%
    dplyr::select(calculated_variable, variable_type, definition, dplyr::everything())

  attr(result, "codebook") = updated_codebook

  return(result)
}

utils::globalVariables(c(
  ":=", "variable_type", "aggregation_strategy", "calculated_variable",
  "total_population_universe", "area_land_sq_kilometer",
  "total_population_universe_M", "population_density_land_sq_kilometer",
  "population_density_land_sq_kilometer_SE", "data_source_year", "geometry",
  "numerator_vars", "numerator_subtract_vars", "denominator_vars", "denominator_subtract_vars"))
