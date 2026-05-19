####----Tests for interpolate_acs()----####

test_data_path = testthat::test_path("fixtures", "test_data_2026-02-08.rds")
test_codebook_path = testthat::test_path("fixtures", "codebook_2026-02-08.rds")

## Helper: subset data to a small set of tables so tests run in seconds
## rather than minutes. Subsets both columns and codebook.
fast_tables = c("total_population", "snap", "race", "median_household_income")

slim_for_test = function(df, tables = fast_tables) {
  codebook = attr(df, "codebook")
  all_cb_vars = codebook$calculated_variable

  table_prefixes = c("total_population", "snap", "race", "median_household_income")
  keep_vars = all_cb_vars[purrr::map_lgl(all_cb_vars, function(v) {
    any(purrr::map_lgl(table_prefixes, ~ stringr::str_starts(v, .x)))
  })]
  keep_vars = c(keep_vars, "data_source_year", "GEOID", "NAME")
  keep_cols = intersect(c(keep_vars, paste0(keep_vars, "_M")), colnames(df))

  df_slim = df[, keep_cols, drop = FALSE]
  attr(df_slim, "codebook") = codebook %>%
    dplyr::filter(calculated_variable %in% keep_vars)
  attr(df_slim, "resolved_tables") = tables
  df_slim
}

####----Input Validation Tests----####

testthat::test_that(
  "Input validation: requires codebook attribute",
  {
    fake_data = data.frame(
      GEOID = c("1", "2"),
      target = c("A", "A"),
      w = c(0.5, 0.5),
      total_population_universe = c(100, 200)
    )

    testthat::expect_error(
      interpolate_acs(fake_data, target_geoid = "target", weight = "w"),
      "codebook attribute"
    )

    testthat::expect_error(
      interpolate_acs(fake_data, target_geoid = "target"),
      "codebook attribute"
    )
  }
)

testthat::test_that(
  "Input validation: requires source_geoid column in .data",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path)

    testthat::expect_error(
      interpolate_acs(df, source_geoid = "nonexistent",
                      target_geoid = "tgt", weight = "w"),
      "not found in .data"
    )
  }
)

testthat::test_that(
  "Input validation: requires target_geoid column when no crosswalk",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path)

    testthat::expect_error(
      interpolate_acs(df, target_geoid = "nonexistent", weight = "w"),
      "not found in .data"
    )

    testthat::expect_error(
      interpolate_acs(df, target_geoid = "nonexistent"),
      "not found in .data"
    )
  }
)

testthat::test_that(
  "Input validation: requires weight column when no crosswalk (fractional mode)",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path)
    df$target = "A"

    testthat::expect_error(
      interpolate_acs(df, target_geoid = "target", weight = "nonexistent"),
      "not found in .data"
    )
  }
)

testthat::test_that(
  "Input validation: crosswalk must be a data frame",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path)

    testthat::expect_error(
      interpolate_acs(df, crosswalk = "not_a_df",
                      target_geoid = "tgt", weight = "w"),
      "must be a data frame"
    )
  }
)

testthat::test_that(
  "Input validation: crosswalk must contain required columns",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path)

    bad_xwalk = data.frame(GEOID = "1", tgt = "A")

    testthat::expect_error(
      interpolate_acs(df, crosswalk = bad_xwalk,
                      target_geoid = "tgt", weight = "w"),
      "not found in crosswalk"
    )
  }
)

testthat::test_that(
  "Input validation: negative weights produce error",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:3)
    df$target = "A"
    df$w = c(-0.5, 0.5, 1.0)

    testthat::expect_error(
      interpolate_acs(df, target_geoid = "target", weight = "w"),
      "non-negative"
    )
  }
)

testthat::test_that(
  "Input validation: non-unity weights produce warning",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:3) %>% slim_for_test()

    ## crosswalk that only sends 50% of each tract to a target
    xwalk = data.frame(
      GEOID = df$GEOID,
      target = "A",
      w = 0.5
    )

    testthat::expect_warning(
      interpolate_acs(df, crosswalk = xwalk,
                      target_geoid = "target", weight = "w"),
      "do not sum to 1"
    )
  }
)

####----Fractional Allocation (weight != NULL) Tests----####

testthat::test_that(
  "Identity crosswalk (weight=1, 1:1 mapping) returns original count values",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    ## Each tract maps to itself with weight 1
    xwalk = data.frame(
      GEOID = df$GEOID,
      target = df$GEOID,
      w = 1.0
    )

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "target", weight = "w")

    ## Count variables should match original values
    testthat::expect_equal(
      result$snap_received,
      df$snap_received
    )
    testthat::expect_equal(
      result$total_population_universe,
      df$total_population_universe
    )
  }
)

testthat::test_that(
  "Identity crosswalk preserves MOE values for count variables",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    xwalk = data.frame(
      GEOID = df$GEOID,
      target = df$GEOID,
      w = 1.0
    )

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "target", weight = "w")

    testthat::expect_equal(
      result$snap_received_M,
      df$snap_received_M,
      tolerance = 0.01
    )
  }
)

testthat::test_that(
  "Proportional split: weight=0.5 produces half the count",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1) %>% slim_for_test()

    ## Split one tract evenly into two targets
    xwalk = data.frame(
      GEOID = rep(df$GEOID, 2),
      target = c("A", "B"),
      w = c(0.5, 0.5)
    )

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "target", weight = "w")

    ## Each target should get half the population
    testthat::expect_equal(
      result$total_population_universe[result$GEOID == "A"],
      df$total_population_universe * 0.5
    )
    testthat::expect_equal(
      result$total_population_universe[result$GEOID == "B"],
      df$total_population_universe * 0.5
    )
  }
)

testthat::test_that(
  "Proportional split: MOE scales by weight",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1) %>% slim_for_test()

    xwalk = data.frame(
      GEOID = rep(df$GEOID, 2),
      target = c("A", "B"),
      w = c(0.6, 0.4)
    )

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "target", weight = "w")

    ## With a single source, MOE(w*X) = w * MOE(X), and se_sum of a single
    ## element just returns that element's SE * 1.645
    testthat::expect_equal(
      result$snap_received_M[result$GEOID == "A"],
      df$snap_received_M * 0.6,
      tolerance = 0.01
    )
    testthat::expect_equal(
      result$snap_received_M[result$GEOID == "B"],
      df$snap_received_M * 0.4,
      tolerance = 0.01
    )
  }
)

testthat::test_that(
  "Aggregated count MOE matches manual se_sum calculation",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    set.seed(42)
    df = readRDS(test_data_path) %>% dplyr::slice_sample(n = 10) %>% slim_for_test()

    ## Assign each tract fully to one of two groups (weight = 1)
    xwalk = data.frame(
      GEOID = df$GEOID,
      target = rep(c("A", "B"), each = 5),
      w = 1.0
    )

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "target", weight = "w")

    ## Manual calculation for group "A"
    var_name = "snap_received"
    moe_name = paste0(var_name, "_M")

    group_a = df %>% dplyr::slice(1:5)
    manual_se = se_sum(as.list(group_a[[moe_name]]),
                       as.list(group_a[[var_name]]))
    manual_moe = manual_se * 1.645

    auto_moe = result %>%
      dplyr::filter(GEOID == "A") %>%
      dplyr::pull(!!moe_name)

    testthat::expect_equal(manual_moe, auto_moe, tolerance = 0.001)
  }
)

testthat::test_that(
  "Aggregated count MOE with fractional weights matches manual calculation",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:3) %>% slim_for_test()

    ## Two tracts go to target "A" with different weights
    xwalk = data.frame(
      GEOID = df$GEOID[1:2],
      target = "A",
      w = c(0.7, 0.3)
    )

    result = suppressWarnings(
      interpolate_acs(df, crosswalk = xwalk,
                      target_geoid = "target", weight = "w"))

    ## Manual: allocated MOEs are w * original MOE, then se_sum
    var_name = "snap_received"
    moe_name = paste0(var_name, "_M")

    allocated_moes = df[[moe_name]][1:2] * c(0.7, 0.3)
    allocated_ests = df[[var_name]][1:2] * c(0.7, 0.3)
    manual_se = se_sum(as.list(allocated_moes), as.list(allocated_ests))
    manual_moe = manual_se * 1.645

    auto_moe = result %>% dplyr::pull(!!moe_name)

    testthat::expect_equal(manual_moe, auto_moe, tolerance = 0.001)
  }
)

testthat::test_that(
  "Interpolated percentages are within valid range [0, 1]",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:20) %>% slim_for_test()

    ## Split into target geographies with varying weights
    set.seed(123)
    geoids = df$GEOID
    xwalk = purrr::map(geoids, function(g) {
      n_targets = sample(1:3, 1)
      weights = runif(n_targets)
      weights = weights / sum(weights)
      data.frame(
        GEOID = g,
        target = paste0("T", seq_len(n_targets)),
        w = weights
      )
    }) %>% purrr::list_rbind()

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "target", weight = "w")

    pct_vars = result %>%
      dplyr::select(dplyr::matches("_percent$")) %>%
      colnames()

    for (var in pct_vars) {
      vals = result[[var]]
      testthat::expect_true(
        all(is.na(vals) | (vals >= 0 & vals <= 1)),
        info = paste0("Variable ", var, " has values outside [0,1]")
      )
    }
  }
)

testthat::test_that(
  "Percent MOE matches manual se_proportion_ratio on interpolated components",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    set.seed(42)
    df = readRDS(test_data_path) %>% dplyr::slice_sample(n = 10) %>% slim_for_test()
    cb = attr(df, "codebook")
    testthat::skip_if_not(
      all(c("numerator_vars", "denominator_vars") %in% colnames(cb)),
      "Test fixture codebook lacks parsed columns for percent MOE")

    ## All tracts go to a single target with weight = 1 (sum all tracts)
    xwalk = data.frame(
      GEOID = df$GEOID,
      target = "ALL",
      w = 1.0
    )

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "target", weight = "w")

    ## Test snap_received_percent
    num_var = "snap_received"
    denom_var = "snap_universe"
    pct_var = "snap_received_percent"

    ## Manual: sum the components, then calculate proportion SE
    num_est = sum(df[[num_var]], na.rm = TRUE)
    denom_est = sum(df[[denom_var]], na.rm = TRUE)

    num_se = se_sum(as.list(df[[paste0(num_var, "_M")]]),
                    as.list(df[[num_var]]))
    denom_se = se_sum(as.list(df[[paste0(denom_var, "_M")]]),
                      as.list(df[[denom_var]]))

    manual_se = se_proportion_ratio(
      estimate_numerator = num_est,
      estimate_denominator = denom_est,
      se_numerator = num_se,
      se_denominator = denom_se)
    manual_moe = manual_se * 1.645

    auto_moe = result %>%
      dplyr::pull(paste0(pct_var, "_M"))

    testthat::expect_equal(manual_moe, auto_moe, tolerance = 0.001)
  }
)

testthat::test_that(
  "Crosswalk as separate data frame produces same result as columns in .data",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    xwalk = data.frame(
      GEOID = df$GEOID,
      target = c("A", "A", "B", "B", "B"),
      w = 1.0
    )

    ## Method 1: separate crosswalk
    result1 = interpolate_acs(df, crosswalk = xwalk,
                              target_geoid = "target", weight = "w")

    ## Method 2: columns in .data
    df_with_xwalk = df %>%
      dplyr::left_join(xwalk, by = "GEOID")
    result2 = interpolate_acs(df_with_xwalk,
                              target_geoid = "target", weight = "w")

    testthat::expect_equal(result1$total_population_universe,
                           result2$total_population_universe)
    testthat::expect_equal(result1$snap_received,
                           result2$snap_received)
  }
)

testthat::test_that(
  "GEOID column is populated from target_geoid (fractional mode)",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    xwalk = data.frame(
      GEOID = df$GEOID,
      neighborhood = c("Downtown", "Downtown", "Uptown", "Uptown", "Uptown"),
      alloc = 1.0
    )

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "neighborhood", weight = "alloc")

    testthat::expect_true("GEOID" %in% colnames(result))
    testthat::expect_setequal(result$GEOID, c("Downtown", "Uptown"))
  }
)

testthat::test_that(
  "Codebook attribute is preserved and updated with interpolation notes",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    xwalk = data.frame(
      GEOID = df$GEOID,
      target = "A",
      w = 1.0
    )

    result = interpolate_acs(df, crosswalk = xwalk,
                             target_geoid = "target", weight = "w")

    codebook = attr(result, "codebook")
    testthat::expect_false(is.null(codebook))
    testthat::expect_true(
      any(stringr::str_detect(codebook$definition, "Interpolated"))
    )
  }
)

testthat::test_that(
  "NA values in target_geoid are excluded with message",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    xwalk = data.frame(
      GEOID = df$GEOID,
      target = c("A", "A", NA, "B", "B"),
      w = 1.0
    )

    df_with_xwalk = df %>%
      dplyr::left_join(xwalk, by = "GEOID")

    testthat::expect_message(
      suppressWarnings(
        interpolate_acs(df_with_xwalk,
                        target_geoid = "target", weight = "w")),
      "NA values"
    )
  }
)

####----Complete Nesting (weight = NULL) Tests----####

testthat::test_that(
  "weight = NULL: simple grouping produces correct count sums",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    df$custom_group = "A"

    expected_snap = sum(df$snap_received, na.rm = TRUE)

    result = interpolate_acs(df, target_geoid = "custom_group")

    testthat::expect_equal(result$snap_received, expected_snap)
  }
)

testthat::test_that(
  "weight = NULL: single-tract group equals original values",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:3) %>% slim_for_test()

    df$custom_group = df$GEOID

    result = interpolate_acs(df, target_geoid = "custom_group")

    testthat::expect_equal(result$snap_received, df$snap_received)
    testthat::expect_equal(result$total_population_universe, df$total_population_universe)
  }
)

testthat::test_that(
  "weight = NULL: GEOID column is populated from target_geoid",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    df$neighborhood_id = "Neighborhood_A"

    result = interpolate_acs(df, target_geoid = "neighborhood_id")

    testthat::expect_true("GEOID" %in% colnames(result))
    testthat::expect_equal(result$GEOID, "Neighborhood_A")
  }
)

testthat::test_that(
  "weight = NULL: codebook has Aggregated notes",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    df$custom_group = "A"

    result = interpolate_acs(df, target_geoid = "custom_group")

    codebook = attr(result, "codebook")
    testthat::expect_false(is.null(codebook))
    testthat::expect_true(
      any(stringr::str_detect(codebook$definition, "Aggregated"))
    )
  }
)

testthat::test_that(
  "weight = NULL: MOEs are present for aggregated sum variables",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    df$custom_group = "A"

    result = interpolate_acs(df, target_geoid = "custom_group")

    testthat::expect_true("snap_received_M" %in% colnames(result))
    testthat::expect_true(result$snap_received_M > 0)
  }
)

testthat::test_that(
  "weight = NULL: aggregated percentages are within valid range [0, 1]",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:10) %>% slim_for_test()

    df$custom_group = rep(c("A", "B"), each = 5)

    result = interpolate_acs(df, target_geoid = "custom_group")

    pct_vars = result %>%
      dplyr::select(dplyr::matches("_percent$")) %>%
      colnames()

    for (var in pct_vars) {
      vals = result[[var]]
      testthat::expect_true(
        all(is.na(vals) | (vals >= 0 & vals <= 1)),
        info = paste0("Variable ", var, " has values outside [0,1]")
      )
    }
  }
)

testthat::test_that(
  "weight = NULL: NA values in target_geoid excluded with message",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    df$custom_group = c("A", "A", NA, "B", "B")

    testthat::expect_message(
      interpolate_acs(df, target_geoid = "custom_group"),
      "NA values"
    )
  }
)

testthat::test_that(
  "weight = NULL: MOE for count matches manual se_sum",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    set.seed(42)
    df = readRDS(test_data_path) %>% dplyr::slice_sample(n = 10) %>% slim_for_test()

    df$group_id = rep(c("A", "B"), each = 5)

    result = interpolate_acs(df, target_geoid = "group_id")

    var_name = "snap_received"
    moe_name = paste0(var_name, "_M")

    group_a = df %>% dplyr::slice(1:5)
    manual_se = se_sum(as.list(group_a[[moe_name]]),
                       as.list(group_a[[var_name]]))
    manual_moe = manual_se * 1.645

    auto_moe = result %>%
      dplyr::filter(GEOID == "A") %>%
      dplyr::pull(!!moe_name)

    testthat::expect_equal(manual_moe, auto_moe, tolerance = 0.001)
  }
)

testthat::test_that(
  "weight = NULL: percent MOE matches manual calculation",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    set.seed(42)
    df = readRDS(test_data_path) %>% dplyr::slice_sample(n = 10) %>% slim_for_test()
    cb = attr(df, "codebook")
    testthat::skip_if_not(
      all(c("numerator_vars", "denominator_vars") %in% colnames(cb)),
      "Test fixture codebook lacks parsed columns for percent MOE")

    df$group_id = rep(c("A", "B"), each = 5)

    result = interpolate_acs(df, target_geoid = "group_id")

    pct_var = "snap_received_percent"
    num_var = "snap_received"
    denom_var = "snap_universe"

    group_a = df %>% dplyr::slice(1:5)

    num_est = sum(group_a[[num_var]], na.rm = TRUE)
    denom_est = sum(group_a[[denom_var]], na.rm = TRUE)

    num_se = se_sum(as.list(group_a[[paste0(num_var, "_M")]]),
                    as.list(group_a[[num_var]]))
    denom_se = se_sum(as.list(group_a[[paste0(denom_var, "_M")]]),
                      as.list(group_a[[denom_var]]))

    manual_se = se_proportion_ratio(
      estimate_numerator = num_est,
      estimate_denominator = denom_est,
      se_numerator = num_se,
      se_denominator = denom_se)
    manual_moe = manual_se * 1.645

    auto_moe = result %>%
      dplyr::filter(GEOID == "A") %>%
      dplyr::pull(paste0(pct_var, "_M"))

    testthat::expect_equal(manual_moe, auto_moe, tolerance = 0.001)
  }
)

testthat::test_that(
  "weight = NULL: crosswalk join works without weight column",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:5) %>% slim_for_test()

    xwalk = data.frame(
      GEOID = df$GEOID,
      target = c("A", "A", "B", "B", "B")
    )

    result = interpolate_acs(df, crosswalk = xwalk, target_geoid = "target")

    testthat::expect_true("GEOID" %in% colnames(result))
    testthat::expect_setequal(result$GEOID, c("A", "B"))
    testthat::expect_equal(
      sum(result$total_population_universe),
      sum(df$total_population_universe)
    )
  }
)

testthat::test_that(
  "weight = NULL with weight = 1 crosswalk matches for counts",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path) %>% dplyr::slice(1:10) %>% slim_for_test()

    ## weight = NULL: direct grouping
    df_grouped = df
    df_grouped$custom_group = rep(c("A", "B"), each = 5)
    result_null = interpolate_acs(df_grouped, target_geoid = "custom_group")

    ## weight = "w" with w = 1: fractional mode with identity weights
    xwalk = data.frame(
      GEOID = df$GEOID,
      target = rep(c("A", "B"), each = 5),
      w = 1.0
    )
    result_weighted = interpolate_acs(df, crosswalk = xwalk,
                                       target_geoid = "target", weight = "w")

    testthat::expect_equal(
      result_null$total_population_universe,
      result_weighted$total_population_universe
    )
    testthat::expect_equal(
      result_null$snap_received,
      result_weighted$snap_received
    )
    testthat::expect_equal(
      result_null$snap_received_M,
      result_weighted$snap_received_M,
      tolerance = 0.001
    )
  }
)
