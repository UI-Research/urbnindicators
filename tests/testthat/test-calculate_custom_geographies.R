####----Tests for calculate_custom_geographies()----####

testthat::test_that(
  "Input validation: requires codebook attribute",
  {
    # Create a dataframe without codebook attribute
    fake_data = data.frame(
      GEOID = c("1", "2"),
      group_id = c("A", "A"),
      total_population_universe = c(100, 200)
    )

    testthat::expect_error(
      calculate_custom_geographies(fake_data, group_id = "group_id"),
      "codebook attribute"
    )
  }
)

testthat::test_that(
  "Input validation: requires group_id column",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    testthat::expect_error(
      calculate_custom_geographies(df, group_id = "nonexistent_column"),
      "not found"
    )
  }
)

testthat::test_that(
  "Input validation: requires weight_variable column",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Add a group column
    df$custom_group = "A"

    testthat::expect_error(
      calculate_custom_geographies(df, group_id = "custom_group", weight_variable = "nonexistent"),
      "not found"
    )
  }
)

testthat::test_that(
  "Codebook: aggregation_strategy column has correct values by variable type",
  {
    # Load actual codebook from disk (with pre-parsed columns)
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "codebook_2025-11-06.rds")),
      "Test fixture not available")
    codebook = readRDS(testthat::test_path("test-data", "codebook_2025-11-06.rds"))

    # Check that codebook has the aggregation_strategy column
    testthat::expect_true("aggregation_strategy" %in% colnames(codebook))

    # Check that each variable type gets the expected aggregation strategy
    count_strategy = codebook %>%
      dplyr::filter(variable_type == "Count") %>%
      dplyr::pull(aggregation_strategy) %>%
      unique()
    testthat::expect_equal(count_strategy, "sum")

    percent_strategy = codebook %>%
      dplyr::filter(variable_type == "Percent") %>%
      dplyr::pull(aggregation_strategy) %>%
      unique()
    testthat::expect_equal(percent_strategy, "recalculate_percent")

    median_strategy = codebook %>%
      dplyr::filter(variable_type == "Median ($)") %>%
      dplyr::pull(aggregation_strategy) %>%
      unique()
    testthat::expect_equal(median_strategy, "weighted_average")

    metadata_strategy = codebook %>%
      dplyr::filter(variable_type == "Metadata") %>%
      dplyr::pull(aggregation_strategy) %>%
      unique()
    testthat::expect_equal(metadata_strategy, "metadata")
  }
)

testthat::test_that(
  "Single-tract custom geography equals original tract values for count variables",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Take first 3 tracts and assign each to its own custom geography
    df_subset = df %>%
      dplyr::slice(1:3) %>%
      dplyr::mutate(custom_group = GEOID)

    # Aggregate (should return same values since each tract is its own group)
    result = calculate_custom_geographies(df_subset, group_id = "custom_group")

    # Check that count variables match
    original_snap = df_subset$snap_received
    result_snap = result$snap_received

    testthat::expect_equal(result_snap, original_snap)
  }
)

testthat::test_that(
  "Aggregated percentages are within valid range [0, 1]",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Assign first 10 tracts to 2 custom geographies
    df_subset = df %>%
      dplyr::slice(1:10) %>%
      dplyr::mutate(custom_group = rep(c("A", "B"), each = 5))

    # Aggregate
    result = calculate_custom_geographies(df_subset, group_id = "custom_group")

    # Check that all percentage variables are in [0, 1]
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
  "Summing tracts produces correct total for count variable",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Assign first 5 tracts to one group
    df_subset = df %>%
      dplyr::slice(1:5) %>%
      dplyr::mutate(custom_group = "A")

    # Calculate expected sum
    expected_snap = sum(df_subset$snap_received, na.rm = TRUE)

    # Aggregate
    result = calculate_custom_geographies(df_subset, group_id = "custom_group")

    # Check
    testthat::expect_equal(result$snap_received, expected_snap)
  }
)

testthat::test_that(
  "MOEs are present for aggregated sum variables",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Assign first 5 tracts to one group
    df_subset = df %>%
      dplyr::slice(1:5) %>%
      dplyr::mutate(custom_group = "A")

    # Aggregate
    result = calculate_custom_geographies(df_subset, group_id = "custom_group")

    # Check that MOE exists for snap_received
    testthat::expect_true("snap_received_M" %in% colnames(result))

    # MOE should be positive
    testthat::expect_true(result$snap_received_M > 0)
  }
)

testthat::test_that(
  "GEOID column is renamed from group_id",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Assign first 5 tracts to one group
    df_subset = df %>%
      dplyr::slice(1:5) %>%
      dplyr::mutate(neighborhood_id = "Neighborhood_A")

    # Aggregate
    result = calculate_custom_geographies(df_subset, group_id = "neighborhood_id")

    # Check that GEOID column exists and contains the custom geography ID
    testthat::expect_true("GEOID" %in% colnames(result))
    testthat::expect_equal(result$GEOID, "Neighborhood_A")
  }
)

testthat::test_that(
  "Codebook attribute is preserved and updated",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Assign first 5 tracts to one group
    df_subset = df %>%
      dplyr::slice(1:5) %>%
      dplyr::mutate(custom_group = "A")

    # Aggregate
    result = calculate_custom_geographies(df_subset, group_id = "custom_group")

    # Check that codebook attribute exists
    codebook = attr(result, "codebook")
    testthat::expect_false(is.null(codebook))

    # Check that aggregation notes were added
    testthat::expect_true(
      any(stringr::str_detect(codebook$definition, "Aggregated"))
    )
  }
)

testthat::test_that(
  "Weighted average variables have reasonable values",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Assign first 10 tracts to 2 groups
    df_subset = df %>%
      dplyr::slice(1:10) %>%
      dplyr::mutate(custom_group = rep(c("A", "B"), each = 5))

    # Aggregate
    result = calculate_custom_geographies(df_subset, group_id = "custom_group")

    # Check that median household income exists and is positive
    if ("median_household_income_universe_allraces" %in% colnames(result)) {
      testthat::expect_true(
        all(result$median_household_income_universe_allraces > 0, na.rm = TRUE)
      )
    }
  }
)

testthat::test_that(
  "NA values in group_id are handled with warning",
  {
    # Load test data
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))

    # Assign first 5 tracts with some NAs
    df_subset = df %>%
      dplyr::slice(1:5) %>%
      dplyr::mutate(custom_group = c("A", "A", NA, "B", "B"))

    # Should produce a warning about NA values
    testthat::expect_message(
      calculate_custom_geographies(df_subset, group_id = "custom_group"),
      "NA values"
    )
  }
)

testthat::test_that(
  "MOE for count variable matches manual se_sum calculation",
  {
    # Load test data and sample 100 rows
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    set.seed(42)
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))
    df_sample = df %>%
      dplyr::slice_sample(n = 100) %>%
      dplyr::mutate(
        row_id = dplyr::row_number(),
        group_id = dplyr::if_else(row_id %% 2 == 0, "even", "odd"))

    # Run calculate_custom_geographies
    result = calculate_custom_geographies(df_sample, group_id = "group_id")

    # Test variable: snap_received (raw ACS count)
    var_name = "snap_received"
    moe_name = paste0(var_name, "_M")

    # Manual calculation for "odd" group
    group_odd = df_sample %>% dplyr::filter(group_id == "odd")
    manual_se = se_sum(
      as.list(group_odd[[moe_name]]),
      as.list(group_odd[[var_name]]))
    manual_moe = manual_se * 1.645

    # Get result from calculate_custom_geographies
    auto_moe = result %>%
      dplyr::filter(GEOID == "odd") %>%
      dplyr::pull(!!moe_name)

    testthat::expect_equal(manual_moe, auto_moe, tolerance = 0.001)
  }
)

testthat::test_that(
  "MOE for sum variable matches manual se_sum calculation",
  {
    # Load test data and sample 100 rows
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    set.seed(42)
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))
    df_sample = df %>%
      dplyr::slice_sample(n = 100) %>%
      dplyr::mutate(
        row_id = dplyr::row_number(),
        group_id = dplyr::if_else(row_id %% 2 == 0, "even", "odd"))

    # Run calculate_custom_geographies
    result = calculate_custom_geographies(df_sample, group_id = "group_id")

    # Test variable: age_10_14_years (derived sum of male + female)
    var_name = "age_10_14_years"
    moe_name = paste0(var_name, "_M")

    # Manual calculation for "odd" group
    group_odd = df_sample %>% dplyr::filter(group_id == "odd")
    manual_se = se_sum(
      as.list(group_odd[[moe_name]]),
      as.list(group_odd[[var_name]]))
    manual_moe = manual_se * 1.645

    # Get result from calculate_custom_geographies
    auto_moe = result %>%
      dplyr::filter(GEOID == "odd") %>%
      dplyr::pull(!!moe_name)

    testthat::expect_equal(manual_moe, auto_moe, tolerance = 0.001)
  }
)

testthat::test_that(
  "SE/MOE for percent variable matches manual calculation using se_sum and se_proportion_ratio",
  {
    # Load test data and sample 100 rows
    testthat::skip_if_not(
      file.exists(testthat::test_path("test-data", "test_data_2025-11-06.rds")),
      "Test fixture not available")
    set.seed(42)
    df = readRDS(testthat::test_path("test-data", "test_data_2025-11-06.rds"))
    df_sample = df %>%
      dplyr::slice_sample(n = 100) %>%
      dplyr::mutate(
        row_id = dplyr::row_number(),
        group_id = dplyr::if_else(row_id %% 2 == 0, "even", "odd"))

    # Run calculate_custom_geographies
    result = calculate_custom_geographies(df_sample, group_id = "group_id")

    # Test variable: snap_received_percent
    # Definition: Numerator = snap_received. Denominator = snap_universe.
    pct_var = "snap_received_percent"
    num_var = "snap_received"
    denom_var = "snap_universe"

    # Manual calculation for "odd" group
    group_odd = df_sample %>% dplyr::filter(group_id == "odd")

    # Step 1: Calculate aggregated estimates
    num_est = sum(group_odd[[num_var]], na.rm = TRUE)
    denom_est = sum(group_odd[[denom_var]], na.rm = TRUE)

    # Step 2: Calculate SEs for numerator and denominator using se_sum
    num_se = se_sum(
      as.list(group_odd[[paste0(num_var, "_M")]]),
      as.list(group_odd[[num_var]]))
    denom_se = se_sum(
      as.list(group_odd[[paste0(denom_var, "_M")]]),
      as.list(group_odd[[denom_var]]))

    # Step 3: Calculate SE for the proportion using se_proportion_ratio
    manual_se = se_proportion_ratio(
      estimate_numerator = num_est,
      estimate_denominator = denom_est,
      se_numerator = num_se,
      se_denominator = denom_se)
    manual_moe = manual_se * 1.645

    # Get result from calculate_custom_geographies
    auto_moe = result %>%
      dplyr::filter(GEOID == "odd") %>%
      dplyr::pull(paste0(pct_var, "_M"))

    testthat::expect_equal(manual_moe, auto_moe, tolerance = 0.001)
  }
)
