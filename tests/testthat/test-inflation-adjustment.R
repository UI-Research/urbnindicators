testthat::test_that("compile_acs_data accepts dollar_year parameter", {
  # Test that the function accepts the new parameter without error
  testthat::expect_error(
    compile_acs_data(
      variables = list("median_household_income_universe_allraces_" = "B19013_001"),
      years = 2022,
      geography = "state",
      states = "DE",
      spatial = FALSE,
      dollar_year = 2022
    ), 
    NA
  )
  
  # Test that the function works with dollar_year = NULL (default)
  testthat::expect_error(
    compile_acs_data(
      variables = list("median_household_income_universe_allraces_" = "B19013_001"),
      years = 2022,
      geography = "state", 
      states = "DE",
      spatial = FALSE,
      dollar_year = NULL
    ),
    NA
  )
})

testthat::test_that("inflation adjustment helper functions work correctly", {
  # Test identify_dollar_variables function
  test_data <- data.frame(
    GEOID = "01",
    data_source_year = 2022,
    median_household_income_universe_allraces_ = 50000,
    median_household_income_universe_allraces__M = 1000,
    race_nonhispanic_white_alone_percent = 0.75,
    total_population_universe = 100000
  )
  
  dollar_vars <- identify_dollar_variables(test_data)
  testthat::expect_true("median_household_income_universe_allraces_" %in% dollar_vars)
  testthat::expect_true("median_household_income_universe_allraces__M" %in% dollar_vars)
  testthat::expect_false("race_nonhispanic_white_alone_percent" %in% dollar_vars)
  testthat::expect_false("total_population_universe" %in% dollar_vars)
})

testthat::test_that("CPI adjustment calculation works correctly", {
  # Test with mock CPI data
  mock_cpi_data <- data.frame(
    year = c(2020, 2021, 2022),
    cpi = c(258.8, 271.0, 292.7)
  )
  
  adjustment_factor <- calculate_cpi_adjustment_factor(2020, 2022, mock_cpi_data)
  expected_factor <- 292.7 / 258.8
  testthat::expect_equal(adjustment_factor, expected_factor, tolerance = 0.001)
})

testthat::test_that("dollar variables are adjusted correctly", {
  # Test data with dollar variables
  test_data <- data.frame(
    GEOID = "01",
    data_source_year = 2020,
    median_household_income_universe_allraces_ = 50000,
    median_household_income_universe_allraces__M = 1000,
    race_nonhispanic_white_alone_percent = 0.75
  )
  
  adjusted_data <- adjust_dollar_variables(test_data, 2022)
  
  # Check that dollar variables are adjusted
  testthat::expect_gt(
    adjusted_data$median_household_income_universe_allraces_,
    test_data$median_household_income_universe_allraces_
  )
  
  # Check that margins of error are adjusted
  testthat::expect_gt(
    adjusted_data$median_household_income_universe_allraces__M,
    test_data$median_household_income_universe_allraces__M
  )
  
  # Check that non-dollar variables are unchanged
  testthat::expect_equal(
    adjusted_data$race_nonhispanic_white_alone_percent,
    test_data$race_nonhispanic_white_alone_percent
  )
})

testthat::test_that("compile_acs_data with dollar_year produces adjusted values", {
  # Skip this test if API key is not available
  testthat::skip_if_not(Sys.getenv("CENSUS_API_KEY") != "", "Census API key not available")
  
  # Test with two different years to see adjustment
  df_2020 <- compile_acs_data(
    variables = list("median_household_income_universe_allraces_" = "B19013_001"),
    years = 2020,
    geography = "state",
    states = "DE", 
    spatial = FALSE,
    dollar_year = 2022
  )
  
  df_2022 <- compile_acs_data(
    variables = list("median_household_income_universe_allraces_" = "B19013_001"),
    years = 2022,
    geography = "state",
    states = "DE",
    spatial = FALSE,
    dollar_year = 2022
  )
  
  # 2020 data should be adjusted upward to 2022 dollars
  # 2022 data should remain unchanged
  testthat::expect_gte(
    df_2020$median_household_income_universe_allraces_,
    df_2022$median_household_income_universe_allraces_ * 0.9  # allowing for some variation
  )
})

testthat::test_that("codebook includes inflation adjustment information", {
  # Skip this test if API key is not available
  testthat::skip_if_not(Sys.getenv("CENSUS_API_KEY") != "", "Census API key not available")
  
  df <- compile_acs_data(
    variables = list("median_household_income_universe_allraces_" = "B19013_001"),
    years = 2020,
    geography = "state", 
    states = "DE",
    spatial = FALSE,
    dollar_year = 2022
  )
  
  codebook <- attr(df, "codebook")
  
  # Check that codebook mentions inflation adjustment
  income_entry <- codebook[codebook$variable == "median_household_income_universe_allraces_", ]
  testthat::expect_true(any(grepl("2022", income_entry$description)))
  testthat::expect_true(any(grepl("inflation", income_entry$description)))
})

testthat::test_that("user receives message about inflation adjustment", {
  # Skip this test if API key is not available
  testthat::skip_if_not(Sys.getenv("CENSUS_API_KEY") != "", "Census API key not available")
  
  testthat::expect_message(
    compile_acs_data(
      variables = list("median_household_income_universe_allraces_" = "B19013_001"),
      years = 2020,
      geography = "state",
      states = "DE",
      spatial = FALSE,
      dollar_year = 2022
    ),
    "inflation.adjusted"
  )
})

testthat::test_that("dollar_year=NULL defaults to latest year", {
  # Skip this test if API key is not available  
  testthat::skip_if_not(Sys.getenv("CENSUS_API_KEY") != "", "Census API key not available")
  
  df_explicit <- compile_acs_data(
    variables = list("median_household_income_universe_allraces_" = "B19013_001"),
    years = c(2020, 2022),
    geography = "state",
    states = "DE",
    spatial = FALSE,
    dollar_year = 2022
  )
  
  df_default <- compile_acs_data(
    variables = list("median_household_income_universe_allraces_" = "B19013_001"),
    years = c(2020, 2022),
    geography = "state",
    states = "DE", 
    spatial = FALSE,
    dollar_year = NULL
  )
  
  # Results should be identical
  testthat::expect_equal(
    df_explicit$median_household_income_universe_allraces_,
    df_default$median_household_income_universe_allraces_
  )
})