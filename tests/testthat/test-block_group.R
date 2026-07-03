# Block-group geography support.
#
# To (re)generate the integration fixture (requires a Census API key + network):
# bg_df = compile_acs_data(
#   tables = c("race", "tenure", "sex_by_age", "units_in_structure"),
#   years = 2022, geography = "block group", states = "DC", counties = "11001")
# saveRDS(bg_df, test_path("fixtures", "bg_test_data_2026-06-26.rds"))

bg_fixture_path = test_path("fixtures", "bg_test_data_2026-06-26.rds")

####----Availability partitioning (pure logic, no network)----####

testthat::test_that("bg_partition_tables classifies keep / dropped / partial from the codebook", {
  ## synthetic codebook: total population at block group; gini only at tract;
  ## median household income base at block group but its race iteration at tract
  census_codebook = tibble::tribble(
    ~name,          ~geography,
    "B01003_001",   "block group",
    "B19083_001",   "tract",
    "B19013_001",   "block group",
    "B19013A_001",  "tract")

  partition = bg_partition_tables(
    resolved_tables = c("total_population", "gini", "median_household_income", "population_density"),
    census_codebook = census_codebook)

  ## fully-available and derived tables are kept
  testthat::expect_true(all(c("total_population", "population_density") %in% partition[["keep"]]))
  ## a table with no block-group variables is dropped
  testthat::expect_true("gini" %in% partition[["dropped"]])
  testthat::expect_false("gini" %in% partition[["keep"]])
  ## a partially-available table is kept and records its dropped variables
  testthat::expect_true("median_household_income" %in% partition[["keep"]])
  testthat::expect_true("median_household_income" %in% names(partition[["partial"]]))
  testthat::expect_true("B19013A_001" %in% partition[["partial"]][["median_household_income"]])
  testthat::expect_false("B19013_001" %in% partition[["partial"]][["median_household_income"]])
})

testthat::test_that("bg_partition_tables errors when the codebook lacks a geography column", {
  census_codebook = tibble::tibble(name = "B01003_001", label = "x", concept = "y")
  testthat::expect_error(
    bg_partition_tables("total_population", census_codebook),
    "geography")
})

####----Input validation (errors fire before any data query)----####

testthat::test_that("block-group queries require an explicit states argument", {
  testthat::expect_error(
    compile_acs_data(years = 2022, geography = "block group"),
    "require an explicit `states`")
})

testthat::test_that("block-group geography is only supported for 2013 and later", {
  testthat::expect_error(
    compile_acs_data(years = 2011, geography = "block group", states = "DC"),
    "2013 and later")
})

testthat::test_that("census block geography is not supported", {
  testthat::expect_error(
    compile_acs_data(years = 2022, geography = "block"),
    "not supported")
})

####----Discovery----####

testthat::test_that("list_tables(geography = 'block group') returns a restricted subset", {
  testthat::skip_if_offline()
  skip_if_no_census_key()
  bg_tables = list_tables(geography = "block group")
  all_tables = list_tables()

  testthat::expect_true(length(bg_tables) < length(all_tables))
  ## tables known to be published at the block-group level
  testthat::expect_true(all(c("race", "tenure", "age") %in% bg_tables))
  ## tables known NOT to be published at the block-group level
  testthat::expect_false(any(c("gini", "poverty", "snap") %in% bg_tables))
})

####----Integration fixture----####

testthat::test_that("block-group data has 12-digit GEOIDs and bounded percentages", {
  testthat::skip_if_not(file.exists(bg_fixture_path), "Block-group fixture not available")
  bg_df = readRDS(bg_fixture_path)

  ## block-group GEOIDs are 12 characters (state + county + tract + block group)
  testthat::expect_true(all(nchar(bg_df[["GEOID"]]) == 12))

  ## all percentages are bounded within [0, 1]
  percent_bounds = bg_df %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::matches("percent$")) %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric),
                                   list(min = ~ min(.x, na.rm = TRUE),
                                        max = ~ max(.x, na.rm = TRUE))))
  mins = percent_bounds %>% dplyr::select(dplyr::ends_with("_min")) %>% unlist()
  maxes = percent_bounds %>% dplyr::select(dplyr::ends_with("_max")) %>% unlist()
  testthat::expect_true(all(mins >= 0, na.rm = TRUE))
  testthat::expect_true(all(maxes <= 1, na.rm = TRUE))

  ## a dropped (unavailable) table contributes no columns
  testthat::expect_false(any(grepl("gini", names(bg_df))))

  ## a codebook is attached
  testthat::expect_false(is.null(attr(bg_df, "codebook")))
})

testthat::test_that("block-group request of only DSL definitions does not error", {
  testthat::skip_if_offline()
  skip_if_no_census_key()
  ## total_population is available at the block-group level, so a custom definition
  ## computed from its columns should run (and not trip the "none available" error)
  bg_df = compile_acs_data(
    tables = list(define_percent(numerator = "total_population_universe",
                                 denominator = "total_population_universe",
                                 output = "all_population_share")),
    years = 2022, geography = "block group", states = "DC", counties = "11001")
  testthat::expect_true("all_population_share" %in% names(bg_df))
})
