# Tests for the raw-ACS-query cache (R/cache.R and the cache = TRUE fetch
# path in fetch_acs()). Unit tests mock acs_query() so no network or API key
# is needed; the integration tests at the bottom are gated on
# skip_if_no_census_key().

super_state_geographies = c(
  "us", "region", "division", "metropolitan/micropolitan statistical area",
  "metropolitan statistical area/micropolitan statistical area",
  "cbsa", "urban area", "zip code tabulation area", "zcta")

new_cache_stats = function() {
  stats = new.env(parent = emptyenv())
  stats$hits = 0
  stats$total = 0
  stats
}

cache_files = function(full.names = FALSE) {
  list.files(acs_cache_dir(), pattern = "^acs_.*\\.rds$", full.names = full.names)
}

## build a get_acs()-shaped wide tibble from a query's named variables vector
fake_acs_response = function(args) {
  values = purrr::set_names(
    purrr::map(seq_along(args$variables), ~ rep(.x, 2)),
    paste0(names(args$variables), "E"))
  dplyr::bind_cols(
    tibble::tibble(GEOID = c("10", "34"), NAME = c("Delaware", "New Jersey")),
    tibble::as_tibble(values))
}

test_that("cached_get_acs writes on a miss and reads on a hit", {
  withr::local_options(urbnindicators.cache_dir = withr::local_tempdir())
  calls = 0
  local_mocked_bindings(acs_query = function(args) {
    calls <<- calls + 1
    fake_acs_response(args)
  })

  stats = new_cache_stats()
  args = list(
    geography = "state", year = 2022, survey = "acs5", output = "wide",
    variables = c(alpha_count_ = "B00001_001"))

  first = cached_get_acs(args, table_name = "alpha", cache = TRUE, cache_stats = stats)
  expect_equal(calls, 1)
  expect_length(cache_files(), 1)

  second = cached_get_acs(args, table_name = "alpha", cache = TRUE, cache_stats = stats)
  expect_equal(calls, 1)
  expect_identical(second, first)
  expect_equal(stats$hits, 1)
  expect_equal(stats$total, 2)
})

test_that("cache keys distinguish geography, year, and state", {
  withr::local_options(urbnindicators.cache_dir = withr::local_tempdir())
  local_mocked_bindings(acs_query = fake_acs_response)

  base_args = list(
    geography = "state", year = 2022, survey = "acs5", output = "wide",
    variables = c(alpha_count_ = "B00001_001"))
  year_args = base_args
  year_args$year = 2021
  county_args = base_args
  county_args$geography = "county"
  state_args = c(base_args, list(state = "NJ"))

  purrr::walk(
    list(base_args, year_args, county_args, state_args),
    ~ cached_get_acs(.x, table_name = "alpha", cache = TRUE, cache_stats = new_cache_stats()))
  expect_length(cache_files(), 4)
})

test_that("cache entries are per table: subset and added tables reuse prior downloads", {
  withr::local_options(urbnindicators.cache_dir = withr::local_tempdir())
  fetched_tables = character(0)
  local_mocked_bindings(acs_query = function(args) {
    fetched_tables <<- c(fetched_tables, names(args$variables)[1])
    fake_acs_response(args)
  })

  map_ab = list(
    alpha = c(alpha_count_ = "B00001_001"),
    beta = c(beta_count_ = "B00002_001"))
  call_fetch = function(table_variable_map) {
    fetch_acs(
      geography = "state", variables = unlist(unname(table_variable_map)),
      years = 2022, states = "NJ", counties = c(), county_codes = NULL,
      super_state_geographies = super_state_geographies,
      cache = TRUE, table_variable_map = table_variable_map,
      cache_stats = new_cache_stats())
  }

  first = call_fetch(map_ab)
  expect_equal(fetched_tables, c("alpha_count_", "beta_count_"))
  expect_length(cache_files(), 2)
  ## assembled chunks follow the combined variable order, matching a single call
  expect_identical(
    colnames(first),
    c("GEOID", "NAME", "alpha_count_E", "beta_count_E", "data_source_year"))

  ## a subset request triggers no new fetches
  call_fetch(map_ab["alpha"])
  expect_equal(fetched_tables, c("alpha_count_", "beta_count_"))

  ## adding a table fetches only the new table
  map_abc = c(map_ab, list(gamma = c(gamma_count_ = "B00003_001")))
  call_fetch(map_abc)
  expect_equal(fetched_tables, c("alpha_count_", "beta_count_", "gamma_count_"))
  expect_length(cache_files(), 3)
})

test_that("cache = FALSE issues one combined query and writes nothing", {
  withr::local_options(urbnindicators.cache_dir = withr::local_tempdir())
  variable_counts = integer(0)
  local_mocked_bindings(acs_query = function(args) {
    variable_counts <<- c(variable_counts, length(args$variables))
    fake_acs_response(args)
  })

  variables = c(alpha_count_ = "B00001_001", beta_count_ = "B00002_001")
  fetch_acs(
    geography = "state", variables = variables, years = c(2021, 2022),
    states = "NJ", counties = c(), county_codes = NULL,
    super_state_geographies = super_state_geographies, cache = FALSE)

  ## one combined (two-variable) call per year, nothing cached
  expect_equal(variable_counts, c(2L, 2L))
  expect_length(cache_files(), 0)
})

test_that("a corrupt cache file triggers a refetch that repairs it", {
  withr::local_options(urbnindicators.cache_dir = withr::local_tempdir())
  calls = 0
  local_mocked_bindings(acs_query = function(args) {
    calls <<- calls + 1
    fake_acs_response(args)
  })

  args = list(
    geography = "state", year = 2022, survey = "acs5", output = "wide",
    variables = c(alpha_count_ = "B00001_001"))
  first = cached_get_acs(args, table_name = "alpha", cache = TRUE, cache_stats = new_cache_stats())

  writeLines("not an rds file", cache_files(full.names = TRUE))
  second = cached_get_acs(args, table_name = "alpha", cache = TRUE, cache_stats = new_cache_stats())
  expect_equal(calls, 2)
  expect_identical(second, first)
  expect_identical(readRDS(cache_files(full.names = TRUE)), first)
})

test_that("clear_acs_cache removes cached files and returns the count", {
  cache_dir = withr::local_tempdir()
  withr::local_options(urbnindicators.cache_dir = cache_dir)
  saveRDS(1, file.path(cache_dir, "acs_a.rds"))
  saveRDS(2, file.path(cache_dir, "acs_b.rds"))

  expect_message(removed <- clear_acs_cache(), "Removed 2")
  expect_equal(removed, 2)
  expect_length(cache_files(), 0)

  expect_message(removed_again <- clear_acs_cache(), "Removed 0")
  expect_equal(removed_again, 0)
})

test_that("compile_acs_data validates the cache argument", {
  expect_error(compile_acs_data(cache = "yes"), "must be TRUE or FALSE")
})

test_that("cache = TRUE output matches cache = FALSE output", {
  skip_if_no_census_key()
  withr::local_options(urbnindicators.cache_dir = withr::local_tempdir())

  compile_dc = function(cache) {
    suppressMessages(suppressWarnings(compile_acs_data(
      tables = c("snap", "race"), years = 2022, geography = "state",
      states = "DC", cache = cache)))
  }

  cached_cold = compile_dc(cache = TRUE)
  expect_gt(length(cache_files()), 0)

  uncached = compile_dc(cache = FALSE)
  expect_equal(cached_cold, uncached)

  cached_warm = compile_dc(cache = TRUE)
  expect_equal(cached_warm, uncached)
})
