# Tests for the Census Bureau API key checks in R/census_api_key.R and the
# request-time detection in acs_query() (R/cache.R) and load_acs_variables().
# None of these tests hit the network: the pre-flight format checks are pure,
# the response detector is fed hand-built response objects, and the request-time
# paths mock tidycensus::get_acs() / httr::GET() to reproduce the API's real
# invalid-key behavior (observed live: a well-formed but fake key is redirected
# to an HTML error page, which makes JSON parsing fail with a lexical error).

####----Pre-flight format validation----####

test_that("validate_census_api_key() errors when no key is set", {
  withr::local_envvar(CENSUS_API_KEY = "")
  expect_error(validate_census_api_key(), "key is required")
})

test_that("validate_census_api_key() rejects a key that is too short", {
  withr::local_envvar(CENSUS_API_KEY = "abc123")
  expect_error(validate_census_api_key(), "does not look like a valid")
})

test_that("validate_census_api_key() rejects a long key with no numbers", {
  withr::local_envvar(CENSUS_API_KEY = "abcdefghijklmnop")
  expect_error(validate_census_api_key(), "missing numbers")
})

test_that("validate_census_api_key() rejects a long key with no letters", {
  withr::local_envvar(CENSUS_API_KEY = "1234567890123456")
  expect_error(validate_census_api_key(), "missing letters")
})

test_that("validate_census_api_key() accepts a well-formed key and returns it", {
  key = "abc123def456ghi789xy"
  withr::local_envvar(CENSUS_API_KEY = key)
  expect_silent(result <- validate_census_api_key())
  expect_identical(result, key)
})

####----Request-time detection: httr response----####

test_that("census_key_error_from_response() flags the invalid-key redirect", {
  response = list(
    url = "https://api.census.gov/data/invalid_key.html",
    headers = list(`content-type` = "text/html"))
  expect_true(census_key_error_from_response(response))
})

test_that("census_key_error_from_response() flags the missing-key redirect", {
  response = list(
    url = "https://api.census.gov/data/missing_key.html",
    headers = list(`content-type` = "text/html"))
  expect_true(census_key_error_from_response(response))
})

test_that("census_key_error_from_response() passes a normal JSON response", {
  response = list(
    url = "https://api.census.gov/data/2022/acs/acs5/variables.json?key=abc123def456",
    headers = list(`content-type` = "application/json;charset=utf-8"))
  expect_false(census_key_error_from_response(response))
})

####----Outage vs bad key: the reachability probe----####

test_that("census_api_is_reachable() returns FALSE when the probe request fails", {
  local_mocked_bindings(
    GET = function(...) stop("Could not resolve host: api.census.gov"),
    .package = "httr")
  expect_false(census_api_is_reachable())
})

test_that("census_api_is_reachable() returns TRUE against the live keyless endpoint", {
  # the probe endpoint needs no API key, so this is gated only on connectivity
  testthat::skip_on_cran()
  testthat::skip_if_offline("api.census.gov")
  expect_true(census_api_is_reachable())
})

test_that("abort_census_api_key_rejected() blames the key when the API is reachable", {
  local_mocked_bindings(census_api_is_reachable = function(...) TRUE)
  expect_error(abort_census_api_key_rejected(), "rejected the API key")
})

test_that("abort_census_api_key_rejected() reports an outage when the API is unreachable", {
  local_mocked_bindings(census_api_is_reachable = function(...) FALSE)
  expect_error(abort_census_api_key_rejected(), "temporary outage")
})

####----Request-time detection: tidycensus::get_acs (the compile path)----####

test_that("acs_query() turns the get_acs invalid-key parse error into a key message", {
  # This is the exact error tidycensus surfaces for a well-formed but fake key:
  # the API returns HTML where JSON is expected, so jsonlite fails to parse.
  # With the API reachable, the failure is attributed to the key.
  local_mocked_bindings(
    get_acs = function(...) {
      stop("lexical error: invalid char in json text.\n  <html style=...")
    },
    .package = "tidycensus")
  local_mocked_bindings(census_api_is_reachable = function(...) TRUE)

  expect_error(
    acs_query(list(geography = "state", variables = "B01001_001", year = 2022)),
    "rejected the API key")
})

test_that("acs_query() reports an outage when get_acs fails and the API is unreachable", {
  local_mocked_bindings(
    get_acs = function(...) {
      stop("lexical error: invalid char in json text.\n  <html style=...")
    },
    .package = "tidycensus")
  local_mocked_bindings(census_api_is_reachable = function(...) FALSE)

  expect_error(
    acs_query(list(geography = "state", variables = "B01001_001", year = 2022)),
    "temporary outage")
})

test_that("acs_query() passes through unrelated get_acs errors unchanged", {
  local_mocked_bindings(
    get_acs = function(...) stop("There was an error while running your query"),
    .package = "tidycensus")

  expect_error(
    acs_query(list(geography = "state", variables = "B01001_001", year = 2022)),
    "Census API request failed")
})

test_that("acs_query() returns get_acs output when the query succeeds", {
  fake = tibble::tibble(GEOID = "11", NAME = "District of Columbia", B01001_001E = 700000)
  local_mocked_bindings(get_acs = function(...) fake, .package = "tidycensus")

  expect_identical(
    acs_query(list(geography = "state", variables = "B01001_001", year = 2022)),
    fake)
})

####----load_acs_variables() rejects a fake key without an opaque parse error----####

test_that("load_acs_variables() reports a key problem when the API redirects to the error page", {
  # bypass the pre-flight format check with a well-formed (but fake) key, then
  # mock httr::GET to return the HTML error page the real API serves for it
  withr::local_envvar(CENSUS_API_KEY = "abc123def456ghi789xy")
  local_mocked_bindings(
    GET = function(...) {
      list(
        url = "https://api.census.gov/data/invalid_key.html",
        headers = list(`content-type` = "text/html"))
    },
    .package = "httr")
  # hold the probe fixed so this tests key detection, not the outage branch
  # (the httr::GET mock above would otherwise also intercept the probe)
  local_mocked_bindings(census_api_is_reachable = function(...) TRUE)

  # clear the in-memory cache so the mocked fetch actually runs
  rlang::env_unbind(.variables_cache, ls(.variables_cache))

  expect_error(load_acs_variables(year = 2022), "rejected the API key")
})
