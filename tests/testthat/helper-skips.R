# The Census Bureau API requires a key for the variables metadata endpoint
# (see load_acs_variables()), so any test that touches the ACS variable
# dictionary or queries data needs a key, not just network access.
skip_if_no_census_key = function() {
  testthat::skip_if(!nzchar(Sys.getenv("CENSUS_API_KEY")), "CENSUS_API_KEY not set")
}
