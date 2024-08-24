####----Load Test Data----####

## Statistics for NJ Counties
df = compile_acs_data(
  variables = list_acs_variables(year = "2022"),
  years = 2022,
  geography = "county",
  states = "NJ",
  counties = NULL,
  retain_moes = TRUE,
  spatial = TRUE) %>%
  sf::st_drop_geometry() ## this is not included in the codebook

codebook = attr(df, "codebook")

#####----TESTING----#####

## No missingness in codebook
  testthat::test_that(
    "No column in the codebook has a missing value.",
    {
      results_missingness = codebook %>%
        dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), ~ is.na(.x))) %>%
        nrow

      testthat::expect_equal(results_missingness, 0) } )

## No transcribed function calls
  testthat::test_that(
    "No transcribed functions included in codebook output.",
    {
      results_transcribed_functions = codebook %>%
        dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), ~ stringr::str_detect(.x, "dplyr"))) %>%
        nrow

      testthat::expect_equal(results_transcribed_functions, 0) } )

## No missing raw variable codes
  testthat::test_that(
    "No variable definitions contain '(NA)' in lieu of the raw variable code.",
    {
      results_missing_raw_variables = codebook %>%
        dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), ~ stringr::str_detect(.x, "\\(\\)|\\(NA\\)"))) %>%
        nrow

      testthat::expect_equal(results_missing_raw_variables, 0) } )

## No universe variables in numerators (except population density)
  testthat::test_that(
    "Only population density contains a universe variable in the numerator.",
    {
      results_universe_numerators = codebook %>%
        dplyr::filter(stringr::str_detect(definition, "Numerator.*universe.*Denominator")) %>%
        nrow

      testthat::expect_equal(results_universe_numerators, 1) } )

## No definitions for variables that are percentages of universes (not possible)
  testthat::test_that(
    "No calculated variables are perentages of a universe estimate.",
    {
      results_universe_percentages = codebook %>%
        dplyr::filter(stringr::str_detect(calculated_variable, "universe.*percent$")) %>%
        nrow

      testthat::expect_equal(results_universe_percentages, 0) } )

## No codebook variable definitions that are missing from the input dataset
  testthat::test_that(
    "No codebook entries for variables that don't exist in the input data.",
    {
      results_phantom_definitions = codebook %>%
        dplyr::filter(!(calculated_variable %in% (df %>% colnames))) %>%
        nrow

      testthat::expect_equal(results_phantom_definitions, 0) } )

## All variables in the input data are in the codebook
  testthat::test_that(
    "All variables in the input data are in the codebook.",
    {
      derived_variables = df %>% dplyr::select(dplyr::matches("percent$")) %>% colnames
      undefined_variables = derived_variables[!(derived_variables %in% (codebook %>% dplyr::pull(calculated_variable)))]

      testthat::expect_equal(length(undefined_variables), 0) } )
