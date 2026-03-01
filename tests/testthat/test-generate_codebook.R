## Testing data is created in test-compile_acs_data.R

test_data_path = test_path("fixtures", "test_data_2026-02-08.rds")
test_codebook_path = test_path("fixtures", "codebook_2026-02-08.rds")

####----Tests----####
## No missingness in codebook
  testthat::test_that(
    "No column in the codebook has a missing value.",
    {
      testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
      codebook = readRDS(test_codebook_path)

      results_missingness = codebook %>%
        dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), ~ is.na(.x))) %>%
        nrow

      testthat::expect_equal(results_missingness, 0) } )

## No transcribed function calls
  testthat::test_that(
    "No transcribed functions included in codebook output.",
    {
      testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
      codebook = readRDS(test_codebook_path)

      results_transcribed_functions = codebook %>%
        dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), ~ stringr::str_detect(.x, "dplyr"))) %>%
        nrow

      testthat::expect_equal(results_transcribed_functions, 0) } )

## No missing raw variable codes
  testthat::test_that(
    "No variable definitions contain '(NA)' in lieu of the raw variable code.",
    {
      testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
      codebook = readRDS(test_codebook_path)

      results_missing_raw_variables = codebook %>%
        dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), ~ stringr::str_detect(.x, "\\(\\)|\\(NA\\)"))) %>%
        nrow

      testthat::expect_equal(results_missing_raw_variables, 0) } )

## No universe variables in numerators (with specific exceptions)
## Note: educational enrollment numerator is calculated by subtracting from the universe,
## which is more concise than enumerating each of the individual component variables needed
  testthat::test_that(
    "Only population density and educational enrollment 1-12 contain a universe variable in the numerator.",
    {
      testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
      codebook = readRDS(test_codebook_path)

      results_universe_numerators = codebook %>%
        dplyr::filter(stringr::str_detect(definition, "Numerator.*universe.*Denominator")) %>%
        nrow

      testthat::expect_equal(results_universe_numerators, 2) } )

## No definitions for variables that are percentages of universes (not possible)
  testthat::test_that(
    "No calculated variables are perentages of a universe estimate.",
    {
      testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
      codebook = readRDS(test_codebook_path)

      results_universe_percentages = codebook %>%
        dplyr::filter(stringr::str_detect(calculated_variable, "universe.*percent$")) %>%
        nrow

      testthat::expect_equal(results_universe_percentages, 0) } )

## No codebook variable definitions that are missing from the input dataset
  testthat::test_that(
    "No codebook entries for variables that don't exist in the input data.",
    {
      testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
      testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
      ## Statistics for CA and TX Tracts
      df = readRDS(test_data_path)
      codebook = readRDS(test_codebook_path)

      results_phantom_definitions = codebook %>%
        dplyr::filter(!(calculated_variable %in% (df %>% colnames))) %>%
        nrow

      testthat::expect_equal(results_phantom_definitions, 0) } )

## All variables in the input data are in the codebook
  testthat::test_that(
    "All variables in the input data are in the codebook.",
    {
      testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
      testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
      ## Statistics for CA and TX Tracts
      df = readRDS(test_data_path)
      codebook = readRDS(test_codebook_path)

      derived_variables = df %>% dplyr::select(dplyr::matches("percent$")) %>% colnames
      undefined_variables = derived_variables[!(derived_variables %in% (codebook %>% dplyr::pull(calculated_variable)))]

      testthat::expect_equal(length(undefined_variables), 0) } )

####----Table Registry Codebook Tests----####

  testthat::test_that(
    "Registry codebook entries reference valid tables.",
    {
      all_tables = list_tables()
      testthat::expect_gte(length(all_tables), 30)
    })

  testthat::test_that(
    "All codebook entry types are recognized.",
    {
      valid_types = c("simple_percent", "across_percent", "across_sum",
                       "complex", "one_minus", "metadata")

      purrr::walk(names(.table_registry$tables), function(table_name) {
        table_entry = get_table(table_name)
        if (length(table_entry[["definitions"]]) > 0) {
          purrr::walk(table_entry[["definitions"]], function(entry) {
            testthat::expect_true(
              entry[["type"]] %in% valid_types,
              info = paste0("Table '", table_name, "' has unrecognized codebook entry type: ", entry[["type"]]))
          })
        }
      })
    })

  testthat::test_that(
    "All codebook entries have an output field.",
    {
      purrr::walk(names(.table_registry$tables), function(table_name) {
        table_entry = get_table(table_name)
        if (length(table_entry[["definitions"]]) > 0) {
          purrr::walk(table_entry[["definitions"]], function(entry) {
            testthat::expect_true(
              !is.null(entry[["output"]]) || entry[["type"]] %in% c("across_percent", "across_sum"),
              info = paste0("Table '", table_name, "' has codebook entry without output field"))
          })
        }
      })
    })
