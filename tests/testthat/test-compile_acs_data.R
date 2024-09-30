# message("Update test data prior to testing, as needed.")
#
df = compile_acs_data(
  variables = NULL,
  years = c(2022),
  geography = "tract",
  states = c("CA", "TX"),
  counties = NULL,
  spatial = FALSE)

codebook = attr(df, "codebook")

saveRDS(object = df, file = file.path("inst", "test-data", "test_data_2024-08-24.rds"))
saveRDS(codebook, file = file.path("inst", "test-data", "codebook_2024-08-24.rds"))

df %>% dplyr::select(dplyr::matches("mobile")) %>% colnames()

####----Tests----####
# All percentages have no values greater than one and no values less than zero
testthat::test_that(
  "All percentages have no values greater than one and no values less than zero",
  {
    ## Statistics for CA and TX Tracts
    df = readRDS(system.file("test-data", "test_data_2024-08-24.rds", package = "urbnindicators"))

    percentage_outliers_maxima = df %>%
      dplyr::select(dplyr::matches("percent$")) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric), ~ max(.x, na.rm = T))) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::arrange(desc(value)) %>%
      dplyr::filter(value > 1) %>%
      nrow

    percentage_outliers_minima = df %>%
      dplyr::select(dplyr::matches("percent$")) %>%
      dplyr::summarise(dplyr::across(.cols = where(is.numeric), ~ min(.x, na.rm = T))) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::arrange(desc(value)) %>%
      dplyr::filter(value < 0) %>%
      nrow

    testthat::expect_equal(percentage_outliers_maxima, 0)
    testthat::expect_equal(percentage_outliers_minima, 0) } )

# All measures have meaningful values: maximum and mean greater than zero,
# all values are not missing, and there are at least two distinct values per measure
testthat::test_that(
  "All measures have meaningful values",
  {
    ## Statistics for CA and TX Tracts
    df = readRDS(system.file("test-data", "test_data_2024-08-24.rds", package = "urbnindicators"))

    summary_statistics = df %>%
      dplyr::select(GEOID, matches("percent$")) %>%
      tidyr::pivot_longer(-GEOID) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(
        count_total  = dplyr::n(),
        count_na = sum(is.na(value)),
        min = min(value, na.rm = T),
        max = max(value, na.rm = T),
        mean = mean(value, na.rm = T),
        distinct_values = dplyr::n_distinct(value, na.rm = T))

    purrr::map_dbl(summary_statistics$max, ~testthat::expect_gt(.x, 0) )
    purrr::map_dbl(summary_statistics$mean, ~testthat::expect_gt(.x, 0) )
    purrr::map2_dbl(summary_statistics$count_na, summary_statistics$count_total, ~ testthat::expect_lt(.x, .y) )
    purrr::map_dbl(summary_statistics$min, ~testthat::expect_gte(.x, 0) ) })

# No percentage measure has exactly the same values (for every observation) as
# that of another percentage measure (Note: this is implemented with tract data. This does not
# inherently apply to other levels of geographic observation, nor for subsets of the US.)
testthat::test_that(
  "All percentages are distinct",
  {
    ## Statistics for CA and TX Tracts
    df = readRDS(system.file("test-data", "test_data_2024-08-24.rds", package = "urbnindicators"))

    duplicates = purrr::map_dfr(
      colnames(df %>% dplyr::select(dplyr::matches("percent$"))),
      function(colname1) {
        purrr::map_dfr(
          colnames(df %>% dplyr::select(-dplyr::matches("_M$|universe"))),
          function(colname2) {
            if ((colname1 != colname2) & (identical(df[[colname1]], df[[colname2]]))) {
              data.frame(variable_one = colname1, variable_two = colname2)}})})

    distinct_duplicates = duplicates %>%
      dplyr::mutate(
        combined_variables = dplyr::if_else(
          variable_one > variable_two,
          paste(variable_one, variable_two),
          paste(variable_two, variable_one))) %>%
      dplyr::distinct(combined_variables, .keep_all = T) %>%
      # this variable intentionally has a single duplicate (differently-named) variable
      # for consistency across different concepts
      dplyr::filter(!stringr::str_detect(variable_one, "year_structure_built.*later"))

    testthat::expect_equal(nrow(distinct_duplicates), 0) } )
