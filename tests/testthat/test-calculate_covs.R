testthat::test_that(
  "No CV has missing values for all observations",
  {
    ## Statistics for CA and TX tracts
    df = readRDS(system.file("test-data", "test_data_2025-11-06.rds", package = "urbnindicators"))

    measure_level_quality = df %>%
      dplyr::select(dplyr::matches("_CV$")) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::arrange(dplyr::desc(value)) %>%
      dplyr::mutate(
        flag = dplyr::case_when(
          is.na(value) ~ NA,
          value > 1000 ~ "1000+",
          value > 100 ~ "100+",
          value > 30 ~ "30+",
          TRUE ~ "<=30")) %>%
      dplyr::group_by(name, flag) %>%
      dplyr::summarize(
        count = dplyr::n()) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(
        total = sum(count),
        percent_missing = count / total) %>%
      dplyr::ungroup()

    testthat::expect_lt(
      measure_level_quality %>%
        dplyr::slice(1) %>%
        dplyr::pull(percent_missing), 1) }
  )

testthat::test_that(
  "All measures have at least some values with modest CVs",
  {
    ## Statistics for CA and TX tracts
    df = readRDS(system.file("test-data", "test_data_2025-11-06.rds", package = "urbnindicators"))

    measure_level_quality = df %>%
      dplyr::select(dplyr::matches("_CV$")) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::arrange(dplyr::desc(value)) %>%
      dplyr::mutate(
        flag = dplyr::case_when(
          is.na(value) ~ NA,
          value > 1000 ~ "1000+",
          value > 100 ~ "100+",
          value > 30 ~ "30+",
          TRUE ~ "<=30")) %>%
      dplyr::group_by(name, flag) %>%
      dplyr::summarize(
        count = dplyr::n()) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(
        total = sum(count),
        percent_missing = count / total) %>%
      dplyr::ungroup()

    testthat::expect_gt(
      measure_level_quality %>%
        dplyr::filter(flag == "<=30") %>%
        nrow(), 0) }
)

testthat::test_that(
  "There is a CV for every variable that has an MOE (or for which one can be calculated)",
  {
    df = readRDS(system.file("test-data", "test_data_2025-11-06.rds", package = "urbnindicators"))
    moes = df %>%
      dplyr::select(dplyr::matches("_M$")) %>%
      colnames() %>%
      stringr::str_remove("_M$")
    cvs = df %>%
      dplyr::select(matches("_CV$")) %>%
      colnames() %>%
      stringr::str_remove("_CV$")

    testthat::expect_equal(
      moes[!moes %in% cvs] %>% length(),
      3) ## there are three controlled variables for which MOEs are not provided by Census
  }
)
