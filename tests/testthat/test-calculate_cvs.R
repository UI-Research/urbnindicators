test_data_path = test_path("fixtures", "test_data_2026-02-08.rds")
test_codebook_path = test_path("fixtures", "codebook_2026-02-08.rds")

testthat::test_that(
  "No CV has missing values for all observations",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    ## Statistics for CA and TX tracts
    df = readRDS(test_data_path)

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
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    ## Statistics for CA and TX tracts
    df = readRDS(test_data_path)

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
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path)
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
      0)
  }
)

testthat::test_that(
  "All _pct variables have CVs calculated",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
    df = readRDS(test_data_path)
    codebook = readRDS(test_codebook_path)

    ## _pct variables are raw ACS count variables renamed from _percent
    pct_vars = colnames(df) %>%
      stringr::str_subset("_pct$") %>%
      stringr::str_subset("_M$|_SE$|_CV$", negate = TRUE)

    ## all _pct variables should be in the codebook
    pct_in_codebook = pct_vars[pct_vars %in% codebook$calculated_variable]
    testthat::expect_equal(length(pct_in_codebook), length(pct_vars))

    ## all _pct variables should have corresponding _CV columns
    pct_cvs = paste0(pct_vars, "_CV")
    pct_cvs_present = pct_cvs[pct_cvs %in% colnames(df)]
    testthat::expect_equal(length(pct_cvs_present), length(pct_vars))

    ## all _pct variables should have corresponding _SE columns
    pct_ses = paste0(pct_vars, "_SE")
    pct_ses_present = pct_ses[pct_ses %in% colnames(df)]
    testthat::expect_equal(length(pct_ses_present), length(pct_vars))
  }
)

