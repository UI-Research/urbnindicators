test_data_path = test_path("fixtures", "test_data_2026-02-08.rds")
test_codebook_path = test_path("fixtures", "codebook_2026-02-08.rds")

testthat::test_that(
  "No MOE has missing values for all observations",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    df = readRDS(test_data_path)

    moe_cols = df %>%
      dplyr::select(dplyr::matches("_M$"))

    ## every MOE column should have at least one non-NA value
    all_na_count = purrr::map_lgl(moe_cols, ~ all(is.na(.x))) %>% sum()
    testthat::expect_equal(all_na_count, 0)
  }
)

testthat::test_that(
  "All _pct variables have MOEs calculated",
  {
    testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
    testthat::skip_if_not(file.exists(test_codebook_path), "Test fixture not available")
    df = readRDS(test_data_path)
    codebook = readRDS(test_codebook_path)

    ## _pct variables are raw ACS count variables renamed from _percent
    pct_vars = colnames(df) %>%
      stringr::str_subset("_pct$") %>%
      stringr::str_subset("_M$", negate = TRUE)

    ## all _pct variables should be in the codebook
    pct_in_codebook = pct_vars[pct_vars %in% codebook$calculated_variable]
    testthat::expect_equal(length(pct_in_codebook), length(pct_vars))

    ## all _pct variables should have corresponding _M columns
    pct_moes = paste0(pct_vars, "_M")
    pct_moes_present = pct_moes[pct_moes %in% colnames(df)]
    testthat::expect_equal(length(pct_moes_present), length(pct_vars))
  }
)
