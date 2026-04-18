## ---------------------------------------------------------------------------
## Unit tests for se_simple()
## ---------------------------------------------------------------------------

testthat::test_that("se_simple converts MOE to SE correctly", {
  testthat::expect_equal(se_simple(1.645), 1.0)
  testthat::expect_equal(se_simple(c(1.645, 3.29)), c(1.0, 2.0))
  testthat::expect_equal(se_simple(0), 0)
  testthat::expect_true(is.na(se_simple(NA_real_)))
})

## ---------------------------------------------------------------------------
## Unit tests for se_sum()
## ---------------------------------------------------------------------------

testthat::test_that("se_sum computes pooled SE for basic case (no zero estimates)", {
  moe1 = c(1.645, 3.29)
  moe2 = c(3.29, 1.645)
  est1 = c(100, 200)
  est2 = c(50, 150)

  result = se_sum(list(moe1, moe2), list(est1, est2))

  se1 = moe1 / 1.645
  se2 = moe2 / 1.645
  expected = sqrt(se1^2 + se2^2)
  testthat::expect_equal(result, expected)
})

testthat::test_that("se_sum applies Census zero-estimate rule", {
  # Row 1: est2 and est3 are both zero; est3 has the larger MOE (4.935 > 3.29)
  # So only est3's SE should be kept among the zero-estimate components
  moe1 = c(1.645)
  moe2 = c(3.29)
  moe3 = c(4.935)
  est1 = c(100)
  est2 = c(0)
  est3 = c(0)

  result = se_sum(list(moe1, moe2, moe3), list(est1, est2, est3))

  se1 = 1.645 / 1.645  # 1.0
  se3 = 4.935 / 1.645  # 3.0 (kept — largest MOE among zero estimates)
  # se2 is dropped (zero estimate with smaller MOE)
  expected = sqrt(se1^2 + se3^2)
  testthat::expect_equal(result, expected)
})

testthat::test_that("se_sum with single component equals se_simple", {
  moe = c(1.645, 3.29, 4.935)
  est = c(10, 20, 30)

  result = se_sum(list(moe), list(est))
  expected = se_simple(moe)
  testthat::expect_equal(result, expected)
})

testthat::test_that("se_sum includes NA-estimate components in sum", {
  moe1 = c(1.645)
  moe2 = c(3.29)
  est1 = c(100)
  est2 = c(NA_real_)

  result = se_sum(list(moe1, moe2), list(est1, est2))

  se1 = 1.645 / 1.645
  se2 = 3.29 / 1.645
  expected = sqrt(se1^2 + se2^2)
  testthat::expect_equal(result, expected)
})

testthat::test_that("se_sum handles mixed zero-estimate patterns across rows", {
  # Row 1: est2=0, est3=0 → keep only est3 (larger MOE: 4.935 > 3.29)
  # Row 2: est2=50, est3=0 → no zero-estimate filtering needed (only one zero)
  moe1 = c(1.645, 1.645)
  moe2 = c(3.29, 3.29)
  moe3 = c(4.935, 4.935)
  est1 = c(100, 100)
  est2 = c(0, 50)
  est3 = c(0, 0)

  result = se_sum(list(moe1, moe2, moe3), list(est1, est2, est3))

  # Row 1: se1 + se3 only (se2 dropped)
  se1_r1 = 1.645 / 1.645
  se3_r1 = 4.935 / 1.645
  expected_r1 = sqrt(se1_r1^2 + se3_r1^2)

  # Row 2: all three SEs included (only one zero estimate, no filtering)
  se1_r2 = 1.645 / 1.645
  se2_r2 = 3.29 / 1.645
  se3_r2 = 4.935 / 1.645
  expected_r2 = sqrt(se1_r2^2 + se2_r2^2 + se3_r2^2)

  testthat::expect_equal(result, c(expected_r1, expected_r2))
})

## ---------------------------------------------------------------------------
## Integration tests (fixture-based)
## ---------------------------------------------------------------------------

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
