# Tests for how derived variables behave when their denominator is zero (for
# example, the share of a tract's population without health insurance when the
# tract has no population). Any ratio with a zero denominator is undefined, so
# safe_divide() returns NA, and complements (define_complement()) and margins
# of error of those ratios are NA as well.
#
# Unit tests use small hand-built inputs. Fixture tests use the committed
# Texas county fixture (see the regeneration recipe in
# test-compile_acs_data.R). The live test at the bottom is gated on
# skip_if_no_census_key().

test_data_path = testthat::test_path("fixtures", "test_data_2026-10-20.rds")

## the B27011 health insurance counts and their margins of error
health_insurance_columns = function(df) {
  colnames(df) %>%
    stringr::str_subset("^health_insurance_coverage_status_type_by_employment_status")
}

####----safe_divide()----####

testthat::test_that("safe_divide() returns ordinary quotients when the denominator is non-zero", {
  testthat::expect_equal(safe_divide(1, 2), 0.5)
  testthat::expect_equal(safe_divide(0, 5), 0)
  testthat::expect_equal(safe_divide(-3, 4), -0.75)
  testthat::expect_equal(safe_divide(6, 3), 2)
  testthat::expect_equal(safe_divide(c(1, 2, 3), c(4, 4, 4)), c(0.25, 0.5, 0.75))
})

testthat::test_that("safe_divide() returns NA for 0 / 0", {
  result = safe_divide(0, 0)
  testthat::expect_true(is.na(result))
  testthat::expect_false(is.nan(result))
  testthat::expect_type(result, "double")
})

testthat::test_that("safe_divide() returns NA for any non-zero numerator over zero", {
  result = safe_divide(c(3, -3, 0.0001, Inf), 0)
  testthat::expect_equal(result, rep(NA_real_, 4))
  testthat::expect_false(any(is.infinite(result)))
})

testthat::test_that("safe_divide() treats a negative-zero denominator as zero", {
  testthat::expect_true(is.na(safe_divide(1, -0)))
  testthat::expect_true(is.na(safe_divide(0, -0)))
})

testthat::test_that("safe_divide() propagates NA inputs", {
  testthat::expect_equal(
    safe_divide(c(NA, 1, NA, 0, NA), c(2, NA, NA, NA, 0)),
    rep(NA_real_, 5))
})

testthat::test_that("safe_divide() handles mixed zero and non-zero denominators element by element", {
  testthat::expect_equal(
    safe_divide(c(1, 0, 5, 0, 2), c(4, 0, 0, 3, 8)),
    c(0.25, NA, NA, 0, 0.25))
})

testthat::test_that("safe_divide() returns doubles for integer inputs", {
  result = safe_divide(c(1L, 0L, 3L), c(2L, 0L, 0L))
  testthat::expect_type(result, "double")
  testthat::expect_equal(result, c(0.5, NA, NA))
})

testthat::test_that("safe_divide() recycles a single-value numerator or denominator", {
  testthat::expect_equal(safe_divide(c(1, 2, 0), 2), c(0.5, 1, 0))
  testthat::expect_equal(safe_divide(c(1, 2, 0), 0), rep(NA_real_, 3))
  testthat::expect_equal(safe_divide(2, c(1, 4, 0)), c(2, 0.5, NA))
  testthat::expect_equal(safe_divide(0, c(1, 0)), c(0, NA))
})

testthat::test_that("safe_divide() returns an empty vector for empty inputs", {
  testthat::expect_identical(safe_divide(numeric(0), numeric(0)), numeric(0))
})

####----Each definition type----####

## Row 1: ordinary values. Row 2: zero universe (so every count is zero).
## Row 3: a positive universe with a zero count, which is a true 0%.
zero_universe_data = function() {
  tibble::tibble(
    universe = c(20, 0, 10),
    group_a = c(5, 0, 0),
    group_b = c(15, 0, 10),
    group_b_excluded = c(5, 0, 0))
}

testthat::test_that("simple percent is NA only when its denominator is zero", {
  result = execute_definition(
    zero_universe_data(),
    define_percent("group_a", denominator = "universe"))
  testthat::expect_equal(result$group_a_percent, c(0.25, NA, 0))
})

testthat::test_that("batch (each = TRUE) percents are NA only when their denominator is zero", {
  result = execute_definition(
    zero_universe_data(),
    define_percent("^group_[ab]$", denominator = "universe", each = TRUE))
  testthat::expect_equal(result$group_a_percent, c(0.25, NA, 0))
  testthat::expect_equal(result$group_b_percent, c(0.75, NA, 1))
})

testthat::test_that("batch percents with a per-column denominator are NA when that denominator is zero", {
  df = tibble::tibble(
    poverty_below_black = c(2, 0, 0),
    poverty_universe_black = c(10, 0, 5),
    poverty_below_white = c(4, 1, 0),
    poverty_universe_white = c(8, 4, 0))
  result = execute_definition(
    df,
    define_percent("poverty_below",
                   denominator_replace = c("below" = "universe"),
                   each = TRUE))
  testthat::expect_equal(result$poverty_below_black_percent, c(0.2, NA, 0))
  testthat::expect_equal(result$poverty_below_white_percent, c(0.5, 0.25, NA))
})

testthat::test_that("multi-column (complex) percent is NA when the summed denominator is zero", {
  result = execute_definition(
    zero_universe_data(),
    define_percent(c("group_a", "group_b"), denominator = "universe",
                   output = "groups_percent"))
  testthat::expect_equal(result$groups_percent, c(1, NA, 1))
})

testthat::test_that("percent is NA when subtracting from the denominator leaves zero", {
  ## Row 3 has a positive raw denominator, but every unit is subtracted out,
  ## so the adjusted denominator is zero and the ratio is undefined.
  df = tibble::tibble(
    universe = c(20, 0, 10),
    group_a = c(5, 0, 0),
    excluded = c(10, 0, 10))
  result = execute_definition(
    df,
    define_percent("group_a", denominator = "universe",
                   subtract_from_denominator = c("excluded"),
                   output = "group_a_adjusted_percent"))
  testthat::expect_equal(result$group_a_adjusted_percent, c(0.5, NA, NA))
})

testthat::test_that("percent with a numerator subtraction is NA when the denominator is zero", {
  result = execute_definition(
    zero_universe_data(),
    define_percent("^group_b$", denominator = "universe",
                   subtract_from_numerator = "^group_b_excluded$",
                   output = "group_b_net_percent"))
  testthat::expect_equal(result$group_b_net_percent, c(0.5, NA, 1))
})

testthat::test_that("complement is NA when its source percent is NA, and 1 - x otherwise", {
  result = execute_definitions(
    zero_universe_data(),
    list(
      define_percent("group_a", denominator = "universe"),
      define_complement("group_a_percent", output = "group_a_not_percent")))
  ## Row 2 (zero universe) was 1 under the old 0 / 0 = 0 rule.
  testthat::expect_equal(result$group_a_not_percent, c(0.75, NA, 1))
  testthat::expect_equal(
    result$group_a_percent + result$group_a_not_percent,
    c(1, NA, 1))
})

testthat::test_that("sums of zero counts are zero, not NA", {
  result = execute_definition(
    zero_universe_data(),
    define_sum(c("group_a", "group_b"), output = "groups_total"))
  testthat::expect_equal(result$groups_total, c(20, 0, 10))
})

####----Registered tables on fixture data----####

testthat::test_that("every registered percent on fixture data is NA exactly when its denominator is zero", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
  df = readRDS(test_data_path)
  codebook = attr(df, "codebook")

  derived_variables = codebook %>%
    dplyr::filter(!se_calculation_type %in% c("raw", "metadata", "weighted_average")) %>%
    dplyr::pull(calculated_variable)
  inputs = df %>%
    dplyr::select(-dplyr::any_of(c(derived_variables, paste0(derived_variables, "_M"))))
  recomputed = purrr::reduce(attr(df, "resolved_tables"), function(.data, table_name) {
    definitions = get_table(table_name)[["definitions"]]
    if (length(definitions) > 0) execute_definitions(.data, definitions) else .data
  }, .init = inputs)

  sum_columns = function(columns) {
    columns = columns[nchar(columns) > 0]
    if (length(columns) == 0) return(rep(0, nrow(recomputed)))
    rowSums(as.matrix(recomputed[, columns, drop = FALSE]))
  }

  percent_rows = codebook %>%
    dplyr::filter(se_calculation_type %in% c(
      "simple_percent", "complex_numerator", "complex_denominator", "complex_both"))
  checks = purrr::pmap_dfr(
    percent_rows %>% dplyr::select(calculated_variable, denominator_vars, denominator_subtract_vars),
    function(calculated_variable, denominator_vars, denominator_subtract_vars) {
      denominator = sum_columns(denominator_vars) - sum_columns(denominator_subtract_vars)
      tibble::tibble(
        variable = calculated_variable,
        mismatched_rows = sum(is.na(recomputed[[calculated_variable]]) != (denominator == 0)),
        zero_denominator_rows = sum(denominator == 0))
    })

  testthat::expect_equal(
    checks %>% dplyr::filter(mismatched_rows > 0) %>% dplyr::pull(variable),
    character(0))
  ## the fixture includes real zero denominators (e.g., counties with no NHPI
  ## residents), so the NA path is exercised on actual ACS data
  testthat::expect_gt(sum(checks$zero_denominator_rows > 0), 0)

  ## complements are NA exactly when their source is NA
  complement_rows = codebook %>% dplyr::filter(se_calculation_type == "one_minus")
  purrr::walk2(
    complement_rows$calculated_variable,
    complement_rows$numerator_vars,
    function(complement, source) {
      testthat::expect_equal(
        is.na(recomputed[[complement]]), is.na(recomputed[[source[[1]]]]),
        info = complement)
    })
})

testthat::test_that("recomputing fixture derived variables reproduces the stored values and NAs", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
  ## Guards against the fixture and the current division rule drifting apart:
  ## stored NAs must line up with recomputed NAs, and every other value must
  ## match.
  df = readRDS(test_data_path)
  codebook = attr(df, "codebook")

  derived_variables = codebook %>%
    dplyr::filter(!se_calculation_type %in% c("raw", "metadata", "weighted_average")) %>%
    dplyr::pull(calculated_variable)
  inputs = df %>%
    dplyr::select(-dplyr::any_of(c(derived_variables, paste0(derived_variables, "_M"))))
  recomputed = purrr::reduce(attr(df, "resolved_tables"), function(.data, table_name) {
    definitions = get_table(table_name)[["definitions"]]
    if (length(definitions) > 0) execute_definitions(.data, definitions) else .data
  }, .init = inputs)

  purrr::walk(intersect(derived_variables, colnames(recomputed)), function(variable) {
    stored = df[[variable]]
    new = recomputed[[variable]]
    testthat::expect_equal(is.na(new), is.na(stored), info = variable)
    ## stored values are rounded to 4 digits
    both_present = !is.na(new) & !is.na(stored)
    testthat::expect_lte(max(c(0, abs(new[both_present] - stored[both_present]))), 5.01e-5)
  })
})

####----Margins of error----####

testthat::test_that("percent and complement estimates and MOEs are NA for a zero-universe geography", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
  fixture = readRDS(test_data_path)
  codebook = attr(fixture, "codebook")

  insurance_columns = health_insurance_columns(fixture)
  race_columns = colnames(fixture) %>% stringr::str_subset("^race_")
  input_columns = c(insurance_columns, race_columns) %>%
    stringr::str_subset("_percent(_M)?$", negate = TRUE)

  ## Three real counties; the first has its counts set to zero to stand in for
  ## an unpopulated geography. Its MOEs are kept as published, since the ACS
  ## reports positive MOEs for zero estimates.
  input = fixture %>%
    dplyr::slice(1:3) %>%
    dplyr::select(GEOID, data_source_year, dplyr::all_of(input_columns))
  estimate_columns = stringr::str_subset(input_columns, "_M$", negate = TRUE)
  input[1, estimate_columns] = 0

  with_estimates = execute_definitions(
    input,
    c(get_table("health_insurance")[["definitions"]], get_table("race")[["definitions"]]))
  attr(with_estimates, "codebook") = codebook %>%
    dplyr::filter(calculated_variable %in% colnames(with_estimates))
  result = calculate_moes(with_estimates)

  pairs = list(
    c("health_insurance_coverage_status_covered_percent",
      "health_insurance_coverage_status_notcovered_percent"),
    c("race_nonhispanic_white_alone_percent", "race_personofcolor_percent"))

  purrr::walk(pairs, function(pair) {
    source = pair[1]
    complement = pair[2]

    ## zero-universe row: everything is NA (complement was 1 before this fix)
    testthat::expect_true(is.na(result[[source]][1]), info = source)
    testthat::expect_true(is.na(result[[complement]][1]), info = complement)
    testthat::expect_true(is.na(result[[paste0(source, "_M")]][1]), info = source)
    testthat::expect_true(is.na(result[[paste0(complement, "_M")]][1]), info = complement)

    ## populated rows: values match the published fixture, the complement is
    ## 1 - source, and the complement shares the source's MOE
    testthat::expect_equal(result[[source]][2:3], fixture[[source]][2:3], tolerance = 1e-4)
    testthat::expect_equal(result[[complement]][2:3], 1 - result[[source]][2:3], tolerance = 1e-4)
    testthat::expect_true(all(result[[paste0(source, "_M")]][2:3] > 0))
    testthat::expect_equal(
      result[[paste0(complement, "_M")]][2:3],
      result[[paste0(source, "_M")]][2:3])
  })

  ## the other zero-universe percents in the health insurance table are NA too
  testthat::expect_true(is.na(result$health_insurance_coverage_status_covered_employed_percent[1]))
  testthat::expect_true(is.na(result$health_insurance_coverage_status_covered_unemployed_percent[1]))
})

####----interpolate_acs()----####

## Five real counties assigned to three target geographies:
##   "populated": counties 1 and 2
##   "empty":     county 3, with all counts set to zero
##   "mixed":     county 4 plus county 5, with county 5's counts set to zero
zero_universe_interpolation_input = function() {
  fixture = readRDS(test_data_path)
  codebook = attr(fixture, "codebook")
  keep_variables = codebook$calculated_variable %>%
    stringr::str_subset("^(total_population|race_|median_household_income_universe_allraces$|area_)")
  keep_columns = intersect(c(keep_variables, paste0(keep_variables, "_M")), colnames(fixture))

  df = fixture %>%
    dplyr::slice(1:5) %>%
    dplyr::select(GEOID, data_source_year, dplyr::all_of(keep_columns)) %>%
    dplyr::mutate(target = c("populated", "populated", "empty", "mixed", "mixed"))
  count_columns = keep_columns %>%
    stringr::str_subset("^(total_population|race_)") %>%
    stringr::str_subset("_M$|_percent$", negate = TRUE)
  df[c(3, 5), count_columns] = 0
  ## recompute the source-level percents so they reflect the zeroed counts
  df = execute_definitions(df, get_table("race")[["definitions"]])

  attr(df, "codebook") = codebook %>% dplyr::filter(calculated_variable %in% keep_variables)
  attr(df, "resolved_tables") = c("total_population", "race")
  df
}

testthat::test_that("interpolate_acs() returns NA percents and complements for an all-zero target", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
  df = zero_universe_interpolation_input()

  result = suppressWarnings(interpolate_acs(df, target_geoid_column = "target", weight = NULL))
  empty = result %>% dplyr::filter(GEOID == "empty")
  populated = result %>% dplyr::filter(GEOID == "populated")

  testthat::expect_equal(empty$race_universe, 0)
  testthat::expect_true(is.na(empty$race_nonhispanic_white_alone_percent))
  testthat::expect_true(is.na(empty$race_personofcolor_percent))
  testthat::expect_true(is.na(empty$race_nonhispanic_white_alone_percent_M))
  testthat::expect_true(is.na(empty$race_personofcolor_percent_M))

  testthat::expect_false(is.na(populated$race_personofcolor_percent))
  testthat::expect_equal(
    populated$race_personofcolor_percent,
    1 - populated$race_nonhispanic_white_alone_percent)
})

testthat::test_that("interpolate_acs() ignores zero-universe sources when a target also has populated sources", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
  df = zero_universe_interpolation_input()

  result = suppressWarnings(interpolate_acs(df, target_geoid_column = "target", weight = NULL))
  mixed = result %>% dplyr::filter(GEOID == "mixed")

  ## the zeroed county adds nothing, so the target's percents equal county 4's
  county_four = df %>% dplyr::slice(4)
  testthat::expect_equal(mixed$race_universe, county_four$race_universe)
  testthat::expect_equal(
    mixed$race_nonhispanic_white_alone_percent,
    county_four$race_nonhispanic_white_alone_percent,
    tolerance = 1e-4)
  testthat::expect_equal(
    mixed$race_personofcolor_percent,
    county_four$race_personofcolor_percent,
    tolerance = 1e-4)
  testthat::expect_false(is.na(mixed$race_nonhispanic_white_alone_percent_M))
  testthat::expect_false(is.na(mixed$race_personofcolor_percent_M))
})

testthat::test_that("interpolate_acs() returns NA, not NaN, for weighted averages over zero population", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
  df = zero_universe_interpolation_input()

  result = suppressWarnings(interpolate_acs(df, target_geoid_column = "target", weight = NULL))
  empty = result %>% dplyr::filter(GEOID == "empty")
  populated = result %>% dplyr::filter(GEOID == "populated")

  testthat::expect_true(is.na(empty$median_household_income_universe_allraces))
  testthat::expect_false(is.nan(empty$median_household_income_universe_allraces))
  testthat::expect_false(is.na(populated$median_household_income_universe_allraces))
})

testthat::test_that("interpolate_acs() does not warn about NA percents in its input", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
  ## Percents are recomputed from summed counts, so NA percents from
  ## zero-universe source rows should not trigger the NA-propagation warning.
  df = zero_universe_interpolation_input()
  testthat::expect_true(any(is.na(df$race_personofcolor_percent)))

  testthat::expect_no_warning(
    interpolate_acs(df, target_geoid_column = "target", weight = NULL),
    message = "Input has NA values")
})

testthat::test_that("interpolate_acs() returns NA population density for a target with no land area", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")
  df = zero_universe_interpolation_input()
  ## a water-only geography: no land area and no population
  df$area_land_sq_kilometer[3] = 0
  df$area_land_water_sq_kilometer[3] = df$area_water_sq_kilometer[3]

  result = suppressWarnings(interpolate_acs(df, target_geoid_column = "target", weight = NULL))
  testthat::expect_true(is.na(result$population_density_land_sq_kilometer[result$GEOID == "empty"]))
  testthat::expect_true(all(result$population_density_land_sq_kilometer[result$GEOID != "empty"] > 0))
})

####----Live API----####

testthat::test_that("compile_acs_data() returns NA percents and complements for zero-population tracts", {
  skip_if_no_census_key()

  ## Rhode Island's 2022 tracts include four with no residents (largely
  ## water-only tracts)
  df = suppressMessages(suppressWarnings(compile_acs_data(
    tables = c("health_insurance", "race"),
    years = 2022,
    geography = "tract",
    states = "RI")))

  insurance_universe = df$health_insurance_coverage_status_type_by_employment_status_universe
  empty = insurance_universe == 0
  testthat::expect_gt(sum(empty), 0)

  testthat::expect_true(all(is.na(df$health_insurance_coverage_status_covered_percent[empty])))
  testthat::expect_true(all(is.na(df$health_insurance_coverage_status_notcovered_percent[empty])))
  testthat::expect_true(all(is.na(df$health_insurance_coverage_status_notcovered_percent_M[empty])))

  testthat::expect_false(any(is.na(df$health_insurance_coverage_status_notcovered_percent[!empty])))
  testthat::expect_equal(
    df$health_insurance_coverage_status_notcovered_percent[!empty],
    1 - df$health_insurance_coverage_status_covered_percent[!empty],
    tolerance = 1e-4)

  race_empty = df$race_universe == 0
  testthat::expect_true(all(is.na(df$race_personofcolor_percent[race_empty])))
  testthat::expect_false(any(is.na(df$race_personofcolor_percent[!race_empty])))
})
