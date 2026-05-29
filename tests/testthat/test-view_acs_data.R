test_data_path = test_path("test-data", "test_data_2025-11-06.rds")

testthat::test_that("view_acs_data() returns a shinyApp from spatial output", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("mapgl")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")

  df1 = readRDS(test_data_path)
  app1 = view_acs_data(df1)
  testthat::expect_s3_class(app1, "shiny.appobj")
})

testthat::test_that("view_acs_data() errors on non-sf input", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("mapgl")
  testthat::skip_if_not_installed("bslib")

  df1 = tibble::tibble(GEOID = "1", NAME = "A", x = 1)
  testthat::expect_error(view_acs_data(df1), "sf")
})

testthat::test_that("view_acs_data() errors when codebook attribute is missing", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("mapgl")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")

  df1 = readRDS(test_data_path)
  attr(df1, "codebook") = NULL
  testthat::expect_error(view_acs_data(df1), "codebook")
})

testthat::test_that("view_acs_data() errors when zero-row input is passed", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("mapgl")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")

  df1 = readRDS(test_data_path)
  df_empty = df1[0, , drop = FALSE]
  testthat::expect_error(view_acs_data(df_empty), "zero rows")
})

testthat::test_that("build_variable_choices() filters to eligible vars and excludes _M cols", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")

  df1 = readRDS(test_data_path)
  codebook1 = attr(df1, "codebook")
  choices1 = build_variable_choices(df1, codebook1, variables = NULL)

  testthat::expect_gt(length(choices1), 0)
  ## values are raw column names, all present in the data
  testthat::expect_true(all(unname(choices1) %in% colnames(df1)))
  ## no MOE columns in the picker
  testthat::expect_false(any(stringr::str_detect(unname(choices1), "_M$")))
  ## no metadata columns either
  testthat::expect_false(any(c("GEOID", "NAME", "data_source_year") %in% unname(choices1)))
})

testthat::test_that("build_variable_choices() honors the `variables` argument", {
  testthat::skip_if_not(file.exists(test_data_path), "Test fixture not available")

  df1 = readRDS(test_data_path)
  codebook1 = attr(df1, "codebook")

  numeric_cols = codebook1 %>%
    dplyr::filter(
      variable_type %in% c("Count", "Percent", "Sum", "Median",
                           "Median ($)", "Average", "Index"),
      calculated_variable %in% colnames(df1),
      !stringr::str_detect(calculated_variable, "_M$")) %>%
    dplyr::pull(calculated_variable)

  testthat::skip_if(length(numeric_cols) < 2, "Not enough vars in fixture for test.")
  pick1 = numeric_cols[1:2]

  ## restrict by raw column names
  choices_raw = build_variable_choices(df1, codebook1, variables = pick1)
  testthat::expect_setequal(unname(choices_raw), pick1)

  ## restrict by pretty labels
  pretty1 = make_pretty_names(pick1)
  choices_pretty = build_variable_choices(df1, codebook1, variables = pretty1)
  testthat::expect_setequal(unname(choices_pretty), pick1)
})

testthat::test_that("palette_for_type() returns urbnthemes-derived palettes per type", {
  pct1 = palette_for_type("Percent")
  testthat::expect_equal(pct1$fmt, "percent")
  testthat::expect_equal(pct1$palette, .urbn_palettes$quintile)

  dol1 = palette_for_type("Median ($)")
  testthat::expect_equal(dol1$fmt, "dollar")
  testthat::expect_equal(dol1$palette, .urbn_palettes$cyan)

  cnt1 = palette_for_type("Count")
  testthat::expect_equal(cnt1$fmt, "comma")
  testthat::expect_equal(cnt1$palette, .urbn_palettes$cyan)

  idx1 = palette_for_type("Index")
  testthat::expect_equal(idx1$palette, .urbn_palettes$diverging)
})

testthat::test_that("make_color_stops() spans the given range with palette length", {
  pal1 = .urbn_palettes$cyan
  stops1 = make_color_stops(c(0, 10), pal1)
  testthat::expect_equal(length(stops1$stops), length(pal1))
  testthat::expect_equal(stops1$stops[1], 0)
  testthat::expect_equal(stops1$stops[length(pal1)], 10)

  ## handles degenerate range without producing zero-width
  stops_const = make_color_stops(c(3, 3), pal1)
  testthat::expect_true(stops_const$stops[length(pal1)] > stops_const$stops[1])

  ## non-finite or wrong-length input returns NULL
  testthat::expect_null(make_color_stops(c(NA_real_, NA_real_), pal1))
  testthat::expect_null(make_color_stops(c(0, 1, 2), pal1))
})

testthat::test_that("default_range_for_variable() defaults to c(0,1) for Percent vars", {
  rng1 = default_range_for_variable(c(0.1, 0.4, 0.7), "Percent")
  testthat::expect_equal(rng1, c(0, 1))
})

testthat::test_that("default_range_for_variable() uses observed range for non-percent vars", {
  rng1 = default_range_for_variable(c(10, 50, 200, NA), "Count")
  testthat::expect_equal(rng1, c(10, 200))

  rng_med = default_range_for_variable(c(35000, 80000), "Median ($)")
  testthat::expect_equal(rng_med, c(35000, 80000))
})

testthat::test_that("default_range_for_variable() handles all-NA / constant input", {
  rng_na = default_range_for_variable(c(NA_real_, NA_real_), "Count")
  testthat::expect_equal(rng_na, c(0, 1))

  rng_const = default_range_for_variable(c(5, 5, 5), "Count")
  testthat::expect_true(rng_const[2] > rng_const[1])
})

testthat::test_that("out_of_range_indices() identifies values outside the range", {
  vals1 = c(0.1, 0.5, 0.9, NA, -0.1, 1.2)
  out1 = out_of_range_indices(vals1, c(0, 1))
  testthat::expect_equal(out1, c(5, 6))

  ## NA never flagged
  out_na = out_of_range_indices(c(NA, NA), c(0, 1))
  testthat::expect_length(out_na, 0)

  ## empty range = nothing flagged
  out_bad = out_of_range_indices(c(1, 2, 3), c(NA, NA))
  testthat::expect_length(out_bad, 0)
})

testthat::test_that("make_popup_html() adds an out-of-range note for flagged rows", {
  df1 = data.frame(
    NAME = c("A", "B", "C"),
    GEOID = c("1", "2", "3"),
    x = c(0.1, 0.5, 1.2),
    stringsAsFactors = FALSE)

  popups_all_in = make_popup_html(df1, "x", NULL, "percent", "X", out_of_range_idx = integer(0))
  testthat::expect_false(any(stringr::str_detect(popups_all_in, "Outside selected range")))

  popups_one_out = make_popup_html(df1, "x", NULL, "percent", "X", out_of_range_idx = 3L)
  testthat::expect_false(stringr::str_detect(popups_one_out[1], "Outside selected range"))
  testthat::expect_false(stringr::str_detect(popups_one_out[2], "Outside selected range"))
  testthat::expect_true(stringr::str_detect(popups_one_out[3],  "Outside selected range"))
})

testthat::test_that("format_value() formats by type", {
  testthat::skip_if_not_installed("scales")
  testthat::expect_match(format_value(0.123, "percent"), "%")
  testthat::expect_match(format_value(1234, "dollar"), "\\$")
  testthat::expect_match(format_value(1234, "comma"), ",")
})

testthat::test_that("benchmark_levels_for_geography() returns appropriate options", {
  testthat::expect_setequal(
    names(benchmark_levels_for_geography("tract")),
    c("None", "County", "State"))
  testthat::expect_setequal(
    names(benchmark_levels_for_geography("county")),
    c("None", "State"))
  testthat::expect_setequal(
    names(benchmark_levels_for_geography("state")),
    c("None", "National"))

  ## Unsupported / NULL / NA → empty (no dropdown)
  testthat::expect_length(benchmark_levels_for_geography(NULL),    0)
  testthat::expect_length(benchmark_levels_for_geography(NA),      0)
  testthat::expect_length(benchmark_levels_for_geography("zcta"),  0)
  testthat::expect_length(benchmark_levels_for_geography("block group"), 0)

  ## "None" always sits first so it's the default selection.
  tract_opts = benchmark_levels_for_geography("tract")
  testthat::expect_equal(names(tract_opts)[1], "None")
  testthat::expect_equal(unname(tract_opts)[1], "none")
})

testthat::test_that("parent_geoid_length_for_level() returns the right substring length", {
  testthat::expect_equal(parent_geoid_length_for_level("county"),   5L)
  testthat::expect_equal(parent_geoid_length_for_level("state"),    2L)
  testthat::expect_true(is.na(parent_geoid_length_for_level("national")))
  testthat::expect_true(is.na(parent_geoid_length_for_level("metro")))
})

testthat::test_that("assign_parent_geoids() extracts the parent prefix or 'US' for national", {
  tract_geoids = c("34001011901", "34001001900", "06037980031")

  testthat::expect_equal(
    assign_parent_geoids(tract_geoids, "county"),
    c("34001", "34001", "06037"))

  testthat::expect_equal(
    assign_parent_geoids(tract_geoids, "state"),
    c("34", "34", "06"))

  testthat::expect_equal(
    assign_parent_geoids(c("34", "06", "48"), "national"),
    c("US", "US", "US"))

  testthat::expect_length(assign_parent_geoids(character(0), "county"), 0)
})

testthat::test_that("assign_parent_geoids() uses lookup when supplied", {
  lookup1 = tibble::tibble(
    GEOID        = c("N1", "N2", "N3", "N1", "N2", "N3"),
    level        = c("county", "county", "county", "state", "state", "state"),
    parent_geoid = c("34001", "34003", "34005", "34", "34", "34"))

  testthat::expect_equal(
    assign_parent_geoids(c("N1", "N3", "N2"), "county", lookup = lookup1),
    c("34001", "34005", "34003"))

  testthat::expect_equal(
    assign_parent_geoids(c("N1", "N3", "N2"), "state", lookup = lookup1),
    c("34", "34", "34"))

  ## Unknown ids return NA via match()
  testthat::expect_true(is.na(
    assign_parent_geoids("N99", "county", lookup = lookup1)))
})

testthat::test_that("build_spatial_crosswalk() produces normalized shares", {
  testthat::skip_if_not_installed("sf")

  ## Two source squares side by side; target is a single rectangle that
  ## covers the right half of the left source and all of the right source.
  src_polys = list(
    sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)))),
    sf::st_polygon(list(rbind(c(10, 0), c(20, 0), c(20, 10), c(10, 10), c(10, 0)))))
  src1 = sf::st_sf(
    GEOID    = c("S1", "S2"),
    geometry = sf::st_sfc(src_polys, crs = 5070))

  tgt_poly = sf::st_polygon(list(rbind(c(5, 0), c(20, 0), c(20, 10),
                                       c(5, 10), c(5, 0))))
  tgt1 = sf::st_sf(
    GEOID    = "T1",
    geometry = sf::st_sfc(list(tgt_poly), crs = 5070))

  ## Capture the partial-coverage warning so the test stays quiet.
  xw1 = suppressWarnings(build_spatial_crosswalk(src1, tgt1))

  ## Both sources map only to T1, so shares renormalize to 1.
  testthat::expect_setequal(unique(xw1$target_geoid), "T1")
  testthat::expect_equal(sort(xw1$source_geoid), c("S1", "S2"))
  testthat::expect_equal(xw1$share, c(1, 1))
  testthat::expect_true(all(xw1$intersection_area > 0))
})

testthat::test_that("build_spatial_crosswalk() errors when target lacks GEOID", {
  testthat::skip_if_not_installed("sf")

  poly1 = sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  src1 = sf::st_sf(GEOID = "A",
                   geometry = sf::st_sfc(list(poly1), crs = 5070))
  tgt1 = sf::st_sf(name = "B",
                   geometry = sf::st_sfc(list(poly1), crs = 5070))

  testthat::expect_error(build_spatial_crosswalk(src1, tgt1), "GEOID")
})

testthat::test_that("compute_target_parent_map() picks the majority-area parent", {
  ## T1 receives more area from sources in county 34001 than 34003,
  ## T2 receives more area from 34003 than 34001.
  crosswalk1 = tibble::tibble(
    source_geoid      = c("34001011901", "34003012301", "34001011902",
                          "34003012302"),
    target_geoid      = c("T1", "T1", "T2", "T2"),
    intersection_area = c(100, 25, 10, 80),
    share             = c(1, 1, 1, 1))

  pm1 = compute_target_parent_map(
    crosswalk1,
    benchmark_levels = c("None" = "none", "County" = "county", "State" = "state"))

  county_rows = pm1[pm1$level == "county", ]
  testthat::expect_equal(
    county_rows$parent_geoid[match(c("T1", "T2"), county_rows$GEOID)],
    c("34001", "34003"))

  state_rows = pm1[pm1$level == "state", ]
  testthat::expect_equal(
    sort(unique(state_rows$parent_geoid)),
    "34")
})

testthat::test_that("compute_target_parent_map() returns NULL with no benchmark levels", {
  xw1 = tibble::tibble(
    source_geoid = "A", target_geoid = "B",
    intersection_area = 1, share = 1)
  testthat::expect_null(compute_target_parent_map(xw1, character(0)))
  testthat::expect_null(compute_target_parent_map(xw1, c("None" = "none")))
})

testthat::test_that("classify_significance() returns Larger/Smaller/Not significant", {
  testthat::skip_if_not_installed("tidycensus")
  ## Construct cases where significance is unambiguous given the MOEs.
  est1 = c(0.50, 0.10, 0.30, NA,   0.40)
  moe1 = c(0.02, 0.02, 0.02, 0.02, 0.02)
  est2 = c(0.30, 0.30, 0.31, 0.30, NA)
  moe2 = c(0.02, 0.02, 0.02, 0.02, 0.02)

  out1 = classify_significance(est1, moe1, est2, moe2, clevel = 0.9)

  testthat::expect_equal(out1[1], "Larger")
  testthat::expect_equal(out1[2], "Smaller")
  testthat::expect_equal(out1[3], "Not significant")
  testthat::expect_true(is.na(out1[4]))
  testthat::expect_true(is.na(out1[5]))
})

testthat::test_that("classify_significance() returns all-NA when MOEs are missing", {
  out1 = classify_significance(
    est1 = c(0.5, 0.3),
    moe1 = c(NA_real_, NA_real_),
    est2 = c(0.4, 0.4),
    moe2 = c(0.01, 0.01))
  testthat::expect_true(all(is.na(out1)))
})

testthat::test_that("make_popup_html() includes benchmark info when supplied", {
  df1 = data.frame(
    NAME = c("A", "B"),
    GEOID = c("1", "2"),
    x = c(0.5, 0.2),
    stringsAsFactors = FALSE)

  bm1 = list(
    values   = c(0.3, 0.3),
    moes     = c(0.02, 0.02),
    category = c("Larger", "Not significant"),
    label    = "County")

  popups = make_popup_html(df1, "x", NULL, "percent", "X",
                           out_of_range_idx = integer(0),
                           benchmark = bm1)

  testthat::expect_true(stringr::str_detect(popups[1], "vs. County"))
  testthat::expect_true(stringr::str_detect(popups[1], "significantly larger"))
  testthat::expect_true(stringr::str_detect(popups[2], "not significantly different"))
})
