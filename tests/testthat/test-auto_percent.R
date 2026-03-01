####----UNIT TESTS (no API calls)----####

test_that("is_raw_acs_code identifies valid ACS table codes", {
  ## positive cases
  expect_true(is_raw_acs_code("B25070"))
  expect_true(is_raw_acs_code("B01001"))
  expect_true(is_raw_acs_code("C15002"))
  expect_true(is_raw_acs_code("B01001A"))
  expect_true(is_raw_acs_code("B01001I"))
  expect_true(is_raw_acs_code("B01001APR"))

  ## negative cases
  expect_false(is_raw_acs_code("race"))
  expect_false(is_raw_acs_code("snap"))
  expect_false(is_raw_acs_code("B2507"))   ## too few digits
  expect_false(is_raw_acs_code("B250700")) ## too many digits
  expect_false(is_raw_acs_code("D25070"))  ## wrong prefix
  expect_false(is_raw_acs_code("B25070_001")) ## variable code, not table code
  expect_false(is_raw_acs_code("b25070"))  ## lowercase
})

test_that("build_label_tree correctly assigns parent-child relationships", {
  ## mock a minimal variables_df for B22003 (SNAP receipt)
  mock_df = data.frame(
    name = c("B22003_001", "B22003_002", "B22003_003", "B22003_004",
             "B22003_005", "B22003_006", "B22003_007"),
    label = c(
      "Estimate!!Total:",
      "Estimate!!Total:!!Received Food Stamps/SNAP in the past 12 months:",
      "Estimate!!Total:!!Received Food Stamps/SNAP in the past 12 months:!!Household income in the past 12 months below poverty level",
      "Estimate!!Total:!!Received Food Stamps/SNAP in the past 12 months:!!Household income in the past 12 months at or above poverty level",
      "Estimate!!Total:!!Did not receive Food Stamps/SNAP in the past 12 months:",
      "Estimate!!Total:!!Did not receive Food Stamps/SNAP in the past 12 months:!!Household income in the past 12 months below poverty level",
      "Estimate!!Total:!!Did not receive Food Stamps/SNAP in the past 12 months:!!Household income in the past 12 months at or above poverty level"
    ),
    concept = rep("RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY POVERTY STATUS IN THE PAST 12 MONTHS FOR HOUSEHOLDS", 7),
    stringsAsFactors = FALSE
  )

  ## apply clean_acs_names (requires the package function)
  mock_df = mock_df %>% clean_acs_names()
  result = build_label_tree(mock_df)

  ## total has no parent
  expect_true(result$is_total[1])
  expect_true(is.na(result$parent_code[1]))

  ## subtotals (received, did not receive) should have total as parent
  expect_equal(result$parent_code[2], "B22003_001")
  expect_equal(result$parent_code[5], "B22003_001")

  ## leaves should have their subtotal as parent
  expect_equal(result$parent_code[3], "B22003_002")
  expect_equal(result$parent_code[4], "B22003_002")
  expect_equal(result$parent_code[6], "B22003_005")
  expect_equal(result$parent_code[7], "B22003_005")
})

test_that("classify_acs_table correctly identifies count vs skip tables", {
  ## count table: has a "Total:" label
  count_nodes = data.frame(
    concept = "RECEIPT OF FOOD STAMPS/SNAP",
    label = c("Estimate!!Total:", "Estimate!!Total:!!Received"),
    is_total = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  expect_equal(classify_acs_table(count_nodes), "count")

  ## median table
  median_nodes = data.frame(
    concept = "MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS",
    label = c("Estimate!!Median household income in the past 12 months (in 2022 inflation-adjusted dollars)"),
    is_total = c(TRUE),
    stringsAsFactors = FALSE
  )
  expect_equal(classify_acs_table(median_nodes), "skip")

  ## aggregate table
  aggregate_nodes = data.frame(
    concept = "AGGREGATE HOUSEHOLD INCOME",
    label = c("Estimate!!Aggregate household income in the past 12 months (in 2022 inflation-adjusted dollars)"),
    is_total = c(TRUE),
    stringsAsFactors = FALSE
  )
  expect_equal(classify_acs_table(aggregate_nodes), "skip")

  ## singleton table (only 1 variable)
  singleton_nodes = data.frame(
    concept = "GINI INDEX OF INCOME INEQUALITY",
    label = c("Estimate!!Gini Index"),
    is_total = c(TRUE),
    stringsAsFactors = FALSE
  )
  expect_equal(classify_acs_table(singleton_nodes), "skip")
})

test_that("generate_auto_definitions produces correct definitions with parent mode", {
  ## mock nodes with parent info
  mock_nodes = data.frame(
    name = c("B22003_001", "B22003_002", "B22003_003"),
    clean_name_trimmed = c("snap_universe", "snap_received", "snap_below_poverty"),
    is_total = c(TRUE, FALSE, FALSE),
    parent_code = c(NA, "B22003_001", "B22003_002"),
    parent_clean_name = c(NA, "snap_universe", "snap_received"),
    stringsAsFactors = FALSE
  )

  defs = generate_auto_definitions(mock_nodes, denominator_mode = "parent")

  expect_length(defs, 2)
  expect_equal(defs[[1]][["type"]], "simple_percent")
  expect_equal(defs[[1]][["output"]], "snap_received_percent")
  expect_equal(defs[[1]][["numerator"]], "snap_received")
  expect_equal(defs[[1]][["denominator"]], "snap_universe") ## parent of snap_received

  expect_equal(defs[[2]][["output"]], "snap_below_poverty_percent")
  expect_equal(defs[[2]][["denominator"]], "snap_received") ## parent of snap_below_poverty
})

test_that("generate_auto_definitions produces correct definitions with total mode", {
  mock_nodes = data.frame(
    name = c("B22003_001", "B22003_002", "B22003_003"),
    clean_name_trimmed = c("snap_universe", "snap_received", "snap_below_poverty"),
    is_total = c(TRUE, FALSE, FALSE),
    parent_code = c(NA, "B22003_001", "B22003_002"),
    parent_clean_name = c(NA, "snap_universe", "snap_received"),
    stringsAsFactors = FALSE
  )

  defs = generate_auto_definitions(mock_nodes, denominator_mode = "total")

  expect_length(defs, 2)
  ## all denominators should be the total
  expect_equal(defs[[1]][["denominator"]], "snap_universe")
  expect_equal(defs[[2]][["denominator"]], "snap_universe")
})

####----INTEGRATION TESTS (require Census API key)----####

test_that("compile_acs_data works with an unregistered ACS table", {
  skip_on_cran()
  skip_if_not(nchar(Sys.getenv("CENSUS_API_KEY")) > 0, "Census API key not available")

  ## B25070: Gross Rent as a Percentage of Household Income (not registered)
  result = compile_acs_data(
    tables = "B25070",
    years = 2022,
    geography = "state",
    states = "DC")

  ## should have auto-computed percentage columns
  pct_cols = grep("_percent$", colnames(result), value = TRUE)
  expect_true(length(pct_cols) > 0)

  ## computed percentage columns should be 0-1 bounded
  for (col in pct_cols) {
    vals = result[[col]]
    expect_true(all(vals >= 0 & vals <= 1, na.rm = TRUE),
                info = paste0(col, " has values outside [0, 1]"))
  }

  ## should have codebook
  codebook = attr(result, "codebook")
  expect_true(is.data.frame(codebook))
  expect_true(nrow(codebook) > 0)

  ## should have MOE columns but not SE/CV columns
  expect_true(any(grepl("_M$", colnames(result))))
  expect_false(any(grepl("_SE$", colnames(result))))
  expect_false(any(grepl("_CV$", colnames(result))))
})

test_that("compile_acs_data works with mixed registry + unregistered tables", {
  skip_on_cran()
  skip_if_not(nchar(Sys.getenv("CENSUS_API_KEY")) > 0, "Census API key not available")

  result = compile_acs_data(
    tables = c("snap", "B25070"),
    years = 2022,
    geography = "state",
    states = "DC")

  ## should have both snap and auto variables
  expect_true("snap_received_percent" %in% colnames(result))
  ## should have auto-computed B25070 percentage variables
  auto_pct = grep("gross_rent.*_percent$", colnames(result), value = TRUE)
  expect_true(length(auto_pct) > 0)
})

test_that("compile_acs_data with denominator = 'total' uses table total", {
  skip_on_cran()
  skip_if_not(nchar(Sys.getenv("CENSUS_API_KEY")) > 0, "Census API key not available")

  ## B25070 is not registered, so denominator param fully controls it
  result = compile_acs_data(
    tables = "B25070",
    denominator = "total",
    years = 2022,
    geography = "state",
    states = "DC")

  ## verify codebook: all auto-table percent definitions should use the _001 total
  codebook = attr(result, "codebook")
  auto_pct_rows = codebook %>%
    dplyr::filter(variable_type == "Percent",
                  !grepl("total_population", calculated_variable))

  expect_true(nrow(auto_pct_rows) > 0)

  ## all definitions should contain the universe/_001 variable as denominator
  for (i in seq_len(nrow(auto_pct_rows))) {
    def = auto_pct_rows$definition[i]
    expect_true(grepl("_001\\)", def) || grepl("universe", def),
                info = paste0("Definition for ", auto_pct_rows$calculated_variable[i],
                              " does not use total as denominator: ", def))
  }
})

test_that("compile_acs_data handles median tables silently (no percentages)", {
  skip_on_cran()
  skip_if_not(nchar(Sys.getenv("CENSUS_API_KEY")) > 0, "Census API key not available")

  ## B19013: Median household income - should return raw variables, no percentages
  result = compile_acs_data(
    tables = "B19013",
    years = 2022,
    geography = "state",
    states = "DC")

  ## should have raw variables but no auto-computed percent columns for this table
  auto_pct = grep("median.*household.*income.*_percent$", colnames(result), value = TRUE)
  expect_length(auto_pct, 0)
})

test_that("compile_acs_data works with race-iterated table", {
  skip_on_cran()
  skip_if_not(nchar(Sys.getenv("CENSUS_API_KEY")) > 0, "Census API key not available")

  ## B06007: Place of birth by language spoken at home — not registered, has hierarchy
  result = compile_acs_data(
    tables = "B06007",
    years = 2022,
    geography = "state",
    states = "DC")

  pct_cols = grep("_percent$", colnames(result), value = TRUE)
  expect_true(length(pct_cols) > 0)

  ## percentages should be 0-1 bounded
  for (col in pct_cols) {
    vals = result[[col]]
    expect_true(all(vals >= 0 & vals <= 1, na.rm = TRUE),
                info = paste0(col, " has values outside [0, 1]"))
  }
})

test_that("compile_acs_data detects overlap with registered tables", {
  skip_on_cran()
  skip_if_not(nchar(Sys.getenv("CENSUS_API_KEY")) > 0, "Census API key not available")

  ## B22003 is the ACS table behind the registered "snap" table
  ## passing the raw code should silently use the registered version
  result = compile_acs_data(
    tables = c("B22003"),
    years = 2022,
    geography = "state",
    states = "DC")

  ## should have the registered snap variable
  expect_true("snap_received_percent" %in% colnames(result))
})
