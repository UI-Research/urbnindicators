####----DSL Validation Tests (no API calls)----####

test_that("is_dsl_definition identifies valid definitions", {
  expect_true(is_dsl_definition(define_percent("a", "b")))
  expect_true(is_dsl_definition(define_complement("y", output = "x")))
  expect_true(is_dsl_definition(define_metadata("x", definition = "desc")))
  expect_true(is_dsl_definition(define_percent(
    "^snap", denominator = "snap_universe", each = TRUE)))
  expect_true(is_dsl_definition(define_sum(
    "^snap", each = TRUE,
    add_replace = c("a" = "b"), output_replace = c("a" = "b"))))

  ## negative cases
  expect_false(is_dsl_definition("snap"))
  expect_false(is_dsl_definition(list(a = 1)))
  expect_false(is_dsl_definition(42))
  expect_false(is_dsl_definition(NULL))
  expect_false(is_dsl_definition(list(type = "unknown_type")))
})

####----validate_definition() Tests----####

test_that("validate_definition accepts valid simple_percent", {
  def = define_percent("x", "y")
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid complex", {
  def = define_percent(c("a", "b"), denominator = "c", output = "x_percent")
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid across_percent", {
  def = define_percent("^race_", denominator = "race_universe", each = TRUE)
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid across_sum", {
  def = define_sum("^male_", each = TRUE,
                   add_replace = c("male" = "female"),
                   output_replace = c("male_" = "total_"))
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid one_minus", {
  def = define_complement("x_percent", output = "not_x_percent")
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid metadata", {
  def = define_metadata("my_var", definition = "A description.")
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid sum", {
  def = define_sum(c("a", "b", "c"), output = "total")
  expect_true(validate_definition(def))
})

test_that("validate_definition rejects non-list", {
  expect_error(validate_definition("not a list"), "must be a list")
})

test_that("validate_definition rejects missing type", {
  expect_error(validate_definition(list(output = "x")), "invalid or missing `type`")
})

test_that("validate_definition rejects invalid type", {
  expect_error(validate_definition(list(type = "bogus")), "invalid or missing `type`")
})

test_that("validate_definition rejects simple_percent missing fields", {
  ## missing numerator
  expect_error(
    validate_definition(list(type = "simple_percent", output = "x", denominator = "d")),
    "numerator.*must be a non-empty string")
  ## missing output
  expect_error(
    validate_definition(list(type = "simple_percent", numerator = "n", denominator = "d")),
    "output.*must be a non-empty string")
})

test_that("validate_definition rejects complex missing numerator/denominator spec", {
  expect_error(
    validate_definition(list(type = "complex", output = "x", denominator_variables = c("d"))),
    "requires `numerator_variables` or `numerator_regex`")
  expect_error(
    validate_definition(list(type = "complex", output = "x", numerator_variables = c("n"))),
    "requires `denominator_variables` or `denominator_regex`")
})

test_that("validate_definition rejects across_percent missing denominator", {
  expect_error(
    validate_definition(list(type = "across_percent", input_regex = "^x", output_suffix = "_p")),
    "requires `denominator` or `denominator_function`")
})

test_that("validate_definition rejects across_percent with non-function denominator_function", {
  expect_error(
    validate_definition(list(type = "across_percent", input_regex = "^x",
                             output_suffix = "_p", denominator_function = "not_a_fn")),
    "denominator_function.*must be a function")
})

test_that("validate_definition rejects across_sum missing functions", {
  expect_error(
    validate_definition(list(type = "across_sum", input_regex = "^x",
                             addend_function = "not_fn", output_naming_function = identity)),
    "addend_function.*must be a function")
  expect_error(
    validate_definition(list(type = "across_sum", input_regex = "^x",
                             addend_function = identity, output_naming_function = "not_fn")),
    "output_naming_function.*must be a function")
})

test_that("validate_definition rejects invalid regex in across_percent", {
  expect_error(
    validate_definition(list(type = "across_percent", input_regex = "[invalid",
                             output_suffix = "_p", denominator = "x")),
    "not a valid regex")
})

test_that("validate_definition rejects one_minus missing source_variable", {
  expect_error(
    validate_definition(list(type = "one_minus", output = "x")),
    "source_variable.*must be a non-empty string")
})

test_that("validate_definition rejects metadata missing definition_text", {
  expect_error(
    validate_definition(list(type = "metadata", output = "x")),
    "definition_text.*must be a non-empty string")
})

test_that("validate_definition rejects sum missing columns", {
  expect_error(
    validate_definition(list(type = "sum", output = "x")),
    "requires `columns`")
})

####----define_percent() constructor tests----####

test_that("define_percent infers output name for simple case", {
  def = define_percent("snap_received", "snap_universe")
  expect_equal(def[["output"]], "snap_received_percent")
  expect_equal(def[["type"]], "complex")
})

test_that("define_percent produces complex type for vector numerator", {
  def = define_percent(c("a", "b"), denominator = "c", output = "x_percent")
  expect_equal(def[["type"]], "complex")
  expect_equal(def[["numerator_variables"]], c("a", "b"))
  expect_equal(def[["denominator_regex"]], "c")
})

test_that("define_percent produces complex type for regex numerator", {
  def = define_percent("age_(6[5-9]).*_years",
                       denominator = "universe",
                       output = "over_65_percent")
  expect_equal(def[["type"]], "complex")
  expect_equal(def[["numerator_regex"]], "age_(6[5-9]).*_years")
})

test_that("define_percent produces across_percent for each=TRUE", {
  def = define_percent("^race_", denominator = "race_universe", each = TRUE)
  expect_equal(def[["type"]], "across_percent")
  expect_equal(def[["input_regex"]], "^race_")
  expect_equal(def[["denominator"]], "race_universe")
  expect_equal(def[["output_suffix"]], "_percent")
})

test_that("define_percent handles denominator_replace with single replacement", {
  def = define_percent("poverty.*below",
                       denominator_replace = c("below" = "universe"),
                       each = TRUE)
  expect_equal(def[["type"]], "across_percent")
  expect_true(is.function(def[["denominator_function"]]))
  expect_equal(def[["denominator_function"]]("poverty_below_black"),
               "poverty_universe_black")
})

test_that("define_percent handles denominator_replace with multiple replacements", {
  def = define_percent("year_built_[0-9]",
                       denominator_replace = c("[0-9].*" = "universe", "built_" = ""),
                       each = TRUE)
  expect_equal(def[["denominator_function"]]("year_built_1940_1949"),
               "year_universe")
})

test_that("define_sum handles exclude parameter", {
  def = define_sum("^col_", each = TRUE,
                   add_replace = c("a" = "b"),
                   output_replace = c("a" = "b"),
                   exclude = "unwanted")
  expect_equal(def[["exclude_regex"]], "unwanted")
})

test_that("define_percent handles subtract_from_numerator (single string -> regex)", {
  def = define_percent(c("universe"), denominator = "universe",
                       subtract_from_numerator = "excluded",
                       output = "remainder_percent")
  expect_equal(def[["type"]], "complex")
  expect_equal(def[["numerator_subtract_regex"]], "excluded")
})

test_that("define_percent handles subtract_from_numerator (vector -> variables)", {
  def = define_percent(c("universe"), denominator = "universe",
                       subtract_from_numerator = c("a", "b"),
                       output = "remainder_percent")
  expect_equal(def[["type"]], "complex")
  expect_equal(def[["numerator_subtract_variables"]], c("a", "b"))
})

test_that("define_percent handles subtract_from_denominator (single string -> regex)", {
  def = define_percent("transport.*bike$",
                       denominator = "transport_universe",
                       subtract_from_denominator = "transport_wfh",
                       output = "bike_percent")
  expect_equal(def[["type"]], "complex")
  expect_equal(def[["subtract_regex"]], "transport_wfh")
})

test_that("define_percent handles subtract_from_denominator (vector -> variables)", {
  def = define_percent("some_col",
                       denominator = "some_denom",
                       subtract_from_denominator = c("a", "b"),
                       output = "result_percent")
  expect_equal(def[["type"]], "complex")
  expect_equal(def[["subtract_variables"]], c("a", "b"))
})

test_that("define_percent handles exclude parameter", {
  def = define_percent("^race_", denominator = "race_universe",
                       each = TRUE, exclude = "universe|percent")
  expect_equal(def[["exclude_regex"]], "universe|percent")
})

test_that("define_percent exclude in non-each mode goes to numerator_exclude_regex", {
  def = define_percent("some_pattern.*",
                       denominator = "denom",
                       output = "result_percent",
                       exclude = "unwanted")
  expect_equal(def[["numerator_exclude_regex"]], "unwanted")
})

test_that("execute_definition warns when each=TRUE matches only 1 column", {
  df = data.frame(race_white = 10, race_universe = 100, other_col = 50)
  def = define_percent("^race_white$", denominator = "race_universe", each = TRUE)
  expect_warning(
    execute_definition(df, def),
    "matched only 1 column")
})

####----define_sum() constructor tests----####

test_that("define_sum produces sum type for simple case", {
  def = define_sum(c("a", "b", "c"), output = "total")
  expect_equal(def[["type"]], "sum")
  expect_equal(def[["columns"]], c("a", "b", "c"))
  expect_equal(def[["output"]], "total")
})

test_that("define_sum produces across_sum for each=TRUE", {
  def = define_sum("female_.*years", each = TRUE,
                   add_replace = c("female" = "male"),
                   output_replace = c("female_" = "total_"))
  expect_equal(def[["type"]], "across_sum")
  expect_true(is.function(def[["addend_function"]]))
  expect_true(is.function(def[["output_naming_function"]]))
  expect_equal(def[["addend_function"]]("female_20_years"), "male_20_years")
  expect_equal(def[["output_naming_function"]]("female_20_years"), "total_20_years")
})

####----define_complement() constructor tests----####

test_that("define_complement produces one_minus type", {
  def = define_complement("x_percent", output = "not_x_percent")
  expect_equal(def[["type"]], "one_minus")
  expect_equal(def[["source_variable"]], "x_percent")
  expect_equal(def[["output"]], "not_x_percent")
})

####----define_metadata() constructor tests----####

test_that("define_metadata produces metadata type", {
  def = define_metadata("my_var", definition = "A description.")
  expect_equal(def[["type"]], "metadata")
  expect_equal(def[["definition_text"]], "A description.")
})

####----extract_explicit_variables() Tests----####

test_that("extract_explicit_variables for simple_percent", {
  ## auto_percent produces simple_percent with known column names
  def = list(type = "simple_percent", output = "a_percent", numerator = "a", denominator = "b")
  expect_equal(sort(extract_explicit_variables(def)), c("a", "b"))
})

test_that("extract_explicit_variables for complex with regex returns empty", {
  ## define_percent("a", "b") produces complex with regex — no explicit vars
  def = define_percent("a", "b")
  expect_equal(extract_explicit_variables(def), character(0))
})

test_that("extract_explicit_variables for complex with vector inputs", {
  def = define_percent(c("a", "b"), denominator = "c",
                       subtract_from_numerator = c("b", "d"),
                       subtract_from_denominator = c("e", "f"),
                       output = "x_percent")
  expect_equal(sort(extract_explicit_variables(def)), c("a", "b", "d", "e", "f"))
})

test_that("extract_explicit_variables for across_percent with fixed denominator", {
  def = define_percent("^race_", denominator = "race_universe", each = TRUE)
  expect_equal(extract_explicit_variables(def), "race_universe")
})

test_that("extract_explicit_variables for across_percent with denominator_replace", {
  def = define_percent("^race_",
                       denominator_replace = c("x" = "y"),
                       each = TRUE)
  ## function-based denominators can't be statically extracted
  expect_equal(extract_explicit_variables(def), character(0))
})

test_that("extract_explicit_variables for one_minus", {
  def = define_complement("x", output = "not_x")
  expect_equal(extract_explicit_variables(def), "x")
})

test_that("extract_explicit_variables for metadata returns empty", {
  def = define_metadata("my_var", definition = "desc")
  expect_equal(extract_explicit_variables(def), character(0))
})

test_that("extract_explicit_variables for across_sum returns empty", {
  def = define_sum("^x_", each = TRUE,
                   add_replace = c("a" = "b"), output_replace = c("a" = "b"))
  expect_equal(extract_explicit_variables(def), character(0))
})

test_that("extract_explicit_variables for sum returns columns", {
  def = define_sum(c("a", "b", "c"), output = "total")
  expect_equal(sort(extract_explicit_variables(def)), c("a", "b", "c"))
})

####----resolve_definition_variables() Tests----####

test_that("resolve_definition_variables resolves ACS codes in simple_percent", {
  ## auto_percent produces simple_percent with ACS codes that need resolution
  defs = list(list(type = "simple_percent", output = "x_pct",
                   numerator = "B22003_002", denominator = "B22003_001"))
  raw_vars = c(snap_universe_ = "B22003_001", snap_received_ = "B22003_002")

  resolved = resolve_definition_variables(defs, raw_vars)
  expect_equal(resolved[[1]][["numerator"]], "snap_received")
  expect_equal(resolved[[1]][["denominator"]], "snap_universe")
})

test_that("resolve_definition_variables resolves complex type numerator_variables", {
  defs = list(define_percent(c("B22003_001", "B22003_002"),
                             denominator = "B22003_001",
                             output = "x_pct"))
  raw_vars = c(snap_universe_ = "B22003_001", snap_received_ = "B22003_002")

  resolved = resolve_definition_variables(defs, raw_vars)
  expect_equal(resolved[[1]][["numerator_variables"]], c("snap_universe", "snap_received"))
})

test_that("resolve_definition_variables handles empty variables vector", {
  defs = list(list(type = "simple_percent", output = "a_percent",
                   numerator = "a", denominator = "b"))
  resolved = resolve_definition_variables(defs, character(0))
  expect_equal(resolved[[1]][["numerator"]], "a")
  expect_equal(resolved[[1]][["denominator"]], "b")
})

####----validate_definition_variables() Tests----####

test_that("validate_definition_variables passes when all variables exist", {
  defs = list(define_percent(c("a", "b"), denominator = "c", output = "x_pct"))
  expect_true(validate_definition_variables(defs, c("a", "b", "c")))
})

test_that("validate_definition_variables errors on missing variables", {
  defs = list(define_percent(c("a", "missing_var"),
                             denominator = c("c", "d"),
                             output = "x_pct"))
  expect_error(
    validate_definition_variables(defs, c("a", "c", "d")),
    "missing_var")
})

####----check_multi_table_variables() Tests----####

test_that("check_multi_table_variables warns when variables span tables", {
  defs = list(define_percent(c("snap_universe", "public_assistance_universe"),
                             denominator = "snap_universe",
                             output = "x_pct"))
  expect_warning(
    check_multi_table_variables(defs,
                                resolved_tables = c("snap", "public_assistance"),
                                auto_table_entries = list()),
    "multiple ACS tables")
})

test_that("check_multi_table_variables does not warn for single-table variables", {
  defs = list(list(type = "simple_percent", output = "x_pct",
                   numerator = "snap_received", denominator = "snap_universe"))
  expect_silent(
    check_multi_table_variables(defs,
                                resolved_tables = c("snap"),
                                auto_table_entries = list()))
})

####----execute_definition() Tests----####

test_that("execute_definition handles sum type", {
  df = data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
  def = define_sum(c("a", "b", "c"), output = "total")
  result = execute_definition(df, def)
  expect_equal(result$total, c(12, 15, 18))
})
