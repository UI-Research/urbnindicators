####----DSL Validation Tests (no API calls)----####

test_that("is_dsl_definition identifies valid definitions", {
  expect_true(is_dsl_definition(define_percent("x", numerator = "a", denominator = "b")))
  expect_true(is_dsl_definition(define_one_minus("x", source_variable = "y")))
  expect_true(is_dsl_definition(define_metadata("x", definition_text = "desc")))
  expect_true(is_dsl_definition(define_across_percent(
    input_regex = "^snap", output_suffix = "_percent", denominator = "snap_universe")))
  expect_true(is_dsl_definition(define_across_sum(
    input_regex = "^snap", addend_function = identity, output_naming_function = identity)))

  ## negative cases
  expect_false(is_dsl_definition("snap"))
  expect_false(is_dsl_definition(list(a = 1)))
  expect_false(is_dsl_definition(42))
  expect_false(is_dsl_definition(NULL))
  expect_false(is_dsl_definition(list(type = "unknown_type")))
})

####----validate_definition() Tests----####

test_that("validate_definition accepts valid simple_percent", {
  def = define_percent("x_percent", numerator = "x", denominator = "y")
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid complex", {
  def = define_percent("x_percent",
                       numerator_variables = c("a", "b"),
                       denominator_variables = c("c"))
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid across_percent", {
  def = define_across_percent(
    input_regex = "^race_",
    output_suffix = "_percent",
    denominator = "race_universe")
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid across_sum", {
  def = define_across_sum(
    input_regex = "^male_",
    addend_function = function(x) sub("male", "female", x),
    output_naming_function = function(x) sub("male_", "total_", x))
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid one_minus", {
  def = define_one_minus("not_x_percent", source_variable = "x_percent")
  expect_true(validate_definition(def))
})

test_that("validate_definition accepts valid metadata", {
  def = define_metadata("my_var", definition_text = "A description.")
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

####----extract_explicit_variables() Tests----####

test_that("extract_explicit_variables for simple_percent", {
  def = define_percent("x_pct", numerator = "a", denominator = "b")
  expect_equal(sort(extract_explicit_variables(def)), c("a", "b"))
})

test_that("extract_explicit_variables for complex", {
  def = define_percent("x_pct",
                       numerator_variables = c("a", "b"),
                       numerator_subtract_variables = c("b"),
                       denominator_variables = c("c"),
                       subtract_variables = c("d"))
  expect_equal(sort(extract_explicit_variables(def)), c("a", "b", "c", "d"))
})

test_that("extract_explicit_variables for across_percent with fixed denominator", {
  def = define_across_percent(
    input_regex = "^race_", output_suffix = "_pct", denominator = "race_universe")
  expect_equal(extract_explicit_variables(def), "race_universe")
})

test_that("extract_explicit_variables for across_percent with function denominator", {
  def = define_across_percent(
    input_regex = "^race_", output_suffix = "_pct",
    denominator_function = function(x) "race_universe")
  ## function-based denominators can't be statically extracted
  expect_equal(extract_explicit_variables(def), character(0))
})

test_that("extract_explicit_variables for one_minus", {
  def = define_one_minus("not_x", source_variable = "x")
  expect_equal(extract_explicit_variables(def), "x")
})

test_that("extract_explicit_variables for metadata returns empty", {
  def = define_metadata("my_var", definition_text = "desc")
  expect_equal(extract_explicit_variables(def), character(0))
})

test_that("extract_explicit_variables for across_sum returns empty", {
  def = define_across_sum(
    input_regex = "^x_", addend_function = identity, output_naming_function = identity)
  expect_equal(extract_explicit_variables(def), character(0))
})

####----resolve_definition_variables() Tests----####

test_that("resolve_definition_variables resolves ACS codes to clean names", {
  defs = list(
    define_percent("x_pct", numerator = "B22003_002", denominator = "B22003_001"))
  raw_vars = c(snap_universe_ = "B22003_001", snap_received_ = "B22003_002")

  resolved = resolve_definition_variables(defs, raw_vars)
  expect_equal(resolved[[1]][["numerator"]], "snap_received")
  expect_equal(resolved[[1]][["denominator"]], "snap_universe")
})

test_that("resolve_definition_variables leaves clean names unchanged", {
  defs = list(
    define_percent("x_pct", numerator = "snap_received", denominator = "snap_universe"))
  raw_vars = c(snap_universe_ = "B22003_001", snap_received_ = "B22003_002")

  resolved = resolve_definition_variables(defs, raw_vars)
  expect_equal(resolved[[1]][["numerator"]], "snap_received")
  expect_equal(resolved[[1]][["denominator"]], "snap_universe")
})

test_that("resolve_definition_variables handles empty variables vector", {
  defs = list(define_percent("x_pct", numerator = "a", denominator = "b"))
  resolved = resolve_definition_variables(defs, character(0))
  expect_equal(resolved[[1]][["numerator"]], "a")
  expect_equal(resolved[[1]][["denominator"]], "b")
})

test_that("resolve_definition_variables resolves complex type variables", {
  defs = list(define_percent("x_pct",
                             numerator_variables = c("B22003_001", "B22003_002"),
                             denominator_variables = c("B22003_001")))
  raw_vars = c(snap_universe_ = "B22003_001", snap_received_ = "B22003_002")

  resolved = resolve_definition_variables(defs, raw_vars)
  expect_equal(resolved[[1]][["numerator_variables"]], c("snap_universe", "snap_received"))
  expect_equal(resolved[[1]][["denominator_variables"]], c("snap_universe"))
})

####----validate_definition_variables() Tests----####

test_that("validate_definition_variables passes when all variables exist", {
  defs = list(define_percent("x_pct", numerator = "a", denominator = "b"))
  expect_true(validate_definition_variables(defs, c("a", "b", "c")))
})

test_that("validate_definition_variables errors on missing variables", {
  defs = list(define_percent("x_pct", numerator = "a", denominator = "missing_var"))
  expect_error(
    validate_definition_variables(defs, c("a", "b", "c")),
    "missing_var")
})

####----check_multi_table_variables() Tests----####

test_that("check_multi_table_variables warns when variables span tables", {
  defs = list(define_percent("x_pct",
                             numerator_variables = c("snap_universe", "public_assistance_universe"),
                             denominator_variables = c("snap_universe")))
  expect_warning(
    check_multi_table_variables(defs,
                                resolved_tables = c("snap", "public_assistance"),
                                auto_table_entries = list()),
    "multiple ACS tables")
})

test_that("check_multi_table_variables does not warn for single-table variables", {
  defs = list(define_percent("x_pct", numerator = "snap_received", denominator = "snap_universe"))
  expect_silent(
    check_multi_table_variables(defs,
                                resolved_tables = c("snap"),
                                auto_table_entries = list()))
})
