####----Table Registry Tests----####

testthat::test_that(
  "list_tables() returns construct-level table names",
  {
    tables = list_tables()

    ## verify we have the expected count (34 = 32 original - 2 split + 4 constructs)
    testthat::expect_gte(length(tables), 34)

    ## verify construct-level names are present (not internal names)
    testthat::expect_true("age" %in% tables)
    testthat::expect_true("sex" %in% tables)
    testthat::expect_true("nativity" %in% tables)
    testthat::expect_true("language" %in% tables)

    ## verify internal names for split tables are NOT in list_tables()
    testthat::expect_false("sex_by_age" %in% tables)
    testthat::expect_false("nativity_language" %in% tables)

    ## verify unsplit tables are still present
    expected_tables = c(
      "total_population", "public_assistance", "snap", "income_quintiles",
      "gini", "median_household_income", "poverty", "race",
      "disability", "tenure", "occupants_per_room", "units_in_structure",
      "tenure_by_units_in_structure", "year_structure_built", "cost_burden",
      "tenure_by_housing_costs", "median_housing_cost", "median_income_by_tenure",
      "mortgage_status", "transportation_to_work", "travel_time_to_work",
      "vehicles_available", "educational_attainment", "school_enrollment",
      "employment", "household_size", "health_insurance",
      "internet", "computing_devices", "population_density")

    purrr::walk(expected_tables, ~ testthat::expect_true(
      .x %in% tables,
      info = paste0("Missing table: ", .x)))
  })

testthat::test_that(
  "resolve_tables() always includes total_population",
  {
    resolved = resolve_tables(tables = "snap")
    testthat::expect_true("total_population" %in% resolved)
    testthat::expect_true("snap" %in% resolved)
  })

testthat::test_that(
  "resolve_tables() resolves construct names to internal table names",
  {
    ## construct name "age" should resolve to internal name "sex_by_age"
    resolved = resolve_tables(tables = "age")
    testthat::expect_true("sex_by_age" %in% resolved)
    testthat::expect_true("total_population" %in% resolved)

    ## construct name "language" should resolve to "nativity_language"
    resolved = resolve_tables(tables = "language")
    testthat::expect_true("nativity_language" %in% resolved)
  })

testthat::test_that(
  "resolve_tables() still accepts internal table names (backward compat)",
  {
    resolved = resolve_tables(tables = "sex_by_age")
    testthat::expect_true("sex_by_age" %in% resolved)
    testthat::expect_true("total_population" %in% resolved)

    resolved = resolve_tables(tables = "nativity_language")
    testthat::expect_true("nativity_language" %in% resolved)
  })

testthat::test_that(
  "resolve_tables() resolves dependencies",
  {
    resolved = resolve_tables(tables = "population_density")
    testthat::expect_true("total_population" %in% resolved)
    testthat::expect_true("population_density" %in% resolved)
  })

testthat::test_that(
  "resolve_tables() errors on unknown table names",
  {
    testthat::expect_error(
      resolve_tables(tables = "nonexistent_table"),
      "Unknown table")
  })

testthat::test_that(
  "Every registered table has a name and definitions",
  {
    ## iterate internal table names (not construct names)
    purrr::walk(names(.table_registry$tables), function(table_name) {
      table_entry = get_table(table_name)
      testthat::expect_true(!is.null(table_entry$name), info = paste0("Table missing name: ", table_name))
      testthat::expect_true(!is.null(table_entry[["definitions"]]), info = paste0("Table missing definitions: ", table_name))
    })
  })

####----list_variables() Tests----####

testthat::test_that(
  "list_variables() returns a tibble with variable and table columns",
  {
    vars = list_variables()

    testthat::expect_true(tibble::is_tibble(vars))
    testthat::expect_true("variable" %in% colnames(vars))
    testthat::expect_true("table" %in% colnames(vars))
    testthat::expect_gt(nrow(vars), 300)

    ## no duplicate variable names
    testthat::expect_equal(nrow(vars), length(unique(vars$variable)))
  })

testthat::test_that(
  "list_variables() assigns age and sex constructs correctly",
  {
    vars = list_variables()

    age_vars = vars %>% dplyr::filter(table == "age")
    sex_vars = vars %>% dplyr::filter(table == "sex")

    ## age construct includes raw sex_by_age_ vars and computed age_ vars
    testthat::expect_true("age_under_18_percent" %in% age_vars$variable)
    testthat::expect_true("age_over_64_percent" %in% age_vars$variable)
    testthat::expect_true(any(stringr::str_detect(age_vars$variable, "^sex_by_age_")))

    ## sex construct includes sex_female_percent and sex_male_percent
    testthat::expect_true("sex_female_percent" %in% sex_vars$variable)
    testthat::expect_true("sex_male_percent" %in% sex_vars$variable)
  })

testthat::test_that(
  "list_variables() assigns nativity and language constructs correctly",
  {
    vars = list_variables()

    nativity_vars = vars %>% dplyr::filter(table == "nativity")
    language_vars = vars %>% dplyr::filter(table == "language")

    ## nativity construct
    testthat::expect_true("nativity_native_born_percent" %in% nativity_vars$variable)
    testthat::expect_true("nativity_foreign_born_percent" %in% nativity_vars$variable)
    ## raw variables go to nativity
    testthat::expect_true(any(stringr::str_detect(nativity_vars$variable, "^nativity_by_language")))

    ## language construct
    testthat::expect_true("ability_speak_english_very_well_better_percent" %in% language_vars$variable)
    testthat::expect_true("ability_speak_english_less_than_very_well_percent" %in% language_vars$variable)
  })

testthat::test_that(
  "list_variables() includes variables from simple tables",
  {
    vars = list_variables()

    snap_vars = vars %>% dplyr::filter(table == "snap")
    testthat::expect_true("snap_received_percent" %in% snap_vars$variable)
    testthat::expect_true("snap_universe" %in% snap_vars$variable)
    testthat::expect_true("snap_received" %in% snap_vars$variable)
  })
