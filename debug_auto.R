## Debug auto table integration - insert debug prints into compile_acs_data

devtools::load_all()

## Temporarily override execute_definition to add debug info
orig_execute_definition = environment(compile_acs_data)$execute_definition

## Run compile_acs_data and capture the error with full debug info
tryCatch({
  result = compile_acs_data(
    tables = "B25070",
    years = 2022,
    geography = "state",
    states = "DC")
  cat("SUCCESS\n")
  print(colnames(result)[1:10])
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n\n")

  ## Now debug: build the variables ourselves and check naming
  entry = build_auto_table_entry("B25070", year = 2022)
  tp_vars = suppressMessages(suppressWarnings(
    collect_raw_variables(resolved_tables = "total_population", year = 2022)))
  auto_vars = entry$raw_variables
  all_vars = c(tp_vars, auto_vars)

  cat("Variable keys:\n")
  print(names(all_vars))

  ## Get the actual data the same way compile_acs_data does
  df = suppressMessages(tidycensus::get_acs(
    geography = "state",
    variables = all_vars,
    year = 2022,
    state = "DC",
    survey = "acs5",
    output = "wide"))

  df = df %>%
    dplyr::mutate(data_source_year = 2022)

  moes = df %>% dplyr::select(GEOID, data_source_year, dplyr::matches("_M$"))

  df = df %>%
    dplyr::select(-dplyr::matches("_M$")) %>%
    dplyr::rename_with(~ stringr::str_remove(.x, "_E$"))

  cat("\nActual column names:\n")
  print(colnames(df))

  cat("\nFirst definition:\n")
  print(entry$definitions[[1]])

  ## Test execute_definitions
  tryCatch({
    df2 = execute_definitions(df, entry$definitions)
    cat("\nexecute_definitions succeeded\n")
  }, error = function(e2) {
    cat("\nexecute_definitions failed:", conditionMessage(e2), "\n")
  })
})
