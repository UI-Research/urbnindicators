#' @importFrom magrittr %>%

####----TABLE REGISTRY INFRASTRUCTURE----####

## Package-level environment storing table definitions
.table_registry = new.env(parent = emptyenv())
.table_registry$tables = list()

## Register a table definition (internal)
register_table = function(table_def) {
  stopifnot(!is.null(table_def$name))
  .table_registry$tables[[table_def$name]] = table_def
}

## Retrieve a table definition (internal)
get_table = function(name) {
  .table_registry$tables[[name]]
}

## Determine the construct-level table name for a variable (internal)
## `constructs` is a list of lists with `name` and `variable_pattern` fields.
## Returns the first matching construct name, or `default_name` if no pattern matches.
assign_construct = function(variable_name, constructs, default_name) {
  match = purrr::detect(constructs, function(construct) {
    !is.null(construct[["variable_pattern"]]) &&
      stringr::str_detect(variable_name, construct[["variable_pattern"]])
  })
  if (!is.null(match)) match[["name"]] else default_name
}

## Get all construct names for a table definition (internal)
## Returns construct names if the table has a `constructs` field, otherwise returns the table name.
get_construct_names = function(table_entry) {
  if (!is.null(table_entry[["constructs"]])) {
    purrr::map_chr(table_entry[["constructs"]], ~ .x[["name"]])
  } else {
    table_entry[["name"]]
  }
}

## Build a mapping from construct names to internal table names (internal)
build_construct_map = function() {
  purrr::imap(.table_registry$tables, function(table_entry, table_name) {
    construct_names = get_construct_names(table_entry)
    stats::setNames(rep(table_name, length(construct_names)), construct_names)
  }) %>%
    unname() %>%
    unlist() %>%
    as.list()
}

####----DSL CONSTRUCTORS----####

#' Define a percentage variable (simple or complex)
#'
#' Creates a definition object for a derived percentage variable. When both
#' \code{numerator} and \code{denominator} are single strings and no other
#' fields are set, a \code{simple_percent} definition is returned. Otherwise a
#' \code{complex} definition is returned, allowing multi-variable numerators
#' and denominators.
#'
#' @param output A string. The name of the output column to create.
#' @param numerator A string. Single numerator column name (simple case).
#' @param denominator A string. Single denominator column name (simple case).
#' @param numerator_variables A character vector of column names to sum for the
#'   numerator (complex case).
#' @param numerator_regex A regex pattern to match numerator columns (complex case).
#' @param numerator_exclude_regex A regex pattern to exclude from numerator matches.
#' @param numerator_note An optional annotation (not used in computation).
#' @param numerator_subtract_variables A character vector of column names to
#'   subtract from the numerator sum.
#' @param numerator_subtract_regex A regex pattern to match columns to subtract
#'   from the numerator.
#' @param denominator_variables A character vector of column names to sum for the
#'   denominator (complex case).
#' @param denominator_regex A regex pattern to match denominator columns (complex case).
#' @param denominator_exclude_regex A regex pattern to exclude from denominator matches.
#' @param subtract_variables A character vector of column names to subtract from
#'   the denominator sum.
#' @param subtract_regex A regex pattern to match columns to subtract from
#'   the denominator.
#' @returns A list with a \code{type} field (\code{"simple_percent"} or
#'   \code{"complex"}) and the associated fields. Can be passed in the
#'   \code{tables} parameter of \code{\link{compile_acs_data}}.
#' @examples
#' # Simple percentage
#' define_percent("snap_received_percent",
#'               numerator = "snap_received",
#'               denominator = "snap_universe")
#'
#' # Complex percentage with subtraction
#' define_percent("snap_not_received_percent",
#'               numerator_variables = c("snap_universe"),
#'               numerator_subtract_variables = c("snap_received"),
#'               denominator_variables = c("snap_universe"))
#' @export
define_percent = function(output,
                          numerator = NULL,
                          denominator = NULL,
                          numerator_variables = NULL,
                          numerator_regex = NULL,
                          numerator_exclude_regex = NULL,
                          numerator_note = NULL,
                          numerator_subtract_variables = NULL,
                          numerator_subtract_regex = NULL,
                          denominator_variables = NULL,
                          denominator_regex = NULL,
                          denominator_exclude_regex = NULL,
                          subtract_variables = NULL,
                          subtract_regex = NULL) {
  ## simple case: single numerator and denominator strings
  if (!is.null(numerator) && is.character(numerator) && length(numerator) == 1 &&
      !is.null(denominator) && is.character(denominator) && length(denominator) == 1 &&
      is.null(numerator_variables) && is.null(numerator_regex) &&
      is.null(denominator_variables) && is.null(denominator_regex) &&
      is.null(subtract_variables) && is.null(subtract_regex) &&
      is.null(numerator_subtract_variables) && is.null(numerator_subtract_regex)) {
    return(list(
      type = "simple_percent",
      output = output,
      numerator = numerator,
      denominator = denominator))
  }

  ## complex case
  result = list(type = "complex", output = output)
  if (!is.null(numerator_variables)) result[["numerator_variables"]] = numerator_variables
  if (!is.null(numerator_regex)) result[["numerator_regex"]] = numerator_regex
  if (!is.null(numerator_exclude_regex)) result[["numerator_exclude_regex"]] = numerator_exclude_regex
  if (!is.null(numerator_note)) result[["numerator_note"]] = numerator_note
  if (!is.null(numerator_subtract_variables)) result[["numerator_subtract_variables"]] = numerator_subtract_variables
  if (!is.null(numerator_subtract_regex)) result[["numerator_subtract_regex"]] = numerator_subtract_regex
  if (!is.null(denominator_variables)) result[["denominator_variables"]] = denominator_variables
  if (!is.null(denominator_regex)) result[["denominator_regex"]] = denominator_regex
  if (!is.null(denominator_exclude_regex)) result[["denominator_exclude_regex"]] = denominator_exclude_regex
  if (!is.null(subtract_variables)) result[["subtract_variables"]] = subtract_variables
  if (!is.null(subtract_regex)) result[["subtract_regex"]] = subtract_regex
  return(result)
}

#' Define an across-percent variable
#'
#' Creates a definition that computes a percentage for every column matching a
#' regex pattern. Each matched column becomes a numerator; the denominator is
#' either a fixed column or computed by a function.
#'
#' @param input_regex A regex pattern to match input columns.
#' @param output_suffix A string appended to each matched column name to form
#'   the output column name (e.g., \code{"_percent"}).
#' @param denominator A string. A fixed denominator column name.
#' @param denominator_function A function that takes a matched column name and
#'   returns the denominator column name for that match.
#' @param denominator_subtract A string. A column to subtract from the
#'   denominator value.
#' @param exclude_regex A regex pattern to exclude from matched columns.
#' @returns A list with \code{type = "across_percent"} and the associated
#'   fields. Can be passed in the \code{tables} parameter of
#'   \code{\link{compile_acs_data}}.
#' @export
define_across_percent = function(input_regex,
                                 output_suffix,
                                 denominator = NULL,
                                 denominator_function = NULL,
                                 denominator_subtract = NULL,
                                 exclude_regex = NULL) {
  result = list(
    type = "across_percent",
    input_regex = input_regex,
    output_suffix = output_suffix)
  if (!is.null(denominator)) result[["denominator"]] = denominator
  if (!is.null(denominator_function)) result[["denominator_function"]] = denominator_function
  if (!is.null(denominator_subtract)) result[["denominator_subtract"]] = denominator_subtract
  if (!is.null(exclude_regex)) result[["exclude_regex"]] = exclude_regex
  return(result)
}

#' Define an across-sum variable
#'
#' Creates a definition that sums each matched column with a corresponding
#' addend column. The addend and output names are determined by user-supplied
#' functions.
#'
#' @param input_regex A regex pattern to match input columns.
#' @param addend_function A function that takes a matched column name and
#'   returns the name of the column to add.
#' @param output_naming_function A function that takes a matched column name
#'   and returns the output column name.
#' @param exclude_regex A regex pattern to exclude from matched columns.
#' @returns A list with \code{type = "across_sum"} and the associated fields.
#'   Can be passed in the \code{tables} parameter of
#'   \code{\link{compile_acs_data}}.
#' @export
define_across_sum = function(input_regex,
                             addend_function,
                             output_naming_function,
                             exclude_regex = NULL) {
  result = list(
    type = "across_sum",
    input_regex = input_regex,
    addend_function = addend_function,
    output_naming_function = output_naming_function)
  if (!is.null(exclude_regex)) result[["exclude_regex"]] = exclude_regex
  return(result)
}

#' Define a one-minus (complement) variable
#'
#' Creates a definition that computes \code{1 - source_variable}.
#'
#' @param output A string. The name of the output column to create.
#' @param source_variable A string. The column to subtract from 1.
#' @returns A list with \code{type = "one_minus"} and the associated fields.
#'   Can be passed in the \code{tables} parameter of
#'   \code{\link{compile_acs_data}}.
#' @export
define_one_minus = function(output, source_variable) {
  list(
    type = "one_minus",
    output = output,
    source_variable = source_variable)
}

#' Define a metadata variable
#'
#' Creates a definition for a non-computed variable that only generates a
#' codebook entry.
#'
#' @param output A string. The name of the metadata column.
#' @param definition_text A string. Human-readable description for the codebook.
#' @returns A list with \code{type = "metadata"} and the associated fields.
#'   Can be passed in the \code{tables} parameter of
#'   \code{\link{compile_acs_data}}.
#' @export
define_metadata = function(output, definition_text) {
  list(
    type = "metadata",
    output = output,
    definition_text = definition_text)
}

####----DSL VALIDATION AND HELPERS----####

.dsl_types = c("simple_percent", "complex", "across_percent", "across_sum",
               "one_minus", "metadata")

## Check whether an object is a DSL definition
is_dsl_definition = function(x) {
  is.list(x) && !is.null(x[["type"]]) && x[["type"]] %in% .dsl_types
}

## Assert that a field is a non-empty string
check_required_string = function(value, field_name, def_label) {
  if (is.null(value) || !is.character(value) || length(value) != 1 || nchar(value) == 0) {
    stop(paste0("Definition `", def_label, "`: `", field_name, "` must be a non-empty string."))
  }
}

## Assert that a field is a character vector
check_character_vector = function(value, field_name, def_label) {
  if (!is.character(value) || length(value) == 0) {
    stop(paste0("Definition `", def_label, "`: `", field_name, "` must be a non-empty character vector."))
  }
}

## Assert that a string is a valid regex
check_valid_regex = function(value, field_name, def_label) {
  check_required_string(value, field_name, def_label)
  tryCatch(
    grepl(value, "", perl = TRUE),
    warning = function(w) {
      stop(paste0("Definition `", def_label, "`: `", field_name,
                   "` is not a valid regex: ", conditionMessage(w)))
    },
    error = function(e) {
      stop(paste0("Definition `", def_label, "`: `", field_name,
                   "` is not a valid regex: ", conditionMessage(e)))
    })
}

## Structural validation of a single definition
validate_definition = function(definition) {
  if (!is.list(definition)) {
    stop("Definition must be a list.")
  }
  type = definition[["type"]]
  if (is.null(type) || !type %in% .dsl_types) {
    stop(paste0("Definition has invalid or missing `type`. Must be one of: ",
                paste0(.dsl_types, collapse = ", "), "."))
  }
  label = definition[["output"]] %||% definition[["input_regex"]] %||% "<unnamed>"

  if (type == "simple_percent") {
    check_required_string(definition[["output"]], "output", label)
    check_required_string(definition[["numerator"]], "numerator", label)
    check_required_string(definition[["denominator"]], "denominator", label)
  } else if (type == "complex") {
    check_required_string(definition[["output"]], "output", label)
    has_num = !is.null(definition[["numerator_variables"]]) || !is.null(definition[["numerator_regex"]])
    has_den = !is.null(definition[["denominator_variables"]]) || !is.null(definition[["denominator_regex"]])
    if (!has_num) stop(paste0("Definition `", label, "`: complex type requires `numerator_variables` or `numerator_regex`."))
    if (!has_den) stop(paste0("Definition `", label, "`: complex type requires `denominator_variables` or `denominator_regex`."))
    if (!is.null(definition[["numerator_variables"]])) check_character_vector(definition[["numerator_variables"]], "numerator_variables", label)
    if (!is.null(definition[["numerator_regex"]])) check_valid_regex(definition[["numerator_regex"]], "numerator_regex", label)
    if (!is.null(definition[["denominator_variables"]])) check_character_vector(definition[["denominator_variables"]], "denominator_variables", label)
    if (!is.null(definition[["denominator_regex"]])) check_valid_regex(definition[["denominator_regex"]], "denominator_regex", label)
    if (!is.null(definition[["numerator_subtract_variables"]])) check_character_vector(definition[["numerator_subtract_variables"]], "numerator_subtract_variables", label)
    if (!is.null(definition[["subtract_variables"]])) check_character_vector(definition[["subtract_variables"]], "subtract_variables", label)
  } else if (type == "across_percent") {
    check_valid_regex(definition[["input_regex"]], "input_regex", label)
    check_required_string(definition[["output_suffix"]], "output_suffix", label)
    has_denom = !is.null(definition[["denominator"]]) || !is.null(definition[["denominator_function"]])
    if (!has_denom) stop(paste0("Definition `", label, "`: across_percent type requires `denominator` or `denominator_function`."))
    if (!is.null(definition[["denominator"]]) && !is.character(definition[["denominator"]])) {
      stop(paste0("Definition `", label, "`: `denominator` must be a string."))
    }
    if (!is.null(definition[["denominator_function"]]) && !is.function(definition[["denominator_function"]])) {
      stop(paste0("Definition `", label, "`: `denominator_function` must be a function."))
    }
  } else if (type == "across_sum") {
    check_valid_regex(definition[["input_regex"]], "input_regex", label)
    if (!is.function(definition[["addend_function"]])) {
      stop(paste0("Definition `", label, "`: `addend_function` must be a function."))
    }
    if (!is.function(definition[["output_naming_function"]])) {
      stop(paste0("Definition `", label, "`: `output_naming_function` must be a function."))
    }
  } else if (type == "one_minus") {
    check_required_string(definition[["output"]], "output", label)
    check_required_string(definition[["source_variable"]], "source_variable", label)
  } else if (type == "metadata") {
    check_required_string(definition[["output"]], "output", label)
    check_required_string(definition[["definition_text"]], "definition_text", label)
  }

  invisible(TRUE)
}

## Extract all explicitly-named variable references from a definition
## (not regex patterns). Used for variable existence checks.
extract_explicit_variables = function(definition) {
  type = definition[["type"]]
  vars = character(0)

  if (type == "simple_percent") {
    vars = c(definition[["numerator"]], definition[["denominator"]])
  } else if (type == "complex") {
    if (!is.null(definition[["numerator_variables"]])) vars = c(vars, definition[["numerator_variables"]])
    if (!is.null(definition[["denominator_variables"]])) vars = c(vars, definition[["denominator_variables"]])
    if (!is.null(definition[["numerator_subtract_variables"]])) vars = c(vars, definition[["numerator_subtract_variables"]])
    if (!is.null(definition[["subtract_variables"]])) vars = c(vars, definition[["subtract_variables"]])
  } else if (type == "across_percent") {
    if (is.character(definition[["denominator"]])) vars = c(vars, definition[["denominator"]])
    if (is.character(definition[["denominator_subtract"]])) vars = c(vars, definition[["denominator_subtract"]])
  } else if (type == "one_minus") {
    vars = c(vars, definition[["source_variable"]])
  }
  ## across_sum and metadata have no explicit variable refs to check
  unique(vars)
}

## Resolve ACS variable codes (e.g. "B22003_002") in definitions to their clean
## column names using the raw variable crosswalk. Returns updated definitions.
resolve_definition_variables = function(definitions, all_raw_variables) {
  ## build code -> clean_name lookup from the named variable vector
  ## all_raw_variables is c(clean_name_ = "B22003_002", ...) — names have trailing _
  if (length(all_raw_variables) == 0) return(definitions)

  code_to_clean = stats::setNames(
    stringr::str_remove(names(all_raw_variables), "_$"),
    as.character(all_raw_variables))

  resolve_one = function(value) {
    if (is.null(value)) return(NULL)
    if (is.character(value)) {
      resolved = code_to_clean[value]
      unname(dplyr::if_else(!is.na(resolved), resolved, value))
    } else {
      value
    }
  }

  purrr::map(definitions, function(def) {
    type = def[["type"]]
    if (type == "simple_percent") {
      def[["numerator"]] = resolve_one(def[["numerator"]])
      def[["denominator"]] = resolve_one(def[["denominator"]])
    } else if (type == "complex") {
      def[["numerator_variables"]] = resolve_one(def[["numerator_variables"]])
      def[["denominator_variables"]] = resolve_one(def[["denominator_variables"]])
      def[["numerator_subtract_variables"]] = resolve_one(def[["numerator_subtract_variables"]])
      def[["subtract_variables"]] = resolve_one(def[["subtract_variables"]])
    } else if (type == "across_percent") {
      def[["denominator"]] = resolve_one(def[["denominator"]])
      def[["denominator_subtract"]] = resolve_one(def[["denominator_subtract"]])
    } else if (type == "one_minus") {
      def[["source_variable"]] = resolve_one(def[["source_variable"]])
    }
    def
  })
}

## Validate that all explicitly-referenced variables in user definitions exist
## in the data frame. Errors with missing variable names and suggests list_variables().
validate_definition_variables = function(definitions, available_columns) {
  all_referenced = purrr::map(definitions, extract_explicit_variables) %>% unlist() %>% unique()
  missing = setdiff(all_referenced, available_columns)
  if (length(missing) > 0) {
    stop(paste0(
      "User-supplied definitions reference variables not found in the data: ",
      paste0("`", missing, "`", collapse = ", "),
      ". Use `list_variables()` to see available variable names."))
  }
  invisible(TRUE)
}

## Warn if a definition's referenced variables span multiple ACS tables.
## MOE calculations assume variables come from the same table.
check_multi_table_variables = function(definitions, resolved_tables, auto_table_entries) {
  ## build variable -> ACS table code map
  var_to_table = list()
  purrr::walk(resolved_tables, function(table_name) {
    entry = get_table(table_name)
    if (!is.null(entry[["raw_variables"]])) {
      clean = stringr::str_remove(names(entry[["raw_variables"]]), "_$")
      codes = stringr::str_extract(as.character(entry[["raw_variables"]]), "^[BC][0-9]{5}[A-I]?")
      purrr::walk2(clean, codes, function(cl, cd) {
        var_to_table[[cl]] <<- cd
      })
    }
  })
  if (length(auto_table_entries) > 0) {
    purrr::walk(auto_table_entries, function(auto_entry) {
      if (!is.null(auto_entry[["raw_variables"]])) {
        clean = stringr::str_remove(names(auto_entry[["raw_variables"]]), "_$")
        codes = stringr::str_extract(as.character(auto_entry[["raw_variables"]]), "^[BC][0-9]{5}[A-I]?")
        purrr::walk2(clean, codes, function(cl, cd) {
          var_to_table[[cl]] <<- cd
        })
      }
    })
  }

  purrr::walk(definitions, function(def) {
    vars = extract_explicit_variables(def)
    tables_used = unique(unlist(var_to_table[vars]))
    tables_used = tables_used[!is.na(tables_used)]
    if (length(tables_used) > 1) {
      label = def[["output"]] %||% def[["input_regex"]] %||% "<unnamed>"
      rlang::warn(paste0(
        "Definition `", label, "` references variables from multiple ACS tables (",
        paste0(tables_used, collapse = ", "),
        "). MOE calculations assume variables come from the same table; ",
        "the computed MOE for this variable may be approximate."))
    }
  })
  invisible(TRUE)
}

####----EXECUTION ENGINE----####

## Resolve column names matching regex, excluding percent/MOE columns
resolve_regex_columns = function(.data, regex, exclude_regex = NULL) {
  cols = colnames(.data)
  matched = cols[stringr::str_detect(cols, regex)]
  if (!is.null(exclude_regex) && nchar(exclude_regex) > 0) {
    matched = matched[!stringr::str_detect(matched, exclude_regex)]
  }
  matched = matched[!stringr::str_detect(matched, "_M$")]
  return(matched)
}

## Execute a single definition against a data frame
execute_definition = function(.data, definition) {
  type = definition[["type"]]

  if (type == "simple_percent") {
    output = definition[["output"]]
    .data[[output]] = safe_divide(.data[[definition[["numerator"]]]], .data[[definition[["denominator"]]]])
    return(.data)
  }

  if (type == "across_percent") {
    input_columns = resolve_regex_columns(.data, definition[["input_regex"]], definition[["exclude_regex"]])
    .data = purrr::reduce(input_columns, function(.data, column) {
      output_column = paste0(column, definition[["output_suffix"]])
      ## determine denominator
      if (is.character(definition[["denominator"]])) {
        denominator_value = .data[[definition[["denominator"]]]]
      } else if (is.function(definition[["denominator_function"]])) {
        denominator_column_name = definition[["denominator_function"]](column)
        denominator_value = .data[[denominator_column_name]]
      } else {
        denominator_value = .data[[definition[["denominator"]]]]
      }
      ## apply denominator subtraction if specified
      if (!is.null(definition[["denominator_subtract"]])) {
        denominator_value = denominator_value - .data[[definition[["denominator_subtract"]]]]
      }
      .data[[output_column]] = safe_divide(.data[[column]], denominator_value)
      .data
    }, .init = .data)
    return(.data)
  }

  if (type == "across_sum") {
    input_columns = resolve_regex_columns(.data, definition[["input_regex"]], definition[["exclude_regex"]])
    .data = purrr::reduce(input_columns, function(.data, column) {
      addend_column_name = definition[["addend_function"]](column)
      output_column = definition[["output_naming_function"]](column)
      .data[[output_column]] = .data[[column]] + .data[[addend_column_name]]
      .data
    }, .init = .data)
    return(.data)
  }

  if (type == "complex") {
    output = definition[["output"]]

    ## resolve numerator columns
    if (!is.null(definition[["numerator_variables"]])) {
      numerator_columns = definition[["numerator_variables"]]
    } else if (!is.null(definition[["numerator_regex"]])) {
      numerator_columns = resolve_regex_columns(.data, definition[["numerator_regex"]], definition[["numerator_exclude_regex"]])
    }

    ## resolve denominator columns
    if (!is.null(definition[["denominator_variables"]])) {
      denominator_columns = definition[["denominator_variables"]]
    } else if (!is.null(definition[["denominator_regex"]])) {
      denominator_columns = resolve_regex_columns(.data, definition[["denominator_regex"]], definition[["denominator_exclude_regex"]])
    }

    ## compute numerator
    numerator_value = rowSums(dplyr::select(.data, dplyr::all_of(numerator_columns)))

    ## handle numerator subtraction
    if (!is.null(definition[["numerator_subtract_variables"]])) {
      numerator_subtract_value = rowSums(dplyr::select(.data, dplyr::all_of(definition[["numerator_subtract_variables"]])))
      numerator_value = numerator_value - numerator_subtract_value
    } else if (!is.null(definition[["numerator_subtract_regex"]])) {
      numerator_subtract_columns = resolve_regex_columns(.data, definition[["numerator_subtract_regex"]])
      numerator_subtract_value = rowSums(dplyr::select(.data, dplyr::all_of(numerator_subtract_columns)))
      numerator_value = numerator_value - numerator_subtract_value
    }

    ## compute denominator
    denominator_value = rowSums(dplyr::select(.data, dplyr::all_of(denominator_columns)))

    ## handle denominator subtraction
    if (!is.null(definition[["subtract_variables"]])) {
      subtract_value = rowSums(dplyr::select(.data, dplyr::all_of(definition[["subtract_variables"]])))
      denominator_value = denominator_value - subtract_value
    } else if (!is.null(definition[["subtract_regex"]])) {
      subtract_columns = resolve_regex_columns(.data, definition[["subtract_regex"]])
      subtract_value = rowSums(dplyr::select(.data, dplyr::all_of(subtract_columns)))
      denominator_value = denominator_value - subtract_value
    }

    .data[[output]] = safe_divide(numerator_value, denominator_value)
    return(.data)
  }

  if (type == "one_minus") {
    output = definition[["output"]]
    .data[[output]] = 1 - .data[[definition[["source_variable"]]]]
    return(.data)
  }

  if (type == "metadata") {
    ## no-op: metadata definitions don't produce computed columns
    return(.data)
  }

  stop(paste0("Unknown definition type: ", type))
}

## Execute all definitions in order
execute_definitions = function(.data, definitions) {
  purrr::reduce(definitions, function(.data, definition) {
    execute_definition(.data, definition)
  }, .init = .data)
}

####----LIST FUNCTIONS----####

#' @title List available table names
#' @description Returns the names of all registered ACS tables that can be
#'   requested via the \code{tables} parameter of \code{compile_acs_data()}.
#'   Multi-construct tables (e.g., \code{sex_by_age}) are reported as their
#'   individual constructs (e.g., \code{"age"} and \code{"sex"}).
#'   Note: only pre-registered tables are listed here. Any valid ACS table code
#'   (e.g., \code{"B25070"}) can also be passed to \code{compile_acs_data(tables = ...)}
#'   and will be auto-processed.
#' @returns A character vector of table names.
#' @examples
#' list_tables()
#' @export
list_tables = function() {
  all_names = purrr::map(names(.table_registry$tables), function(table_name) {
    table_entry = .table_registry$tables[[table_name]]
    get_construct_names(table_entry)
  }) %>% unlist()
  sort(unique(all_names))
}

## Resolve user selections to full set of internal table names (internal)
## Always includes total_population.
## Accepts both construct names (e.g., "age") and internal names (e.g., "sex_by_age").
resolve_tables = function(tables = NULL) {
  resolved = "total_population"
  construct_map = build_construct_map()
  internal_names = names(.table_registry$tables)

  if (!is.null(tables)) {
    ## map each user-provided name to an internal table name
    mapped = purrr::map_chr(tables, function(requested_table) {
      if (requested_table %in% internal_names) {
        requested_table
      } else if (requested_table %in% names(construct_map)) {
        construct_map[[requested_table]]
      } else {
        NA_character_
      }
    })
    unknown = tables[is.na(mapped)]
    if (length(unknown) > 0) {
      stop(paste0("Unknown table(s): ", paste0(unknown, collapse = ", "),
                   ". Use list_tables() to see available tables."))
    }
    resolved = union(resolved, mapped)
  }

  ## resolve dependencies
  resolved = purrr::reduce(resolved, function(accumulated, table_name) {
    table_entry = get_table(table_name)
    if (!is.null(table_entry[["depends_on"]]) && length(table_entry[["depends_on"]]) > 0) {
      union(accumulated, table_entry[["depends_on"]])
    } else {
      accumulated
    }
  }, .init = resolved)

  return(resolved)
}

## Build named ACS variable vector for resolved tables (internal)
collect_raw_variables = function(resolved_tables, year = 2022) {
  suppressWarnings({suppressMessages({
    census_codebook = tidycensus::load_variables(year = 2022, dataset = "acs5")
  })})

  select_variables = purrr::partial(select_variables_by_name, census_codebook = census_codebook)

  all_variables = purrr::map(resolved_tables, function(table_name) {
    table_entry = get_table(table_name)
    if (is.null(table_entry)) return(NULL)

    selected_variables = c()
    if (!is.null(table_entry[["raw_variable_source"]]) && table_entry[["raw_variable_source"]][["type"]] == "select_variables") {
      selected_variables = purrr::map(table_entry[["raw_variable_source"]][["calls"]], function(selection_call) {
        selected = select_variables(variable_name = selection_call[["pattern"]])
        if (!is.null(selection_call[["filter"]])) {
          selected = filter_variables(
            variable_vector = selected,
            match_string = selection_call[["filter"]][["match_string"]],
            match_type = selection_call[["filter"]][["match_type"]])
        }
        selected
      }) %>% unlist()
    }

    if (!is.null(table_entry[["raw_variables"]])) {
      selected_variables = c(selected_variables, table_entry[["raw_variables"]])
    }
    selected_variables
  }) %>% purrr::compact() %>% unlist()

  return(all_variables)
}

#' @title List all variables and their tables
#' @description Returns a tibble mapping all variables (raw ACS variables and
#'   computed indicators) to their construct-level table name. This provides a
#'   comprehensive view of every variable that \code{compile_acs_data()} produces
#'   for registered tables. Variables from unregistered ACS tables passed as
#'   raw codes (e.g., \code{"B25070"}) are not included here; they are
#'   auto-generated at runtime.
#' @param year The ACS year used to resolve variable names (default 2022).
#' @returns A tibble with columns \code{variable} and \code{table}.
#' @examples
#' \dontrun{
#' list_variables()
#' list_variables() %>% dplyr::filter(table == "age")
#' }
#' @export
list_variables = function(year = 2022) {
  suppressWarnings({suppressMessages({
    census_codebook = tidycensus::load_variables(year = year, dataset = "acs5")
  })})

  select_variables = purrr::partial(select_variables_by_name, census_codebook = census_codebook)

  all_rows = purrr::imap(.table_registry$tables, function(table_entry, table_name) {
    constructs = table_entry[["constructs"]]

    ## Step 1: collect raw variable clean names for this table
    raw_clean_names = character(0)

    if (!is.null(table_entry[["raw_variable_source"]]) && table_entry[["raw_variable_source"]][["type"]] == "select_variables") {
      raw_clean_names = purrr::map(table_entry[["raw_variable_source"]][["calls"]], function(selection_call) {
        selected = select_variables(variable_name = selection_call[["pattern"]])
        if (!is.null(selection_call[["filter"]])) {
          selected = filter_variables(
            variable_vector = selected,
            match_string = selection_call[["filter"]][["match_string"]],
            match_type = selection_call[["filter"]][["match_type"]])
        }
        stringr::str_remove(names(selected), "_$")
      }) %>% unlist()
    }

    if (!is.null(table_entry[["raw_variables"]])) {
      raw_clean_names = c(raw_clean_names, stringr::str_remove(names(table_entry[["raw_variables"]]), "_$"))
    }

    ## Step 2: process definitions in order to resolve computed variable names
    ## maintain a running set of available columns (raw + previously computed)
    available_columns = raw_clean_names
    computed_names = character(0)

    if (!is.null(table_entry[["definitions"]])) {
      result = purrr::reduce(table_entry[["definitions"]], function(state, entry) {
        entry_type = entry[["type"]]
        available = state$available
        computed = state$computed

        if (entry_type %in% c("simple_percent", "complex", "one_minus", "metadata")) {
          out = entry[["output"]]
          if (is.character(out)) {
            computed = c(computed, out)
            available = c(available, out)
          }
        } else if (entry_type == "across_percent") {
          matched = available[stringr::str_detect(available, entry[["input_regex"]])]
          if (!is.null(entry[["exclude_regex"]]) && nchar(entry[["exclude_regex"]]) > 0) {
            matched = matched[!stringr::str_detect(matched, entry[["exclude_regex"]])]
          }
          matched = matched[!stringr::str_detect(matched, "_M$")]
          outputs = paste0(matched, entry[["output_suffix"]])
          computed = c(computed, outputs)
          available = c(available, outputs)
        } else if (entry_type == "across_sum") {
          matched = available[stringr::str_detect(available, entry[["input_regex"]])]
          if (!is.null(entry[["exclude_regex"]]) && nchar(entry[["exclude_regex"]]) > 0) {
            matched = matched[!stringr::str_detect(matched, entry[["exclude_regex"]])]
          }
          matched = matched[!stringr::str_detect(matched, "_M$")]
          outputs = purrr::map_chr(matched, entry[["output_naming_function"]])
          computed = c(computed, outputs)
          available = c(available, outputs)
        }
        list(available = available, computed = computed)
      }, .init = list(available = available_columns, computed = computed_names))
      computed_names = result$computed
    }

    ## Step 3: assign each variable to a construct
    all_variable_names = unique(c(raw_clean_names, computed_names))
    purrr::map(all_variable_names, function(variable_name) {
      if (!is.null(constructs)) {
        table_label = assign_construct(variable_name, constructs, table_name)
      } else {
        table_label = table_name
      }
      tibble::tibble(variable = variable_name, table = table_label)
    }) %>% purrr::list_rbind()
  }) %>% purrr::list_rbind()

  ## deduplicate (a variable should appear only once)
  all_rows = all_rows[!duplicated(all_rows$variable), ]
  all_rows
}

####----CODEBOOK ENTRY EXPANSION----####

## Expand a structured codebook entry into text-format definition strings
## that calculate_cvs.R can parse. Returns a tibble with calculated_variable,
## variable_type, definition.
expand_codebook_entry = function(entry, .data, crosswalk) {
  type = entry[["type"]]

  format_variable = function(clean_name) {
    raw = crosswalk %>%
      dplyr::filter(clean_name == !!clean_name) %>%
      dplyr::pull(raw_name)
    if (length(raw) == 0 || is.na(raw[1])) {
      paste0(clean_name, " (calculated variable)")
    } else {
      paste0(clean_name, " (", raw[1], ")")
    }
  }

  if (type == "simple_percent") {
    numerator_formatted = format_variable(entry[["numerator"]])
    denominator_formatted = format_variable(entry[["denominator"]])
    return(tibble::tibble(
      calculated_variable = entry[["output"]],
      variable_type = "Percent",
      definition = paste0("Numerator = ", numerator_formatted, ". Denominator = ", denominator_formatted, "."),
      numerator_vars = list(entry[["numerator"]]),
      numerator_subtract_vars = list(character(0)),
      denominator_vars = list(entry[["denominator"]]),
      denominator_subtract_vars = list(character(0))))
  }

  if (type == "across_percent") {
    input_columns = .data %>%
      dplyr::select(dplyr::matches(entry[["input_regex"]])) %>%
      {
        if (!is.null(entry[["exclude_regex"]]) && nchar(entry[["exclude_regex"]]) > 0) {
          dplyr::select(., -dplyr::matches(entry[["exclude_regex"]]))
        } else { . }
      } %>%
      dplyr::select(-dplyr::matches("percent$|_M$")) %>%
      colnames()

    purrr::map(input_columns, function(column) {
      output_column = paste0(column, entry[["output_suffix"]])

      ## determine denominator
      if (is.character(entry[["denominator"]])) {
        denominator_column = entry[["denominator"]]
      } else if (is.function(entry[["denominator_function"]])) {
        denominator_column = entry[["denominator_function"]](column)
      } else {
        denominator_column = entry[["denominator"]]
      }

      numerator_formatted = format_variable(column)
      denominator_formatted = format_variable(denominator_column)

      ## handle denominator subtraction
      if (!is.null(entry[["denominator_subtract"]])) {
        subtract_formatted = format_variable(entry[["denominator_subtract"]])
        denominator_formatted = paste0(denominator_formatted, " - ", subtract_formatted)
      }

      ## determine subtract column for pre-parsed columns
      subtract_col = if (!is.null(entry[["denominator_subtract"]])) entry[["denominator_subtract"]] else character(0)

      tibble::tibble(
        calculated_variable = output_column,
        variable_type = "Percent",
        definition = paste0("Numerator = ", numerator_formatted, ". Denominator = ", denominator_formatted, "."),
        numerator_vars = list(column),
        numerator_subtract_vars = list(character(0)),
        denominator_vars = list(denominator_column),
        denominator_subtract_vars = list(subtract_col))
    }) %>% purrr::list_rbind()
  }

  else if (type == "across_sum") {
    input_columns = .data %>%
      dplyr::select(dplyr::matches(entry[["input_regex"]])) %>%
      {
        if (!is.null(entry[["exclude_regex"]]) && nchar(entry[["exclude_regex"]]) > 0) {
          dplyr::select(., -dplyr::matches(entry[["exclude_regex"]]))
        } else { . }
      } %>%
      dplyr::select(-dplyr::matches("percent$|_M$")) %>%
      colnames()

    purrr::map(input_columns, function(column) {
      addend = entry[["addend_function"]](column)
      output_column = entry[["output_naming_function"]](column)

      column_formatted = format_variable(column)
      addend_formatted = format_variable(addend)

      tibble::tibble(
        calculated_variable = output_column,
        variable_type = "Sum",
        definition = paste0("Sum of: ", column_formatted, ", ", addend_formatted, "."),
        numerator_vars = list(c(column, addend)),
        numerator_subtract_vars = list(character(0)),
        denominator_vars = list(character(0)),
        denominator_subtract_vars = list(character(0)))
    }) %>% purrr::list_rbind()
  }

  else if (type == "complex") {
    ## handle numerator
    if (!is.null(entry[["numerator_variables"]])) {
      numerator_columns = entry[["numerator_variables"]]
    } else if (!is.null(entry[["numerator_regex"]])) {
      numerator_columns = .data %>%
        dplyr::select(dplyr::matches(entry[["numerator_regex"]])) %>%
        dplyr::select(-dplyr::matches("percent$|_M$")) %>%
        {
          if (!is.null(entry[["numerator_exclude_regex"]]) && nchar(entry[["numerator_exclude_regex"]]) > 0) {
            dplyr::select(., -dplyr::matches(entry[["numerator_exclude_regex"]]))
          } else { . }
        } %>%
        colnames()
    }

    ## handle denominator
    if (!is.null(entry[["denominator_variables"]])) {
      denominator_columns = entry[["denominator_variables"]]
    } else if (!is.null(entry[["denominator_regex"]])) {
      denominator_columns = .data %>%
        dplyr::select(dplyr::matches(entry[["denominator_regex"]])) %>%
        dplyr::select(-dplyr::matches("percent$|_M$")) %>%
        {
          if (!is.null(entry[["denominator_exclude_regex"]]) && nchar(entry[["denominator_exclude_regex"]]) > 0) {
            dplyr::select(., -dplyr::matches(entry[["denominator_exclude_regex"]]))
          } else { . }
        } %>%
        colnames()
    }

    ## handle denominator subtraction variables
    if (!is.null(entry[["subtract_variables"]])) {
      subtract_columns = entry[["subtract_variables"]]
    } else if (!is.null(entry[["subtract_regex"]])) {
      subtract_columns = .data %>%
        dplyr::select(dplyr::matches(entry[["subtract_regex"]])) %>%
        dplyr::select(-dplyr::matches("percent$|_M$")) %>%
        colnames()
    } else {
      subtract_columns = NULL
    }

    ## handle numerator subtraction variables
    if (!is.null(entry[["numerator_subtract_variables"]])) {
      numerator_subtract_columns = entry[["numerator_subtract_variables"]]
    } else if (!is.null(entry[["numerator_subtract_regex"]])) {
      numerator_subtract_columns = .data %>%
        dplyr::select(dplyr::matches(entry[["numerator_subtract_regex"]])) %>%
        dplyr::select(-dplyr::matches("percent$|_M$")) %>%
        colnames()
    } else {
      numerator_subtract_columns = NULL
    }

    numerator_formatted = purrr::map_chr(numerator_columns, format_variable) %>% paste0(collapse = ", ")
    denominator_formatted = purrr::map_chr(denominator_columns, format_variable) %>% paste0(collapse = ", ")

    if (!is.null(numerator_subtract_columns) && length(numerator_subtract_columns) > 0) {
      numerator_subtract_formatted = purrr::map_chr(numerator_subtract_columns, format_variable) %>% paste0(collapse = ", ")
      numerator_formatted = paste0(numerator_formatted, " - ", numerator_subtract_formatted)
    }

    if (!is.null(subtract_columns) && length(subtract_columns) > 0) {
      subtract_formatted = purrr::map_chr(subtract_columns, format_variable) %>% paste0(collapse = ", ")
      denominator_formatted = paste0(denominator_formatted, " - ", subtract_formatted)
    }

    ## pre-parsed columns for complex type
    num_sub_cols = if (!is.null(numerator_subtract_columns) && length(numerator_subtract_columns) > 0) {
      numerator_subtract_columns
    } else { character(0) }
    denom_sub_cols = if (!is.null(subtract_columns) && length(subtract_columns) > 0) {
      subtract_columns
    } else { character(0) }

    return(tibble::tibble(
      calculated_variable = entry[["output"]],
      variable_type = "Percent",
      definition = paste0("Numerator = ", numerator_formatted, ". Denominator = ", denominator_formatted, "."),
      numerator_vars = list(numerator_columns),
      numerator_subtract_vars = list(num_sub_cols),
      denominator_vars = list(denominator_columns),
      denominator_subtract_vars = list(denom_sub_cols)))
  }

  else if (type == "one_minus") {
    return(tibble::tibble(
      calculated_variable = entry[["output"]],
      variable_type = "Percent",
      definition = paste0("One minus ", entry[["source_variable"]], "."),
      numerator_vars = list(entry[["source_variable"]]),
      numerator_subtract_vars = list(character(0)),
      denominator_vars = list(character(0)),
      denominator_subtract_vars = list(character(0))))
  }

  else if (type == "metadata") {
    return(tibble::tibble(
      calculated_variable = entry[["output"]],
      variable_type = "Metadata",
      definition = entry[["definition_text"]],
      numerator_vars = list(character(0)),
      numerator_subtract_vars = list(character(0)),
      denominator_vars = list(character(0)),
      denominator_subtract_vars = list(character(0))))
  }

  else {
    return(tibble::tibble(
      calculated_variable = entry[["output"]],
      variable_type = "Error",
      definition = paste0("Unrecognized codebook entry type: ", type),
      numerator_vars = list(character(0)),
      numerator_subtract_vars = list(character(0)),
      denominator_vars = list(character(0)),
      denominator_subtract_vars = list(character(0))))
  }
}

####----TABLE REGISTRATIONS: POPULATION----####

register_table(list(
  name = "total_population",
  description = "Total population",
  acs_tables = "B01003",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(total_population_universe_ = "B01003_001"),
  definitions = list()
))

####----TABLE REGISTRATIONS: INCOME, POVERTY, FINANCIAL ASSISTANCE----####

register_table(list(
  name = "public_assistance",
  description = "Public assistance income or food stamps/SNAP receipt for households",
  acs_tables = "B19058",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    public_assistance_universe_ = "B19058_001",
    public_assistance_received_ = "B19058_002"),
  definitions = list(
    define_percent("public_assistance_received_percent",
                   numerator = "public_assistance_received",
                   denominator = "public_assistance_universe"))
))

register_table(list(
  name = "snap",
  description = "SNAP/food stamps receipt",
  acs_tables = "B22003",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    snap_universe_ = "B22003_001",
    snap_received_ = "B22003_002"),
  definitions = list(
    define_percent("snap_received_percent",
                   numerator = "snap_received",
                   denominator = "snap_universe"))
))

register_table(list(
  name = "income_quintiles",
  description = "Household income quintile upper limits, means, and aggregate shares",
  acs_tables = c("B19080", "B19081", "B19082"),
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    household_income_quintile_upper_limit_1_ = "B19080_001",
    household_income_quintile_upper_limit_2_ = "B19080_002",
    household_income_quintile_upper_limit_3_ = "B19080_003",
    household_income_quintile_upper_limit_4_ = "B19080_004",
    household_income_quintile_upper_limit_5_ = "B19080_005",
    household_income_quintile_mean_1_ = "B19081_001",
    household_income_quintile_mean_2_ = "B19081_002",
    household_income_quintile_mean_3_ = "B19081_003",
    household_income_quintile_mean_4_ = "B19081_004",
    household_income_quintile_mean_5_ = "B19081_005",
    household_income_quintile_mean_6_ = "B19081_006",
    household_income_quintile_share_aggregate_1_ = "B19082_001",
    household_income_quintile_share_aggregate_2_ = "B19082_002",
    household_income_quintile_share_aggregate_3_ = "B19082_003",
    household_income_quintile_share_aggregate_4_ = "B19082_004",
    household_income_quintile_share_aggregate_5_ = "B19082_005",
    household_income_quintile_share_aggregate_6_ = "B19082_006"),
  definitions = list()
))

register_table(list(
  name = "gini",
  description = "Gini index of income inequality",
  acs_tables = "B19083",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(gini_index_ = "B19083_001"),
  definitions = list()
))

register_table(list(
  name = "median_household_income",
  description = "Median household income by race/ethnicity",
  acs_tables = c("B19013", "B19013A", "B19013B", "B19013C", "B19013D",
                 "B19013E", "B19013F", "B19013G", "B19013H", "B19013I"),
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    median_household_income_universe_allraces_ = "B19013_001",
    median_household_income_white_alone_ = "B19013A_001",
    median_household_income_white_alone_nonhispanic_ = "B19013H_001",
    median_household_income_black_alone_ = "B19013B_001",
    median_household_income_aian_alone_ = "B19013C_001",
    median_household_income_asian_alone_ = "B19013D_001",
    median_household_income_nhpi_ = "B19013E_001",
    median_household_income_otherrace_alone_ = "B19013F_001",
    median_household_income_twoormore_ = "B19013G_001",
    median_household_income_hispanic_ = "B19013I_001"),
  definitions = list()
))

register_table(list(
  name = "poverty",
  description = "Poverty status by race/ethnicity",
  acs_tables = c("B17020", "B17020B", "B17020C", "B17020D", "B17020E",
                 "B17020F", "B17020G", "B17020H", "B17020I"),
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    federal_poverty_limit_universe_allraces_ = "B17020_001",
    federal_poverty_limit_universe_aian_alone_ = "B17020C_001",
    federal_poverty_limit_universe_asian_alone_ = "B17020D_001",
    federal_poverty_limit_universe_black_alone_ = "B17020B_001",
    federal_poverty_limit_universe_hispanic_ = "B17020I_001",
    federal_poverty_limit_universe_nhpi_alone_ = "B17020E_001",
    federal_poverty_limit_universe_otherrace_alone_ = "B17020F_001",
    federal_poverty_limit_universe_twoormore_ = "B17020G_001",
    federal_poverty_limit_universe_white_alone_nonhispanic_ = "B17020H_001",
    federal_poverty_limit_below_allraces_ = "B17020_002",
    federal_poverty_limit_below_aian_alone_ = "B17020C_002",
    federal_poverty_limit_below_asian_alone_ = "B17020D_002",
    federal_poverty_limit_below_black_alone_ = "B17020B_002",
    federal_poverty_limit_below_hispanic_ = "B17020I_002",
    federal_poverty_limit_below_nhpi_alone_ = "B17020E_002",
    federal_poverty_limit_below_otherrace_alone_ = "B17020F_002",
    federal_poverty_limit_below_twoormore_ = "B17020G_002",
    federal_poverty_limit_below_white_alone_nonhispanic_ = "B17020H_002"),
  definitions = list(
    define_across_percent(
      input_regex = "federal_poverty_limit.*below",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator_function = function(column) { column %>% stringr::str_replace("below", "universe") }))
))

####----TABLE REGISTRATIONS: RACE AND ETHNICITY----####

register_table(list(
  name = "race",
  description = "Race and ethnicity (Hispanic origin by race)",
  acs_tables = "B03002",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    race_universe_ = "B03002_001",
    race_nonhispanic_allraces_ = "B03002_002",
    race_nonhispanic_white_alone_ = "B03002_003",
    race_nonhispanic_black_alone_ = "B03002_004",
    race_nonhispanic_aian_alone_ = "B03002_005",
    race_nonhispanic_asian_alone_ = "B03002_006",
    race_nonhispanic_nhpi_alone_ = "B03002_007",
    race_nonhispanic_otherrace_alone_ = "B03002_008",
    race_nonhispanic_twoormore_ = "B03002_009",
    race_nonhispanic_twoormore_includingotherrace_ = "B03002_010",
    race_nonhispanic_twoormore_excludingotherrace_ = "B03002_011",
    race_hispanic_allraces_ = "B03002_012",
    race_hispanic_white_alone_ = "B03002_013",
    race_hispanic_black_alone_ = "B03002_014",
    race_hispanic_aian_alone_ = "B03002_015",
    race_hispanic_asian_alone_ = "B03002_016",
    race_hispanic_nhpi_alone_ = "B03002_017",
    race_hispanic_otherrace_alone_ = "B03002_018",
    race_hispanic_twoormore_ = "B03002_019",
    race_hispanic_twoormore_includingotherrace_ = "B03002_020",
    race_hispanic_twoormore_excludingotherrace_ = "B03002_021"),
  definitions = list(
    define_across_percent(
      input_regex = "^race_nonhispanic|^race_hispanic",
      exclude_regex = NULL,
      output_suffix = "_percent",
      denominator = "race_universe"),
    define_one_minus("race_personofcolor_percent",
                     source_variable = "race_nonhispanic_white_alone_percent"))
))

####----TABLE REGISTRATIONS: SEX AND AGE----####

register_table(list(
  name = "sex_by_age",
  description = "Sex by age",
  acs_tables = "B01001",
  depends_on = character(0),
  constructs = list(
    list(name = "age", variable_pattern = "^age_|^sex_by_age_"),
    list(name = "sex", variable_pattern = "^sex_(female|male)")
  ),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B01001_"))),
  raw_variables = NULL,
  definitions = list(
    define_percent("sex_female_percent",
                   numerator = "sex_by_age_female",
                   denominator = "sex_by_age_universe"),
    define_percent("sex_male_percent",
                   numerator = "sex_by_age_male",
                   denominator = "sex_by_age_universe"),
    define_across_sum(
      input_regex = "sex_by_age_female_.*years($|_over$)",
      exclude_regex = NULL,
      addend_function = function(column) { column %>% stringr::str_replace("female", "male") },
      output_naming_function = function(column) { column %>% stringr::str_replace("sex_by_age_female_", "age_") }),
    define_across_percent(
      input_regex = "^age.*years($|_over$)",
      exclude_regex = NULL,
      output_suffix = "_percent",
      denominator = "sex_by_age_universe"),
    define_percent("age_under_18_percent",
                   numerator_variables = c("age_under_5_years", "age_5_9_years", "age_10_14_years", "age_15_17_years"),
                   denominator_variables = c("sex_by_age_universe")),
    define_percent("age_over_64_percent",
                   numerator_regex = "age_(6(5|7)|7|8).*_years($|_over$)",
                   denominator_variables = c("sex_by_age_universe")))
))

####----TABLE REGISTRATIONS: DISABILITY----####

register_table(list(
  name = "disability",
  description = "Sex by age by disability status",
  acs_tables = "B18101",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B18101_"))),
  raw_variables = NULL,
  definitions = list(
    define_percent("disability_percent",
                   numerator_regex = "with_a_disability",
                   denominator_variables = c("sex_by_age_by_disability_status_universe")))
))

####----TABLE REGISTRATIONS: HOUSING----####

register_table(list(
  name = "tenure",
  description = "Tenure (by race/ethnicity of householder)",
  acs_tables = "B25003",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B25003"))),
  raw_variables = NULL,
  definitions = list(
    define_across_percent(
      input_regex = "^tenure_renter_occupied|^tenure_owner_occupied",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator = "tenure_universe"),
    define_across_sum(
      input_regex = "tenure_.*_householder_renter_occupied",
      exclude_regex = "percent",
      addend_function = function(column) { column %>% stringr::str_replace("renter", "owner") },
      output_naming_function = function(column) { column %>% stringr::str_replace_all("renter_occupied", "renter_owner_occupied") }),
    define_across_percent(
      input_regex = "tenure.*householder_renter_occupied",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator_function = function(column) { column %>% stringr::str_replace("renter", "renter_owner") }),
    define_across_percent(
      input_regex = "tenure.*householder_owner_occupied",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator_function = function(column) { column %>% stringr::str_replace("owner", "renter_owner") }))
))

register_table(list(
  name = "occupants_per_room",
  description = "Tenure by occupants per room",
  acs_tables = "B25014",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B25014"))),
  raw_variables = NULL,
  definitions = list(
    define_percent("overcrowding_morethan1_ppr_alltenures_percent",
                   numerator_regex = "tenure_by_occupants_per_room.*(1_01|1_51|2_01)",
                   denominator_variables = c("tenure_by_occupants_per_room_universe")),
    define_percent("overcrowding_morethan1_ppr_renteroccupied_percent",
                   numerator_regex = "tenure_by_occupants_per_room_renter.*(1_01|1_51|2_01)",
                   denominator_variables = c("tenure_by_occupants_per_room_renter_occupied")))
))

register_table(list(
  name = "units_in_structure",
  description = "Units in structure",
  acs_tables = "B25024",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B25024"))),
  raw_variables = NULL,
  definitions = list(
    define_across_percent(
      input_regex = "^units_in_structure",
      exclude_regex = "universe|householder|percent",
      output_suffix = "_percent",
      denominator = "units_in_structure_universe"))
))

register_table(list(
  name = "tenure_by_units_in_structure",
  description = "Tenure by units in structure",
  acs_tables = "B25032",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B25032"))),
  raw_variables = NULL,
  definitions = list(
    define_across_sum(
      input_regex = "tenure_by_units.*renter_occupied_housing_units",
      exclude_regex = "owner|percent",
      addend_function = function(column) { column %>% stringr::str_replace("renter", "owner") },
      output_naming_function = function(column) { column %>% stringr::str_replace_all("renter_occupied_housing_units", "renter_owner_occupied_housing_units") }),
    define_across_percent(
      input_regex = "tenure_by_units_in_structure_renter_owner_occupied_housing_units_",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator = "tenure_by_units_in_structure_renter_owner_occupied_housing_units"),
    define_across_percent(
      input_regex = "tenure_by_units_in_structure_renter_occupied_housing_units_",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator = "tenure_by_units_in_structure_renter_occupied_housing_units"),
    define_across_percent(
      input_regex = "tenure_by_units_in_structure_owner_occupied_housing_units_",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator = "tenure_by_units_in_structure_owner_occupied_housing_units"))
))

register_table(list(
  name = "year_structure_built",
  description = "Year structure built",
  acs_tables = "B25034",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B25034"))),
  raw_variables = NULL,
  definitions = list(
    define_across_percent(
      input_regex = "year_structure_built_built_[0-9]",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator_function = function(column) { column %>% stringr::str_replace("[0-9].*", "universe") %>% stringr::str_replace("built_", "") }),
    define_percent("year_structure_built_built_since_1940_percent",
                   numerator_regex = "year_structure_built_built_(19[4-9]|2)",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_percent("year_structure_built_built_since_1950_percent",
                   numerator_regex = "year_structure_built_built_(19[5-9]|2)",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_percent("year_structure_built_built_since_1960_percent",
                   numerator_regex = "year_structure_built_built_(19[6-9]|2)",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_percent("year_structure_built_built_since_1970_percent",
                   numerator_regex = "year_structure_built_built_(19[7-9]|2)",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_percent("year_structure_built_built_since_1980_percent",
                   numerator_regex = "year_structure_built_built_(19[8-9]|2)",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_percent("year_structure_built_built_since_1990_percent",
                   numerator_regex = "year_structure_built_built_(19[9]|2)",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_percent("year_structure_built_built_since_2000_percent",
                   numerator_regex = "year_structure_built_built_(200|201|202)",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_percent("year_structure_built_built_since_2010_percent",
                   numerator_regex = "year_structure_built_built_(201|202)",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_percent("year_structure_built_built_since_2020_percent",
                   numerator_regex = "year_structure_built_built_202",
                   numerator_exclude_regex = "percent",
                   denominator_variables = c("year_structure_built_universe")),
    define_one_minus("year_structure_built_built_before_1960_percent",
                     source_variable = "year_structure_built_built_since_1960_percent"))
))

register_table(list(
  name = "cost_burden",
  description = "Household income by gross rent as a percentage of household income (housing cost burden)",
  acs_tables = "B25074",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B25074"))),
  raw_variables = NULL,
  definitions = list(
    define_percent("cost_burdened_30percentormore_allincomes_percent",
                   numerator_regex = "household_income_by_gross_rent.*(30_0|35_0|40_0|50_0).*(pct)",
                   denominator_regex = "household_income_by_gross_rent.*([0-9]$|100000_more$)",
                   subtract_regex = "household_income.*not_computed"),
    define_percent("cost_burdened_50percentormore_allincomes_percent",
                   numerator_regex = "household_income_by_gross_rent.*50_0.*pct",
                   denominator_regex = "household_income_by_gross_rent.*([0-9]$|100000_more$)",
                   subtract_regex = "household_income.*not_computed"),
    define_percent("cost_burdened_30percentormore_incomeslessthan35000_percent",
                   numerator_regex = "household_income_by_gross_rent.*(10000_|19999|34999).*(30_0|35_0|40_0|50_0).*(pct)",
                   denominator_regex = "household_income_by_gross_rent.*(10000|19999|34999)$",
                   subtract_regex = "household_income.*(10000_|19999|34999).*not_computed"),
    define_percent("cost_burdened_50percentormore_incomeslessthan35000_percent",
                   numerator_regex = "household_income_by_gross_rent.*(10000_|19999|34999).*50_0.*(pct)",
                   denominator_regex = "household_income_by_gross_rent.*(10000|19999|34999)$",
                   subtract_regex = "household_income.*(10000_|19999|34999).*not_computed"),
    define_percent("cost_burdened_30percentormore_incomeslessthan50000_percent",
                   numerator_regex = "household_income_by_gross_rent.*(10000_|19999|34999|49999).*(30_0|35_0|40_0|50_0).*(pct)",
                   denominator_regex = "household_income_by_gross_rent.*(10000|19999|34999|49999)$",
                   subtract_regex = "household_income.*(10000_|19999|34999|49999).*not_computed"),
    define_percent("cost_burdened_50percentormore_incomeslessthan50000_percent",
                   numerator_regex = "household_income_by_gross_rent.*(10000_|19999|34999|49999).*50_0.*pct",
                   denominator_regex = "household_income_by_gross_rent.*(10000|19999|34999|49999)$",
                   subtract_regex = "household_income.*(10000_|19999|34999|49999).*not_computed"))
))

register_table(list(
  name = "tenure_by_housing_costs",
  description = "Tenure by housing costs as a percentage of household income",
  acs_tables = "B25106",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B25106"))),
  raw_variables = NULL,
  definitions = list()
))

register_table(list(
  name = "median_housing_cost",
  description = "Median monthly housing costs",
  acs_tables = "B25105",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(housing_cost_monthly_median_ = "B25105_001"),
  definitions = list()
))

register_table(list(
  name = "median_income_by_tenure",
  description = "Median annual household income by tenure",
  acs_tables = "B25119",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    median_household_income_in_past_12_months_by_tenure_universe_ = "B25119_001",
    median_household_income_in_past_12_months_by_tenure_owner_occupied_ = "B25119_002",
    median_household_income_in_past_12_months_by_tenure_renter_occupied_ = "B25119_003"),
  definitions = list()
))

register_table(list(
  name = "mortgage_status",
  description = "Mortgage status",
  acs_tables = "B25081",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    mortgage_status_universe_ = "B25081_001",
    mortgage_status_housing_units_with_mortgage_ = "B25081_002"),
  definitions = list()
))

####----TABLE REGISTRATIONS: TRANSPORTATION----####

register_table(list(
  name = "transportation_to_work",
  description = "Means of transportation to work",
  acs_tables = "B08301",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B08301_"))),
  raw_variables = NULL,
  definitions = list(
    define_across_percent(
      input_regex = "means_transportation",
      exclude_regex = "universe|worked_from_home|percent",
      output_suffix = "_percent",
      denominator = "means_transportation_work_universe",
      denominator_subtract = "means_transportation_work_worked_from_home"),
    define_percent("means_transportation_work_worked_from_home_percent",
                   numerator = "means_transportation_work_worked_from_home",
                   denominator = "means_transportation_work_universe"),
    define_percent("means_transportation_work_bicycle_walked_percent",
                   numerator_regex = "means_transportation_work_(bicycle|walked)$",
                   denominator_variables = c("means_transportation_work_universe"),
                   subtract_variables = c("means_transportation_work_worked_from_home")),
    define_percent("means_transportation_work_motor_vehicle_percent",
                   numerator_regex = "means_transportation_work_(car_truck_van|taxicab|motorcycle)$",
                   denominator_variables = c("means_transportation_work_universe"),
                   subtract_variables = c("means_transportation_work_worked_from_home")))
))

register_table(list(
  name = "travel_time_to_work",
  description = "Travel time to work",
  acs_tables = "B08303",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B08303_"))),
  raw_variables = NULL,
  definitions = list(
    define_across_percent(
      input_regex = "travel_time_work",
      exclude_regex = "universe|percent",
      output_suffix = "_percent",
      denominator = "travel_time_work_universe"))
))

register_table(list(
  name = "vehicles_available",
  description = "Tenure by vehicles available",
  acs_tables = "B25044",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B25044_"))),
  raw_variables = NULL,
  definitions = list()
))

####----TABLE REGISTRATIONS: EDUCATION----####

register_table(list(
  name = "educational_attainment",
  description = "Educational attainment for the population 25 years and over",
  acs_tables = "B15003",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B15003"))),
  raw_variables = NULL,
  definitions = list(
    define_percent("educational_attainment_highschool_none_percent",
                   numerator_regex = "educational_attainment.*(no_schooling|nursery|kindergarten|_[0-8]th_grade|1st_grade|2nd_grade|3rd_grade)",
                   denominator_variables = c("educational_attainment_population_25_years_over_universe")),
    define_percent("educational_attainment_highschool_nodiploma_percent",
                   numerator_regex = "educational_attainment.*(9th|10th|11th|12th)",
                   denominator_variables = c("educational_attainment_population_25_years_over_universe")),
    define_percent("educational_attainment_ged_percent",
                   numerator = "educational_attainment_population_25_years_over_ged_alternative_credential",
                   denominator = "educational_attainment_population_25_years_over_universe"),
    define_percent("educational_attainment_highschool_diploma_percent",
                   numerator = "educational_attainment_population_25_years_over_regular_high_school_diploma",
                   denominator = "educational_attainment_population_25_years_over_universe"),
    define_percent("educational_attainment_college_some_percent",
                   numerator_regex = "educational_attainment.*some_college",
                   denominator_variables = c("educational_attainment_population_25_years_over_universe")),
    define_percent("educational_attainment_degree_associate_percent",
                   numerator = "educational_attainment_population_25_years_over_associates_degree",
                   denominator = "educational_attainment_population_25_years_over_universe"),
    define_percent("educational_attainment_degree_bachelors_percent",
                   numerator = "educational_attainment_population_25_years_over_bachelors_degree",
                   denominator = "educational_attainment_population_25_years_over_universe"),
    define_percent("educational_attainment_degree_morethanbachelors_percent",
                   numerator_regex = "educational_attainment.*(masters|professional|doctorate)",
                   denominator_variables = c("educational_attainment_population_25_years_over_universe")))
))

register_table(list(
  name = "school_enrollment",
  description = "School enrollment by detailed level of school",
  acs_tables = "B14007",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    school_enrollment_universe_ = "B14007_001",
    school_enrollment_prek_ = "B14007_003",
    school_enrollment_kindergarten_ = "B14007_004",
    school_enrollment_undergraduate_ = "B14007_017",
    school_enrollment_graduate_ = "B14007_018",
    school_enrollment_notenrolled_ = "B14007_019"),
  definitions = list(
    define_percent("educational_enrollment_grades_1thru12_percent",
                   numerator_variables = c("school_enrollment_universe"),
                   numerator_note = "universe minus non-1-12 enrollment categories",
                   denominator_variables = c("school_enrollment_universe"),
                   numerator_subtract_regex = "school_enrollment.*[^(_universe)]"),
    define_across_percent(
      input_regex = "school_enrollment.*[^(_universe)]",
      exclude_regex = "percent",
      output_suffix = "_percent",
      denominator = "school_enrollment_universe"))
))

####----TABLE REGISTRATIONS: NATIVITY AND LANGUAGE----####

register_table(list(
  name = "nativity_language",
  description = "Nativity by language spoken at home by ability to speak English",
  acs_tables = "B16005",
  depends_on = character(0),
  constructs = list(
    list(name = "nativity", variable_pattern = "^nativity_|^nativity_by_language"),
    list(name = "language", variable_pattern = "^ability_speak_english_")
  ),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B16005_",
           filter = list(
             match_string = "universe_$|native_$|foreign_born_$|only|very_well",
             match_type = "positive")))),
  raw_variables = NULL,
  definitions = list(
    define_percent("nativity_native_born_percent",
                   numerator = "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_native",
                   denominator = "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe"),
    define_percent("nativity_foreign_born_percent",
                   numerator = "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_foreign_born",
                   denominator = "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe"),
    define_percent("ability_speak_english_very_well_better_percent",
                   numerator_regex = "nativity.*(only_english|english_very_well)",
                   denominator_variables = c("nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe")),
    define_one_minus("ability_speak_english_less_than_very_well_percent",
                     source_variable = "ability_speak_english_very_well_better_percent"))
))

####----TABLE REGISTRATIONS: EMPLOYMENT----####

register_table(list(
  name = "employment",
  description = "Employment status for the population 16 years and over",
  acs_tables = "B23025",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    employment_civilian_labor_force_universe_ = "B23025_003",
    employment_civilian_labor_force_employed_ = "B23025_004"),
  definitions = list(
    define_percent("employment_civilian_labor_force_percent",
                   numerator = "employment_civilian_labor_force_employed",
                   denominator = "employment_civilian_labor_force_universe"))
))

####----TABLE REGISTRATIONS: HOUSEHOLD COMPOSITION----####

register_table(list(
  name = "household_size",
  description = "Average household size by tenure",
  acs_tables = "B25010",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    household_size_average_ = "B25010_001",
    household_size_average_owneroccupied_ = "B25010_002",
    household_size_average_renteroccupied_ = "B25010_003"),
  definitions = list()
))

####----TABLE REGISTRATIONS: HEALTH INSURANCE----####

register_table(list(
  name = "health_insurance",
  description = "Health insurance coverage status and type by employment status",
  acs_tables = "B27011",
  depends_on = character(0),
  raw_variable_source = list(
    type = "select_variables",
    calls = list(
      list(pattern = "B27011"))),
  raw_variables = NULL,
  definitions = list(
    define_percent("health_insurance_coverage_status_covered_percent",
                   numerator_regex = "health_insurance_coverage_status_type_by_employment_status.*with_health_insurance_coverage$",
                   denominator_variables = c("health_insurance_coverage_status_type_by_employment_status_universe")),
    define_one_minus("health_insurance_coverage_status_notcovered_percent",
                     source_variable = "health_insurance_coverage_status_covered_percent"),
    define_percent("health_insurance_coverage_status_covered_employed_percent",
                   numerator_regex = "health_insurance_coverage_status_type_by_employment_status.*_employed.*with_health_insurance_coverage$",
                   denominator_variables = c("health_insurance_coverage_status_type_by_employment_status_in_labor_force")),
    define_percent("health_insurance_coverage_status_covered_unemployed_percent",
                   numerator_regex = "health_insurance_coverage_status_type_by_employment_status.*_unemployed.*with_health_insurance_coverage$",
                   denominator_variables = c("health_insurance_coverage_status_type_by_employment_status_in_labor_force")))
))

####----TABLE REGISTRATIONS: DIGITAL INFRASTRUCTURE----####

register_table(list(
  name = "internet",
  description = "Presence and types of internet subscriptions in household",
  acs_tables = "B28002",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    internet_subscription_household_universe_ = "B28002_001",
    internet_subscription_household_with_subscription_ = "B28002_002",
    internet_subscription_household_with_subscription_broadband_ = "B28002_004"),
  definitions = list()
))

register_table(list(
  name = "computing_devices",
  description = "Types of computers in household",
  acs_tables = "B28001",
  depends_on = character(0),
  raw_variable_source = list(type = "manual"),
  raw_variables = c(
    types_of_computing_devices_household_universe_ = "B28001_001",
    types_of_computing_devices_household_with_any_device_ = "B28001_002",
    types_of_computing_devices_household_with_computer_laptop_ = "B28001_003",
    types_of_computing_devices_household_with_smartphone_ = "B28001_005"),
  definitions = list()
))

####----TABLE REGISTRATIONS: POPULATION DENSITY----####

register_table(list(
  name = "population_density",
  description = "Population density (requires tigris geometry)",
  acs_tables = character(0),
  depends_on = "total_population",
  raw_variable_source = list(type = "manual"),
  raw_variables = NULL,
  definitions = list(
    define_metadata("population_density_land_sq_kilometer",
                    definition_text = "Rate. Numerator: total_population_universe (B01003_001). Denominator: area_land_sq_kilometer."))
))

####----GLOBAL VARIABLES----####

utils::globalVariables(c(
  "race_universe", "race_nonhispanic_white_alone_percent",
  "sex_by_age_female", "sex_by_age_male", "sex_by_age_universe",
  "age_under_5_years", "age_5_9_years", "age_10_14_years", "age_15_17_years",
  "sex_by_age_by_disability_status_universe",
  "tenure_universe", "tenure_by_occupants_per_room_universe",
  "tenure_by_occupants_per_room_renter_occupied",
  "units_in_structure_universe",
  "tenure_by_units_in_structure_renter_occupied_housing_units",
  "tenure_by_units_in_structure_renter_owner_occupied_housing_units",
  "tenure_by_units_in_structure_owner_occupied_housing_units",
  "year_structure_built_universe", "year_structure_built_built_since_1960_percent",
  "means_transportation_work_universe", "means_transportation_work_worked_from_home",
  "travel_time_work_universe",
  "educational_attainment_population_25_years_over_universe",
  "educational_attainment_population_25_years_over_ged_alternative_credential",
  "educational_attainment_population_25_years_over_regular_high_school_diploma",
  "educational_attainment_population_25_years_over_associates_degree",
  "educational_attainment_population_25_years_over_bachelors_degree",
  "school_enrollment_universe",
  "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_native",
  "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_foreign_born",
  "nativity_by_language_spoken_at_home_by_ability_speak_english_population_5_years_over_universe",
  "ability_speak_english_very_well_better_percent",
  "employment_civilian_labor_force_employed", "employment_civilian_labor_force_universe",
  "health_insurance_coverage_status_type_by_employment_status_universe",
  "health_insurance_coverage_status_covered_percent",
  "health_insurance_coverage_status_type_by_employment_status_in_labor_force",
  "snap_received", "snap_universe", "public_assistance_received", "public_assistance_universe",
  "total_population_universe", "area_land_sq_kilometer",
  "indicator"))
