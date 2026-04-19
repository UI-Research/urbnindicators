#' @importFrom magrittr %>%

####----AUTO-PERCENTAGE COMPUTATION FOR ARBITRARY ACS TABLES----####

## Detect whether a string looks like a raw ACS table code (e.g., "B25070", "C15002B", "B01001APR")
is_raw_acs_code = function(x) {
  grepl("^[BC][0-9]{5}[A-I]?(?:PR)?$", x, perl = TRUE)
}

## Resolve a user-supplied string to an ACS table code.
## Accepts:
##   1. A raw ACS code ("B25070") -> returned as-is
##   2. A cleaned variable name from clean_acs_names() -> resolved to parent table
## Returns the table code, or NULL if not resolvable.
resolve_to_acs_table = function(x, year, census_variables = NULL) {
  if (is_raw_acs_code(x)) return(x)

  ## load census codebook if not provided
  if (is.null(census_variables)) {
    suppressMessages({suppressWarnings({
      census_variables = tidycensus::load_variables(year = year, dataset = "acs5")
    })})
  }

  ## apply clean_acs_names and search for a match
  cleaned = census_variables %>% clean_acs_names()
  clean_col = cleaned$clean_names %>% stringr::str_remove("_$")

  ## exact match first
  match_idx = which(clean_col == x)
  if (length(match_idx) == 0) {
    ## try partial match (user gives a prefix)
    match_idx = which(stringr::str_starts(clean_col, stringr::fixed(x)))
  }

  if (length(match_idx) == 0) return(NULL)

  ## extract the ACS table code from the variable name (e.g., "B25070_001" -> "B25070")
  acs_name = cleaned$name[match_idx[1]]
  table_code = stringr::str_extract(acs_name, "^[BC][0-9]{5}[A-I]?(?:PR)?")
  return(table_code)
}

## Build a label tree for a single ACS table.
## Takes a data frame filtered to one table (from tidycensus::load_variables)
## with clean_acs_names() already applied.
## Returns the data frame with additional columns: segments, depth, is_total,
## is_subtotal, parent_code, parent_clean_name.
build_label_tree = function(variables_df) {
  ## parse label segments (split on !!)
  variables_df = variables_df %>%
    dplyr::mutate(
      segments = stringr::str_split(label, "!!"),
      depth = purrr::map_int(segments, length),
      is_total = stringr::str_detect(name, "_001$"),
      is_subtotal = stringr::str_detect(label, ":$") & !is_total,
      clean_name_trimmed = stringr::str_remove(clean_names, "_$"))

  ## assign parent for each variable
  ## for variable i, walk backward to find the nearest ancestor subtotal
  ## whose segments are a strict prefix of this variable's segments
  n = nrow(variables_df)
  total_name = variables_df$name[1]
  total_clean = variables_df$clean_name_trimmed[1]

  parent_results = purrr::map(seq_len(n), function(i) {
    if (variables_df$is_total[i]) {
      return(list(parent_code = NA_character_, parent_clean_name = NA_character_))
    }

    current_segments = variables_df$segments[[i]]
    candidates = rev(seq_len(i - 1))

    ## find the nearest ancestor whose segments are a strict prefix
    match_idx = purrr::detect(candidates, function(j) {
      candidate_segments = variables_df$segments[[j]]
      length(candidate_segments) < length(current_segments) &&
        all(candidate_segments == current_segments[seq_along(candidate_segments)]) &&
        (variables_df$is_subtotal[j] || variables_df$is_total[j])
    })

    if (!is.null(match_idx)) {
      list(parent_code = variables_df$name[match_idx],
           parent_clean_name = variables_df$clean_name_trimmed[match_idx])
    } else {
      ## fallback to _001 (table total)
      list(parent_code = total_name, parent_clean_name = total_clean)
    }
  })

  variables_df$parent_code = purrr::map_chr(parent_results, "parent_code")
  variables_df$parent_clean_name = purrr::map_chr(parent_results, "parent_clean_name")
  return(variables_df)
}

## Classify an ACS table as "count" (percentages appropriate) or "skip" (not appropriate).
## Detection based on concept field and the _001 label.
classify_acs_table = function(nodes) {
  concept = nodes$concept[1]
  total_label = nodes$label[nodes$is_total][1]

  concept_lower = tolower(concept)
  label_lower = tolower(total_label)

  ## patterns that indicate non-percentage-amenable tables
  skip_patterns = c(
    "median", "aggregate", "average", "mean",
    "allocation of", "imputation of",
    "margin of error")

  has_skip_pattern = purrr::some(skip_patterns, function(pattern) {
    grepl(pattern, concept_lower, fixed = TRUE) ||
      grepl(pattern, label_lower, fixed = TRUE)
  })
  if (has_skip_pattern) return("skip")

  ## singleton tables (only one variable) — no meaningful percentages
  if (nrow(nodes) <= 1) return("skip")

  ## tables where the total is not a count (e.g., median income tables may have
  ## a numeric label rather than "Estimate!!Total:")
  if (!grepl(":", total_label) && !grepl("^Estimate!!Total$", total_label)) {
    return("skip")
  }

  return("count")
}

## Generate simple_percent definitions for auto-computed tables.
## For each non-total variable, produces a define_percent() call.
## denominator_mode: "parent" (nearest parent subtotal), "total" (_001), or a specific ACS variable code.
generate_auto_definitions = function(nodes, denominator_mode = "parent",
                                     custom_denominator = NULL) {
  ## only process non-total variables
  leaf_nodes = nodes %>% dplyr::filter(!is_total)

  if (nrow(leaf_nodes) == 0) return(list())

  ## determine total row clean name (for "total" mode or fallback)
  total_clean_name = nodes$clean_name_trimmed[nodes$is_total][1]

  ## if a custom denominator ACS code is given, find its clean name
  custom_denom_clean = NULL
  if (!is.null(custom_denominator)) {
    match_row = nodes %>% dplyr::filter(name == custom_denominator)
    if (nrow(match_row) > 0) {
      custom_denom_clean = match_row$clean_name_trimmed[1]
    } else {
      rlang::warn(paste0("Custom denominator '", custom_denominator,
                         "' not found in table. Falling back to table total."))
      denominator_mode = "total"
    }
  }

  purrr::map(seq_len(nrow(leaf_nodes)), function(i) {
    row = leaf_nodes[i, ]
    numerator = row$clean_name_trimmed

    ## determine denominator
    if (!is.null(custom_denom_clean)) {
      denominator = custom_denom_clean
    } else if (denominator_mode == "total") {
      denominator = total_clean_name
    } else {
      ## "parent" mode: use parent_clean_name, fall back to total
      denominator = row$parent_clean_name
      if (is.na(denominator)) denominator = total_clean_name
    }

    ## skip if numerator == denominator (the total itself as a subtotal)
    if (identical(numerator, denominator)) return(NULL)

    ## raw variables ending in _pct (renamed from _percent by clean_acs_names):
    ## replace _pct with _percent so the computed column gets the standard suffix
    if (grepl("_pct$", numerator)) {
      output = sub("_pct$", "_percent", numerator)
    } else {
      output = paste0(numerator, "_percent")
    }
    list(type = "simple_percent", output = output,
         numerator = numerator, denominator = denominator)
  }) %>% purrr::compact()
}

## Orchestrator: build a complete auto-table entry from an ACS table code.
## Returns a list with the same shape as register_table() entries, plus is_auto = TRUE.
## Pass census_variables to avoid redundant tidycensus::load_variables() calls.
build_auto_table_entry = function(table_code, year, denominator_mode = "parent",
                                  custom_denominator = NULL,
                                  census_variables = NULL) {
  ## load variables only if not provided
  if (is.null(census_variables)) {
    suppressMessages({suppressWarnings({
      census_variables = tidycensus::load_variables(year = year, dataset = "acs5")
    })})
  }

  table_vars = census_variables %>%
    dplyr::filter(stringr::str_detect(name, paste0("^", table_code, "_")))

  if (nrow(table_vars) == 0) {
    stop(paste0("ACS table '", table_code, "' not found in the ", year,
                " 5-year ACS. Check the table code."))
  }

  ## apply clean_acs_names
  table_vars = table_vars %>% clean_acs_names()

  ## build label tree
  nodes = build_label_tree(table_vars)

  ## classify table
  table_type = classify_acs_table(nodes)

  ## generate definitions
  if (table_type == "count") {
    definitions = generate_auto_definitions(
      nodes,
      denominator_mode = denominator_mode,
      custom_denominator = custom_denominator)
  } else {
    definitions = list()
  }

  ## build raw_variables named vector (clean_name_ -> ACS code)
  raw_variables = stats::setNames(nodes$name, paste0(nodes$clean_name_trimmed, "_"))

  list(
    name = table_code,
    description = nodes$concept[1],
    acs_tables = table_code,
    depends_on = character(0),
    raw_variable_source = list(type = "manual"),
    raw_variables = raw_variables,
    definitions = definitions,
    is_auto = TRUE,
    table_type = table_type)
}

utils::globalVariables(c(
  "clean_names", "is_total", "clean_name_trimmed",
  "segments", "depth", "is_subtotal", "parent_code", "parent_clean_name"))
