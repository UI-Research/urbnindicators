#' @title Division without NaNs
#' @description A division operation that distinguishes structurally-zero ratios
#'    from undefined ones.
#' @details Returns the quotient \code{x / y} except in two cases:
#'    \itemize{
#'      \item When both \code{x} and \code{y} are \code{0}, returns \code{0}
#'        (treating \code{0 / 0} as a structurally-zero ratio rather than NaN).
#'      \item When \code{y} is \code{0} and \code{x} is non-zero (positive or
#'        negative), returns \code{NA_real_} (the ratio is undefined).
#'    }
#'    When \code{x} or \code{y} is \code{NA}, the result is \code{NA}.
#' @param x A numeric vector or scalar.
#' @param y A numeric vector or scalar.
#' @returns A numeric vector. See \code{Details} for the behavior at
#'    \code{y == 0}.
#' @examples
#' safe_divide(1, 2)   # 0.5
#' safe_divide(0, 0)   # 0
#' safe_divide(3, 0)   # NA
#' safe_divide(3, NA)  # NA
#' @export
safe_divide = function(x, y) {
  dplyr::if_else(
    y == 0,
    dplyr::if_else(x == 0, 0, NA_real_),
    x / y)
}

## Best-guess of the most recent ACS 5-year vintage that should be available.
## The Census Bureau releases the (Y-4)..(Y) 5-year ACS in December of year
## Y+1, so as of mid-year X the most recent published vintage is roughly X-2.
latest_acs_year = function() {
  as.numeric(format(Sys.Date(), "%Y")) - 2
}

## Internal helper: fetch raw ACS estimates across years, states, and counties.
## One tidycensus call per (state, year), with vector `county =` when the user
## supplied a counties subset and the geography supports county filtering.
fetch_acs = function(geography, variables, years, states, counties,
                     county_codes, super_state_geographies) {
  if (geography %in% super_state_geographies) {
    return(
      purrr::map(years, function(year) {
        tidycensus::get_acs(
          geography = geography, variables = variables,
          year = as.numeric(year), survey = "acs5", output = "wide") %>%
          dplyr::mutate(data_source_year = year)
      }) %>% purrr::list_rbind())
  }

  county_filterable = geography %in% c("tract", "county", "county subdivision", "block group")
  user_supplied_counties = length(counties) > 0

  purrr::map(states, function(state) {
    purrr::map(years, function(year) {
      args = list(
        geography = geography, variables = variables,
        year = as.numeric(year), state = state,
        survey = "acs5", output = "wide")

      ## block-group queries are always scoped by county (county_codes holds all
      ## counties in the state when the user did not supply a subset); other
      ## county-filterable geographies are scoped only when the user asked
      if (geography == "block group" || (user_supplied_counties && county_filterable)) {
        county_vec = county_codes %>%
          dplyr::filter(state == !!state) %>%
          dplyr::pull(county)
        if (length(county_vec) > 0) args$county = county_vec
      }

      do.call(tidycensus::get_acs, args) %>%
        dplyr::mutate(data_source_year = year)
    }) %>% purrr::list_rbind()
  }) %>% purrr::list_rbind()
}

#' @title Analysis-ready social science measures
#' @description Construct measures frequently used in social sciences
#'    research, leveraging \code{tidycensus::get_acs()} to acquire raw estimates from
#'    the Census Bureau API.
#' @param tables A character vector, list, or NULL specifying which data to
#'    include. Three kinds of elements are accepted and can be mixed freely
#'    inside a \code{list()}:
#'    \itemize{
#'      \item \strong{Registered table names} (e.g., \code{"race"}, \code{"snap"}).
#'        These are pre-built tables with curated variable definitions. Use
#'        \code{list_tables()} to see all available registered tables.
#'      \item \strong{Raw ACS table codes} (e.g., \code{"B25070"}, \code{"C15002B"}).
#'        Any valid ACS Detailed or Collapsed table code can be passed directly.
#'        These are auto-processed at runtime: raw variables are fetched, the
#'        label hierarchy is parsed, and percentages are computed automatically.
#'        Use the \code{denominator} parameter to control how percentages are
#'        calculated for these tables.
#'      \item \strong{DSL definition objects} created with \code{\link{define_percent}},
#'        \code{\link{define_sum}}, \code{\link{define_complement}}, or
#'        \code{\link{define_metadata}}. These let you compute custom derived
#'        variables from the columns produced by the tables you request. User
#'        definitions are executed after all registered and auto-table
#'        definitions, and their results appear in the codebook and have MOEs
#'        computed automatically.
#'    }
#'    When mixing strings and definitions, wrap everything in \code{list()}
#'    (e.g., \code{list("snap", define_percent(...))}).
#'    If an ACS code corresponds to an already-registered table, the registered
#'    version is used automatically.
#'    When NULL (default), all registered tables are included (unregistered ACS
#'    tables must be requested explicitly).
#' @param years A numeric vector of four-digit years for which to pull five-year
#'    American Community Survey estimates.
#' @param geography A geography type that is accepted by \code{tidycensus::get_acs()}, e.g.,
#'    "tract", "county", "state", among others. \code{"block group"} is supported for
#'    years 2013 and later and requires an explicit \code{states} argument; because the
#'    ACS publishes only a limited subset of tables at the block-group level, requested
#'    tables that are not available there are dropped with a warning (use
#'    \code{list_tables(geography = "block group")} to see what is available). Block-group
#'    estimates carry large margins of error and should be used with care. Census blocks
#'    (geography = "block") are not supported, as the ACS publishes no block-level data.
#' @param states A vector of one or more state names, abbreviations, or codes as
#'    accepted by \code{tidycensus::get_acs()}.
#' @param counties A vector of five-digit county FIPS codes. If specified, this parameter
#'    will override the \code{states} parameter. If \code{NULL}, all counties in the the
#'    state(s) specified in the \code{states} parameter will be included.
#' @param spatial Boolean. Return a simple features (sf), spatially-enabled dataframe?
#' @param denominator Controls how auto-computed percentages choose their
#'    denominator. \code{"parent"} (default) uses the nearest parent subtotal from
#'    the ACS label hierarchy. \code{"total"} uses the table total (variable
#'    \code{_001}). A specific ACS variable code (e.g., \code{"B25070_001"}) uses
#'    that variable. Only affects unregistered (auto) tables; registered tables
#'    always use their predefined definitions.
#' @param ... Deprecated arguments. If \code{variables} is passed, a deprecation
#'    warning is issued and the value is ignored.
#' @seealso \code{tidycensus::get_acs()}, which this function wraps.
#' @returns A dataframe containing the requested variables, their MOEs,
#'    a series of derived variables, such as percentages, and the year of the data.
#'    Returned data are formatted wide. A codebook is attached as an attribute
#'    and can be accessed via \code{compile_acs_data() \%>\% attr("codebook")}.
#'    The codebook is a tibble with these columns (treated as a stable interface):
#'    \itemize{
#'      \item \code{calculated_variable} - the column name in the returned data
#'      \item \code{variable_type} - one of \code{"Count"}, \code{"Percent"},
#'        \code{"Sum"}, \code{"Median"}, \code{"Median ($)"}, \code{"Average"},
#'        \code{"Quintile ($)"}, \code{"Index"}, \code{"Metadata"}
#'      \item \code{definition} - human-readable description of the variable
#'      \item \code{numerator_vars}, \code{numerator_subtract_vars},
#'        \code{denominator_vars}, \code{denominator_subtract_vars} - list-columns
#'        of clean column names used in the numerator/denominator (positive and
#'        subtractive terms) of a derived variable
#'      \item \code{se_calculation_type} - one of \code{"raw"}, \code{"sum"},
#'        \code{"simple_percent"}, \code{"complex_numerator"},
#'        \code{"complex_denominator"}, \code{"complex_both"}, \code{"one_minus"},
#'        \code{"weighted_average"}, \code{"metadata"}, \code{"unknown"};
#'        indicates which MOE-propagation formula is appropriate
#'      \item \code{aggregation_strategy} - one of \code{"sum"},
#'        \code{"recalculate_percent"}, \code{"weighted_average"},
#'        \code{"metadata"}, \code{"unknown"}; used by \code{interpolate_acs()}
#'    }
#'    The resolved tables are also attached as a \code{"resolved_tables"}
#'    attribute (used by \code{interpolate_acs()}).
#' @examples
#' \dontrun{
#' ## Pull all tables (default, backward-compatible)
#' df = compile_acs_data(years = c(2022), geography = "county", states = "NJ")
#'
#' ## Pull specific tables
#' df = compile_acs_data(tables = c("race", "snap"), years = 2022,
#'                       geography = "county", states = "NJ")
#'
#' ## Pull block-group data (2013+, requires states; unavailable tables are dropped)
#' df = compile_acs_data(tables = c("race", "tenure"), years = 2022,
#'                       geography = "block group", states = "NJ")
#'
#' ## Pull an unregistered ACS table by code
#' df = compile_acs_data(tables = "B25070", years = 2022,
#'                       geography = "state", states = "DC")
#'
#' ## Mix registered and unregistered tables
#' df = compile_acs_data(tables = c("snap", "B25070"), years = 2022,
#'                       geography = "state", states = "DC")
#'
#' ## Use table total as denominator instead of parent subtotals
#' df = compile_acs_data(tables = "B25070", denominator = "total",
#'                       years = 2022, geography = "state", states = "DC")
#'
#' ## Add a custom derived variable alongside a registered table
#' df = compile_acs_data(
#'   tables = list(
#'     "snap",
#'     define_percent("snap_universe", "snap_universe",
#'                    subtract_from_numerator = "snap_received",
#'                    output = "snap_not_received_percent")),
#'   years = 2022, geography = "county", states = "DC")
#'   }
#' @export
#' @importFrom magrittr %>%

compile_acs_data = function(
    tables = NULL,
    years = latest_acs_year(),
    geography = "county",
    states = NULL,
    counties = NULL,
    spatial = FALSE,
    denominator = "parent",
    ...) {

  ## handle deprecated `variables` parameter and unknown arguments
  dots = list(...)
  if ("variables" %in% names(dots)) {
    lifecycle::deprecate_warn(
      when = "0.1.0",
      what = "compile_acs_data(variables)",
      details = "The `variables` parameter is ignored. Use `tables` to select specific data, or call with no selection arguments for all tables."
    )
  }
  unknown_args = setdiff(names(dots), "variables")
  if (length(unknown_args) > 0) {
    cli::cli_warn(
      "Unknown argument{?s} passed to {.fun compile_acs_data}: {.arg {unknown_args}}. {?It/They} will be ignored."
    )
  }

  old_tigris_cache = getOption("tigris_use_cache")
  options(tigris_use_cache = TRUE)
  on.exit(options(tigris_use_cache = old_tigris_cache), add = TRUE)

  ## normalize geography to lowercase for consistent downstream comparisons
  geography = tolower(geography)

  ## validate years
  years = as.numeric(years)
  if (any(is.na(years)) || any(years != as.integer(years)) || any(nchar(as.integer(years)) != 4)) {
    cli::cli_abort("{.arg years} must be a vector of four-digit integers (e.g., 2022).")
  }
  if (any(years < 2009) || any(years > as.numeric(format(Sys.Date(), "%Y")))) {
    cli::cli_abort("{.arg years} must be between 2009 (earliest 5-year ACS) and the current year.")
  }

  ## validate denominator parameter
  valid_denominator = denominator %in% c("parent", "total") ||
    stringr::str_detect(denominator, "^[BC][0-9]{5}[A-I]?(_[0-9]{3})?$")
  if (!valid_denominator) {
    cli::cli_abort("{.arg denominator} must be {.val parent}, {.val total}, or a valid ACS variable code (e.g., {.val B25070_001}). Got: {.val {denominator}}.")
  }

  ####----Partition tables into registry vs auto vs user definitions----####
  auto_table_entries = list()
  registry_tables = tables
  raw_acs_codes = character(0)
  user_definitions = list()
  has_explicit_tables = !is.null(tables)

  if (!is.null(tables)) {
    ## separate DSL definitions from string elements
    if (is.list(tables) && !is.character(tables)) {
      user_definitions = purrr::keep(tables, is_dsl_definition)
      string_elements = purrr::keep(tables, function(x) is.character(x) && length(x) == 1)
      tables = if (length(string_elements) > 0) as.character(string_elements) else NULL
    }

    ## validate user definitions structurally (fail fast)
    purrr::walk(user_definitions, validate_definition)
  }

  if (!is.null(tables)) {
    construct_map = build_construct_map()
    internal_names = names(.table_registry$tables)

    ## load census variables once for resolve_to_acs_table lookups
    suppressMessages({suppressWarnings({
      census_variables_for_resolve = load_acs_variables(year = max(years), dataset = "acs5")
    })})

    ## collect all acs_tables from registered tables to detect overlap
    registered_acs_codes = purrr::map(internal_names, function(tn) {
      entry = get_table(tn)
      if (!is.null(entry[["acs_tables"]])) entry[["acs_tables"]] else character(0)
    }) %>% unlist() %>% unique()

    ## helper: find the registered table covering a given ACS code
    find_covering_table = function(acs_code) {
      purrr::detect(internal_names, function(tn) {
        entry = get_table(tn)
        acs_code %in% entry[["acs_tables"]]
      })
    }

    ## classify each user-supplied table name
    classified = purrr::map(tables, function(tbl) {
      if (tbl %in% internal_names || tbl %in% names(construct_map)) {
        ## known registry table or construct name
        return(list(type = "registry", value = tbl))
      }
      if (is_raw_acs_code(tbl)) {
        ## raw ACS table code — check for overlap with registered tables
        if (tbl %in% registered_acs_codes) {
          covering = find_covering_table(tbl)
          if (!is.null(covering)) return(list(type = "registry", value = covering))
        }
        return(list(type = "auto", value = tbl))
      }
      ## try resolving as a cleaned variable name
      resolved_code = resolve_to_acs_table(tbl, year = max(years),
                                           census_variables = census_variables_for_resolve)
      if (!is.null(resolved_code)) {
        if (resolved_code %in% registered_acs_codes) {
          covering = find_covering_table(resolved_code)
          if (!is.null(covering)) return(list(type = "registry", value = covering))
        }
        return(list(type = "auto", value = resolved_code))
      }
      ## not resolvable — pass through to resolve_tables() which will error if invalid
      list(type = "registry", value = tbl)
    })

    registry_tables = purrr::map_chr(
      purrr::keep(classified, ~ .x$type == "registry"), "value") %>% unique()
    raw_acs_codes = purrr::map_chr(
      purrr::keep(classified, ~ .x$type == "auto"), "value") %>% unique()
  }

  ####----Resolve tables and variables via the registry----####
  ## resolve which tables to include
  if (is.null(tables) && !has_explicit_tables) {
    ## default: all internal table names (user passed tables = NULL)
    resolved_tables = names(.table_registry$tables)
  } else if (is.null(tables) && has_explicit_tables) {
    ## user passed only definitions, no string tables — include total_population only
    resolved_tables = "total_population"
  } else {
    ## pass only registry tables (not raw ACS codes) to resolve_tables
    registry_tables_input = if (length(registry_tables) > 0) registry_tables else NULL
    resolved_tables = resolve_tables(tables = registry_tables_input)
  }

  ## determine whether tigris geometry is needed
  needs_tigris = isTRUE(spatial) || ("population_density" %in% resolved_tables)

  ####----Build auto table entries for raw ACS codes----####
  if (length(raw_acs_codes) > 0) {
    ## determine denominator mode and custom denominator
    denominator_mode = denominator
    custom_denominator = NULL
    if (!denominator %in% c("parent", "total")) {
      denominator_mode = "custom"
      custom_denominator = denominator
    }

    suppressMessages({suppressWarnings({
      auto_table_entries = purrr::map(raw_acs_codes, function(code) {
        build_auto_table_entry(
          table_code = code,
          year = max(years),
          denominator_mode = denominator_mode,
          custom_denominator = custom_denominator,
          census_variables = census_variables_for_resolve)
      })
    })})
    names(auto_table_entries) = raw_acs_codes
  }

  ####----Block-group geography: validate and restrict to available tables----####
  ## census blocks carry no ACS data; reject early (before any data query)
  if (tolower(geography) == "block") {
    cli::cli_abort("Block-level geography is not supported; the ACS does not publish estimates for census blocks.")
  }

  ## The ACS publishes only a limited subset of tables at the block-group level.
  ## Availability is read from the codebook `geography` column (each variable's
  ## lowest published geography), so no live data query is needed to decide it.
  is_block_group = (tolower(geography) == "block group")
  bg_codebook = NULL

  if (is_block_group) {
    geography = "block group"  ## normalize for tidycensus / tigris

    ## cartographic block-group boundaries (tigris cb = TRUE) begin in 2013
    if (min(years) < 2013) {
      cli::cli_abort(c(
        "Block-group geography is supported only for years 2013 and later
        (the earliest year of cartographic block-group boundaries).",
        "i" = "Requested earliest year: {min(years)}."))
    }

    ## block groups number ~240,000 nationwide; require an explicit state selection
    if (length(states) == 0) {
      cli::cli_abort(c(
        "Block-group queries require an explicit {.arg states} argument (e.g., {.code states = \"NJ\"}).",
        "i" = "National block-group pulls are not supported."))
    }

    bg_codebook = load_acs_variables(year = max(years), dataset = "acs5")

    ## registry tables: partition into available / dropped / partial
    partition = bg_partition_tables(resolved_tables, bg_codebook)

    if (length(partition[["dropped"]]) > 0) {
      cli::cli_warn(
        "These tables are not published at the block-group level and have been dropped: {.val {sort(partition[['dropped']])}}.")
    }
    if (length(partition[["partial"]]) > 0) {
      partial_messages = purrr::imap_chr(partition[["partial"]], function(dropped_vars, table_name) {
        paste0(table_name, " (", paste0(sort(dropped_vars), collapse = ", "), ")")
      })
      cli::cli_warn(
        "Some variables are not published at the block-group level and have been dropped from: {partial_messages}.")
    }

    resolved_tables = partition[["keep"]]

    ## auto (raw-code) tables: drop those not published at the block-group level,
    ## with a clear message (tidycensus itself returns an empty error here)
    bg_variables = bg_codebook$name[bg_codebook$geography == "block group"]
    if (length(auto_table_entries) > 0) {
      kept_auto = list()
      for (code in names(auto_table_entries)) {
        entry = auto_table_entries[[code]]
        if (any(entry[["raw_variables"]] %in% bg_variables)) {
          kept_auto[[code]] = entry
        } else {
          lowest_geography = bg_codebook$geography[bg_codebook$name == paste0(code, "_001")]
          lowest_geography = if (length(lowest_geography) == 0) "an unsupported geography" else lowest_geography[1]
          cli::cli_warn(
            "ACS table {.val {code}} is not published at the block-group level for {max(years)} (lowest available geography: {lowest_geography}); it has been dropped.")
        }
      }
      auto_table_entries = kept_auto
    }

    ## error out only if NONE of the requested tables are available at this
    ## geography. User-supplied DSL definitions compute from whatever columns are
    ## present (e.g., total_population), so they do not count as "unavailable".
    explicit_total_population = is.character(registry_tables) && "total_population" %in% registry_tables
    requested_survivors = c(
      setdiff(resolved_tables, "total_population"),
      names(auto_table_entries),
      if (explicit_total_population) "total_population" else character(0))
    if (has_explicit_tables && length(requested_survivors) == 0 && length(user_definitions) == 0) {
      cli::cli_abort(c(
        "None of the requested tables are published at the block-group level.",
        "i" = "Use {.code list_tables(geography = \"block group\")} to see available tables."))
    }
  }

  ## collect raw ACS variables from the registry
  suppressWarnings({suppressMessages({
    variables = collect_raw_variables(
      resolved_tables = resolved_tables, year = max(years),
      geography = geography,
      census_codebook = bg_codebook)
  })})

  ## append auto-table raw variables
  if (length(auto_table_entries) > 0) {
    auto_variables = purrr::map(auto_table_entries, ~ .x[["raw_variables"]]) %>%
      unname() %>% unlist()
    variables = c(variables, auto_variables)
  }

  ## resolve ACS codes in user definitions to clean column names
  if (length(user_definitions) > 0) {
    user_definitions = resolve_definition_variables(user_definitions, variables)
  }

  super_state_geographies = c(
    "us", "region", "division", "metropolitan/micropolitan statistical area",
    "metropolitan statistical area/micropolitan statistical area",
    "cbsa", "urban area", "zip code tabulation area", "zcta")

  ## warn when `counties` is supplied with a geography that doesn't honor it
  if (length(counties) > 0 && geography %in% super_state_geographies) {
    cli::cli_warn(c(
      "{.arg counties} is ignored when {.arg geography} is {.val {geography}}.",
      "i" = "The {.arg counties} filter only applies to county/tract/county-subdivision queries."))
  }

  ## default values for the states argument
  if (length(states) == 0) {
    states = tigris::fips_codes %>%
      dplyr::filter(!state %in% c("PR", "UM", "VI", "GU", "AS", "MP")) %>%
      dplyr::pull(state) %>% unique()
  }

  ## warning about inter-decadal tract geometry changes
  if ( (max(years) >= 2020) & (min(years) < 2020) & (geography %in% c("tract", "block group")) ) {
    cli::cli_warn(c(
      "Requested years span 2020, when the Census Bureau reconfigured tract and block-group boundaries.",
      "i" = "It is not valid to compare tract- or block group-level statistics across the 2020 boundary.",
      "i" = "Crosswalks are available from NHGIS, or via {.pkg crosswalk} ({.code renv::install('UI-Research/crosswalk')}).")) }

  ## warn user -- county-by-county queries are slow and should be used if only
  ## one or a few counties are desired
  if (length(counties) > 5) {

    cli::cli_warn(c(
      "County-level queries can be slow for more than a few counties.",
      "i" = "Omit the {.arg counties} parameter and filter after the function returns."))}

  ## warn that pulling tracts across many states is a large, slow query
  if (geography == "tract" && length(counties) == 0 && length(states) > 5) {
    cli::cli_warn(
      "Pulling tract-level data across {length(states)} state{?s} can be a slow query.")
  }

  ## resolve county_codes and the state vector used for downstream fetches
  if (geography %in% c("county", "county subdivision", "tract", "block group") & length(counties) > 0) {
    county_codes = tidycensus::fips_codes %>%
      dplyr::mutate(county_fips = paste0(state_code, county_code)) %>%
      dplyr::filter(county_fips %in% counties)

    if (nrow(county_codes) == 0) {
      cli::cli_abort("No valid county FIPS codes were found in {.arg counties}.") }

    if (nrow(county_codes) != length(counties)) {
      invalid_county_count = length(counties) - nrow(county_codes)
      cli::cli_warn("{invalid_county_count} invalid county code{?s} found; no results are returned for {?this county/these counties}.") }
  } else {
    county_codes = tidycensus::fips_codes %>%
      dplyr::filter(state %in% states | state_code %in% states | state_name %in% states)
  }

  ## states_for_fetch is the canonicalized 2-letter codes derived from county_codes.
  ## The user's `states` argument is preserved unchanged for downstream messaging.
  states_for_fetch = county_codes$state %>% unique()

  ## download corresponding geometries from tigris (conditionally)
  if (needs_tigris) {
    suppressMessages({ suppressWarnings({
      geometries = purrr::map(
        years,
        function(year) {
          switch(
            geography,
            "us" = tigris::nation(year = year) %>%
              dplyr::mutate(
                GEOID = "1",
                ALAND = 9161555541118, ## sum of ALAND from tigris::states(year = 2022, cb = TRUE)
                AWATER = 711492860209), ## sum of AWATER from tigris::states(year = 2022, cb = TRUE)
            "region" = tigris::regions(year = year),
            "division" = tigris::divisions(year = year),
            "state" = tigris::states(year = year, cb = TRUE),
            "county" = purrr::map(states_for_fetch, ~ tigris::counties(state = .x, cb = TRUE, year = year, progress_bar = FALSE)) %>% dplyr::bind_rows(),
            "county subdivision" = purrr::map(states_for_fetch, function(s) {
              county_vec = if (length(counties) > 0) {
                county_codes %>% dplyr::filter(state == s) %>% dplyr::pull(county_code)
              } else NULL
              tigris::county_subdivisions(state = s, county = county_vec, cb = TRUE, year = year, progress_bar = FALSE)
            }) %>% dplyr::bind_rows(),
            "tract" = purrr::map(states_for_fetch, function(s) {
              county_vec = if (length(counties) > 0) {
                county_codes %>% dplyr::filter(state == s) %>% dplyr::pull(county_code)
              } else NULL
              tigris::tracts(state = s, county = county_vec, cb = TRUE, year = year, progress_bar = FALSE)
            }) %>% dplyr::bind_rows(),
            "block group" = purrr::map(states_for_fetch, function(s) {
              county_vec = if (length(counties) > 0) {
                county_codes %>% dplyr::filter(state == s) %>% dplyr::pull(county_code)
              } else NULL
              tigris::block_groups(state = s, county = county_vec, cb = TRUE, year = year, progress_bar = FALSE)
            }) %>% dplyr::bind_rows(),
            "place" = purrr::map(states_for_fetch, ~ tigris::places(state = .x, cb = TRUE, year = year, progress_bar = FALSE)) %>% dplyr::bind_rows(),
            "alaska native regional corporation" = tigris::alaska_native_regional_corporations(cb = TRUE, year = year),
            "american indian area/alaska native area/hawaiian home land" = tigris::native_areas(cb = TRUE, year = year),
            "american indian area/alaska native area (reservation of statistical entity only)" = tigris::native_areas(cb = TRUE, year = year),
            "american indian area (off reservation trust land only)/hawaiian home land" = tigris::native_areas(cb = TRUE, year = year),
            "metropolitan/micropolitan statistical area" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "metropolitan statistical area/micropolitan statistical area" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "cbsa" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "combined statistical area" = tigris::combined_statistical_areas(cb = TRUE, year = year),
            "new england city and town area" = tigris::new_england(cb = TRUE, year = year, type = "NECTA"),
            "zcta" = ,
            "zip code tabulation area" = tigris::zctas(cb = TRUE, year = year, progress_bar = FALSE) %>%
              ## tigris returns year-suffixed columns for ZCTAs (e.g., GEOID20, ALAND20).
              ## Strip the suffix so the downstream transmute can use the unsuffixed names.
              dplyr::rename_with(~ stringr::str_remove(.x, "(10|20)$"),
                                 dplyr::matches("^(GEOID|ALAND|AWATER)(10|20)$")),
            cli::cli_abort("Unsupported geography: {.val {geography}}. See {.help compile_acs_data} for supported geographies.")) %>%
            dplyr::transmute(
              area_land_sq_kilometer = ALAND / 1000000,
              area_water_sq_kilometer = AWATER / 1000000,
              area_land_water_sq_kilometer = area_land_sq_kilometer + area_water_sq_kilometer,
              GEOID = GEOID,
              data_source_year = year) }) %>% dplyr::bind_rows()
    })})
  }

  suppressMessages({ suppressWarnings({
    df_raw_estimates = fetch_acs(
      geography = geography,
      variables = variables,
      years = years,
      states = states_for_fetch,
      counties = counties,
      county_codes = county_codes,
      super_state_geographies = super_state_geographies)
    moes = df_raw_estimates %>% dplyr::select(GEOID, data_source_year, dplyr::matches("_M$"))
  })})

  ####----Compute derived variables----####
  df_calculated_estimates = df_raw_estimates %>%
    dplyr::select(-dplyr::matches("_M$")) %>%
    dplyr::rename_with(~ stringr::str_remove(.x, "_E$"))

  ## apply each table's definitions via the execution engine
  df_calculated_estimates = purrr::reduce(resolved_tables, function(.data, table_name) {
    table_entry = get_table(table_name)
    if (!is.null(table_entry) && !is.null(table_entry[["definitions"]]) && length(table_entry[["definitions"]]) > 0) {
      execute_definitions(.data, table_entry[["definitions"]])
    } else {
      .data
    }
  }, .init = df_calculated_estimates)

  ## apply auto-table definitions
  if (length(auto_table_entries) > 0) {
    df_calculated_estimates = purrr::reduce(auto_table_entries, function(.data, auto_entry) {
      if (!is.null(auto_entry[["definitions"]]) && length(auto_entry[["definitions"]]) > 0) {
        execute_definitions(.data, auto_entry[["definitions"]])
      } else {
        .data
      }
    }, .init = df_calculated_estimates)
  }

  ## apply user-supplied definitions
  if (length(user_definitions) > 0) {
    validate_definition_variables(user_definitions, colnames(df_calculated_estimates))
    check_multi_table_variables(user_definitions, resolved_tables, auto_table_entries)
    df_calculated_estimates = execute_definitions(df_calculated_estimates, user_definitions)
  }

  ####----Generate codebook----####
  codebook = generate_codebook(.data = df_calculated_estimates,
                               resolved_tables = resolved_tables,
                               auto_table_entries = auto_table_entries,
                               user_definitions = user_definitions,
                               year = max(years))

  df_calculated_estimates = df_calculated_estimates %>%
    ## ensure the vintage of the data and the GEOID for each observation are the first columns
    dplyr::select(data_source_year, GEOID, dplyr::everything())

  if (needs_tigris) {
    ## filter geometries to only the GEOIDs present in the estimates. tigris
    ## returns all counties/tracts in the requested state(s), so when the user
    ## specified a subset via `counties`, an unfiltered right_join would inflate
    ## the result with NA-estimate rows for unrequested geographies.
    geometries = geometries %>%
      dplyr::filter(GEOID %in% df_calculated_estimates$GEOID)

    ## Use many-to-one rather than one-to-one: geographies whose definitions change
    ## across years (CBSAs, ZCTAs, pre/post-2020 tracts) may not preserve a strict
    ## 1:1 mapping between an estimate row and a geometry row over the requested
    ## span. many-to-one accepts those cases without dropping or erroring.
    df_calculated_estimates = df_calculated_estimates %>%
      dplyr::right_join(geometries, by = c("GEOID", "data_source_year"), relationship = "many-to-one") %>%
      {if (spatial == FALSE) sf::st_drop_geometry(.) else sf::st_as_sf(.) } %>%
      dplyr::mutate(population_density_land_sq_kilometer = safe_divide(total_population_universe, area_land_sq_kilometer))
  }

  df_calculated_estimates = df_calculated_estimates %>%
    dplyr::left_join(
      .,
      moes,
      by = c("GEOID", "data_source_year"))

  ####----Calculate MOEs for derived variables----####
  attr(df_calculated_estimates, "codebook") = codebook

  suppressMessages({suppressWarnings({
    df_moes = calculate_moes(df_calculated_estimates) %>%
      {if (!needs_tigris || spatial == FALSE) . else dplyr::right_join(., geometries %>% dplyr::select(GEOID, data_source_year), by = c("GEOID", "data_source_year"), relationship = "many-to-one")}
  })})

  ## attach the codebook and resolved tables as attributes to the returned dataset
  attr(df_moes, "codebook") = codebook %>%
    dplyr::select(calculated_variable, variable_type, definition, dplyr::everything())
  attr(df_moes, "resolved_tables") = resolved_tables

  if (isTRUE(spatial)) { df_moes = sf::st_as_sf(df_moes) }

  return(df_moes)
}

utils::globalVariables(c(
  "ALAND", "AWATER", "area_land_sq_kilometer", "area_water_sq_kilometer", "total_population_universe",
  "state", "GEOID", "data_source_year", ".",
  "state_code", "county_code", "county_fips", "state_name", "county"))
