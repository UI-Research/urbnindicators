#' @title Division without NAs
#' @description Return 0 when the divisor is 0.
#' @details A modified division operation that returns zero when the divisor is zero
#'    rather than returning NA. Otherwise returns the quotient.
#' @param x A numeric scalar.
#' @param y A numeric scalar.
#' @returns The traditional dividend in all cases except where \code{y == 0}, in which
#'    case it returns 0.
#' @examples
#' safe_divide(1, 2)
#' safe_divide(3, 0)
#' @export
safe_divide = function(x, y) { dplyr::if_else(y == 0, 0, x / y) }

#' @title Analysis-ready social science measures
#' @description Construct measures frequently used in social sciences
#'    research, leveraging \code{tidycensus::get_acs()} to acquire raw estimates from
#'    the Census Bureau API.
#' @param tables A character vector of table names to include. Two formats are
#'    accepted:
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
#'    }
#'    Both formats can be mixed freely (e.g., \code{c("snap", "B25070")}).
#'    If an ACS code corresponds to an already-registered table, the registered
#'    version is used automatically.
#'    When NULL (default), all registered tables are included (unregistered ACS
#'    tables must be requested explicitly).
#' @param years A numeric vector of four-digit years for which to pull five-year
#'    American Community Survey estimates.
#' @param geography A geography type that is accepted by \code{tidycensus::get_acs()}, e.g.,
#'    "tract", "county", "state", among others. Geographies below the tract level are not
#'    supported.
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
#'    Returned data are formatted wide. A codebook generated with \code{generate_codebook()}
#'    is attached and can be accessed via \code{compile_acs_data() %>% attr("codebook")}.
#' @examples
#' \dontrun{
#' ## Pull all tables (default, backward-compatible)
#' df = compile_acs_data(years = c(2022), geography = "county", states = "NJ")
#'
#' ## Pull specific tables
#' df = compile_acs_data(tables = c("race", "snap"), years = 2022,
#'                       geography = "county", states = "NJ")
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
#'   }
#' @export
#' @importFrom magrittr %>%

compile_acs_data = function(
    tables = NULL,
    years = c(2024),
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
    rlang::warn(
      paste0(
        "Unknown argument(s) passed to `compile_acs_data()`: ",
        paste0("`", unknown_args, "`", collapse = ", "),
        ". These will be ignored."
      )
    )
  }

  old_tigris_cache = getOption("tigris_use_cache")
  options(tigris_use_cache = FALSE)
  on.exit(options(tigris_use_cache = old_tigris_cache), add = TRUE)

  ## validate years
  years = as.numeric(years)
  if (any(is.na(years)) || any(years != as.integer(years)) || any(nchar(as.integer(years)) != 4)) {
    stop("`years` must be a vector of four-digit integers (e.g., 2022).")
  }
  if (any(years < 2009) || any(years > as.numeric(format(Sys.Date(), "%Y")))) {
    stop("`years` must be between 2009 (earliest 5-year ACS) and the current year.")
  }

  ## validate denominator parameter
  valid_denominator = denominator %in% c("parent", "total") ||
    grepl("^[BC][0-9]{5}[A-I]?(_[0-9]{3})?$", denominator, perl = TRUE)
  if (!valid_denominator) {
    stop(paste0("`denominator` must be \"parent\", \"total\", or a valid ACS variable code (e.g., \"B25070_001\"). Got: \"", denominator, "\"."))
  }

  ####----Partition tables into registry vs auto (raw ACS codes)----####
  auto_table_entries = list()
  registry_tables = tables
  raw_acs_codes = character(0)

  if (!is.null(tables)) {
    construct_map = build_construct_map()
    internal_names = names(.table_registry$tables)

    ## load census variables once for resolve_to_acs_table lookups
    suppressMessages({suppressWarnings({
      census_variables_for_resolve = tidycensus::load_variables(year = years[1], dataset = "acs5")
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
      resolved_code = resolve_to_acs_table(tbl, year = years[1],
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
  if (is.null(tables)) {
    ## default: all internal table names
    resolved_tables = names(.table_registry$tables)
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
          year = years[1],
          denominator_mode = denominator_mode,
          custom_denominator = custom_denominator,
          census_variables = census_variables_for_resolve)
      })
    })})
    names(auto_table_entries) = raw_acs_codes
  }

  ## collect raw ACS variables from the registry
  suppressWarnings({suppressMessages({
    variables = collect_raw_variables(resolved_tables = resolved_tables, year = years[1])
  })})

  ## append auto-table raw variables
  if (length(auto_table_entries) > 0) {
    auto_variables = purrr::map(auto_table_entries, ~ .x[["raw_variables"]]) %>%
      unname() %>% unlist()
    variables = c(variables, auto_variables)
  }

  ## default values for the states argument
  if (length(states) == 0) {
    states = tigris::fips_codes %>%
      dplyr::filter(!state %in% c("PR", "UM", "VI", "GU", "AS", "MP")) %>%
      dplyr::pull(state) %>% unique()
  }

  ## warning about inter-decadal tract geometry changes
  if ( (max(years) >= 2020) & (min(years) < 2020) & (geography == "tract") ) {
    message("Requested years span the year 2020, which is when the Census Bureau re-configures
      census tract boundaries. It is not valid to compare census tract-level statistics for years before 2020 to
      statistics from 2020 and after.
      
      Crosswalks are available from NHGIS, or via `library(crosswalk)`, which is currently
      under development and can be installed via renv::install('UI-Research/crosswalk').)") }

  ## tracts and larger are supported
  if ((geography %>% tolower) %in% c("block", "block group")) {
    stop("Block and block group geographies are not supported at this time.") }

  ## warn user -- county-by-county queries are slow and should be used if only
  ## one or a few counties are desired
  if (!any(is.null(counties)) && length(counties) > 5) {

warning(
"County-level queries can be slow for more than a few counties. Omit the county parameter
if you are interested in more than five counties; filter to your desired counties after
this function returns.")}

  super_state_geographies = c(
    "us", "region", "division", "metropolitan/micropolitan statistical area",
    "metropolitan statistical area/micropolitan statistical area",
    "cbsa", "urban area", "zip code tabulation area", "zcta")

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
            "county" = purrr::map(states, ~ tigris::counties(state = .x, cb = TRUE, year = year, progress_bar = FALSE)) %>% dplyr::bind_rows(),
            "county subdivision" = purrr::map(states, ~ tigris::county_subdivisions(state = .x, cb = TRUE, year = year, progress_bar = FALSE)) %>% dplyr::bind_rows(),
            "tract" = purrr::map(states, ~ tigris::tracts(state = .x, cb = TRUE, year = year, progress_bar = FALSE)) %>% dplyr::bind_rows(),
            "place" = purrr::map(states, ~ tigris::places(state = .x, cb = TRUE, year = year, progress_bar = FALSE)) %>% dplyr::bind_rows(),
            "alaska native regional corporation" = tigris::alaska_native_regional_corporations(cb = TRUE, year = year),
            "american indian area/alaska native area/hawaiian home land" = tigris::native_areas(cb = TRUE, year = year),
            "american indian area/alaska native area (reservation of statistical entity only)" = tigris::native_areas(cb = TRUE, year = year),
            "american indian area (off reservation trust land only)/hawaiian home land" = tigris::native_areas(cb = TRUE, year = year),
            "metropolitan/micropolitan statistical area" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "metropolitan statistical area/micropolitan statistical area" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "cbsa" = tigris::core_based_statistical_areas(cb = TRUE, year = year),
            "combined statistical area" = tigris::combined_statistical_areas(cb = TRUE, year = year),
            "new england city and town area" = tigris::new_england(cb = TRUE, year = year, type = "NECTA")) %>%
            dplyr::transmute(
              area_land_sq_kilometer = ALAND / 1000000,
              area_water_sq_kilometer = AWATER / 1000000,
              area_land_water_sq_kilometer = area_land_sq_kilometer + area_water_sq_kilometer,
              GEOID = GEOID,
              data_source_year = year) }) %>% dplyr::bind_rows()
    })})
  }

  ## configuring to subset call to specified counties, if applicable
  if (geography %in% c("county", "county subdivision", "tract") & !is.null(counties)) {
    county_codes = tidycensus::fips_codes %>%
      dplyr::mutate(county_fips = paste0(state_code, county_code)) %>%
      dplyr::filter(county_fips %in% counties)

    if (nrow(county_codes) == 0) {
      stop("No valid county FIPS codes were found in the `counties` argument.") }

    if (nrow(county_codes) != length(counties)) {
      invalid_county_count = length(counties) - nrow(county_codes)
      warning(paste0("There were ", invalid_county_count, " invalid county codes; no results are returned for these counties.")) }
  } else {
    county_codes = tidycensus::fips_codes %>%
      dplyr::filter(state %in% states | state_code %in% states | state_name %in% states)
  }

  states = county_codes$state %>% unique

  suppressMessages({ suppressWarnings({

    ## some geographies are not available by state and can only be returned nationally
    if (geography %in% super_state_geographies) {
      df_raw_estimates = purrr::map(
        ## when year is a vector with length > 1 (i.e., there are multiple years)
        ## loop over each item in the vector (and this approach also works for a single year)
        years,
        ~ tidycensus::get_acs(
            geography = geography,
            variables = variables,
            year = as.numeric(.x),
            survey = "acs5",
            output = "wide") %>%
          dplyr::mutate(data_source_year = .x)) %>% purrr::list_rbind()

    } else if (is.null(counties)) {
      ## for those geographies that can (or must) be returned by state, but where
      ## we do not need to query individual counties:
      ## iteratively make calls for data from each state
      ## and then combine the resulting dataframes together into a single dataframe
      df_raw_estimates = purrr::map(
        states,
        function (state) {
          purrr::map(
            ## when year is a vector with length > 1 (i.e., there are multiple years)
            ## loop over each item in the vector (and this approach also works for a single year)
            years,
            ~ tidycensus::get_acs(
              geography = geography,
              variables = variables,
              year = as.numeric(.x),
              state = state,
              ## this argument is ignored when a query cannot be made at the county level
              survey = "acs5",
              output = "wide") %>%
              dplyr::mutate(data_source_year = .x)) %>% purrr::list_rbind()}) %>% purrr::list_rbind()} else {

      ## for queries that must be returned by county within state
      ## iteratively make calls for data from each state, by county,
      ## and then combine the resulting dataframes together into a single dataframe
      df_raw_estimates = purrr::map(
        states,
        function(state) {
          purrr::map(
            ## when year is a vector with length > 1 (i.e., there are multiple years)
            ## loop over each item in the vector (and this approach also works for a single year)
            years,
            function(year) {
              if (geography %in% c("tract", "county")) {
                result = purrr::map(
                  county_codes %>% dplyr::filter(state == !!state) %>% dplyr::pull(county),
                  ~ tidycensus::get_acs(
                    geography = geography,
                    variables = variables,
                    year = as.numeric(year),
                    state = state,
                    county = .x,
                    survey = "acs5",
                    output = "wide")) %>% purrr::list_rbind() %>%
                  dplyr::mutate(data_source_year = year) } else {

                result = tidycensus::get_acs(
                  geography = geography,
                  variables = variables,
                  year = as.numeric(year),
                  state = state,
                  survey = "acs5",
                  output = "wide") %>%
                  dplyr::mutate(data_source_year = year)}}) %>% purrr::list_rbind()}) %>% purrr::list_rbind()
      }
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

  ####----Generate codebook----####
  codebook = generate_codebook(.data = df_calculated_estimates,
                               resolved_tables = resolved_tables,
                               auto_table_entries = auto_table_entries)

  df_calculated_estimates = df_calculated_estimates %>%
    ## ensure the vintage of the data and the GEOID for each observation are the first columns
    dplyr::select(data_source_year, GEOID, dplyr::everything())

  if (needs_tigris) {
    df_calculated_estimates = df_calculated_estimates %>%
      dplyr::right_join(geometries, by = c("GEOID", "data_source_year"), relationship = "one-to-one") %>%
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
      {if (!needs_tigris || spatial == FALSE) . else dplyr::right_join(., geometries %>% dplyr::select(GEOID, data_source_year), by = c("GEOID", "data_source_year"), relationship = "one-to-one")}
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
  "state_code", "county_code", "county_fips", "state_name", "county",
  "needs_tigris", "resolved_tables", "auto_table_entries"))
