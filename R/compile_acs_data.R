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
#' @param tables A character vector of table names to include (e.g.,
#'    \code{c("race", "snap")}). Use \code{list_tables()} to see available tables.
#'    When NULL (default) and \code{indicators} is also NULL, all tables are included.
#' @param indicators A character vector of indicator names to include (e.g.,
#'    \code{c("snap_received_percent")}). Each indicator's parent table is
#'    automatically included.
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
#' ## Pull by indicator name (returns the full parent table)
#' df = compile_acs_data(indicators = c("snap_received_percent"),
#'                       years = 2022, geography = "county", states = "NJ")
#'   }
#' @export
#' @importFrom magrittr %>%

compile_acs_data = function(
    tables = NULL,
    indicators = NULL,
    years = c(2022),
    geography = "county",
    states = NULL,
    counties = NULL,
    spatial = FALSE,
    ...) {

  ## handle deprecated `variables` parameter and unknown arguments
  dots = list(...)
  if ("variables" %in% names(dots)) {
    lifecycle::deprecate_warn(
      when = "0.1.0",
      what = "compile_acs_data(variables)",
      details = "The `variables` parameter is ignored. Use `tables` or `indicators` to select specific data, or call with no selection arguments for all tables."
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

  ####----Resolve tables and variables via the registry----####
  ## resolve which tables to include
  if (is.null(tables) && is.null(indicators)) {
    ## default: all internal table names
    resolved_tables = names(.table_registry$tables)
  } else {
    resolved_tables = resolve_tables(tables = tables, indicators = indicators)
  }

  ## determine whether tigris geometry is needed
  needs_tigris = isTRUE(spatial) || ("population_density" %in% resolved_tables)

  ## collect raw ACS variables from the registry
  suppressWarnings({suppressMessages({
    variables = collect_raw_variables(resolved_tables = resolved_tables, year = years[1])
  })})

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

  ####----Generate codebook----####
  ## generate codebook BEFORE the _pct rename so that regex matching in
  ## expand_codebook_entry() works on the original _percent column names;
  ## the codebook's internal rename (percent -> pct for Count variables)
  ## produces the correct output names
  codebook = generate_codebook(.data = df_calculated_estimates, resolved_tables = resolved_tables)

  df_calculated_estimates = df_calculated_estimates %>%
    ## these variable names end in "percent", but they're actually count estimates
    dplyr::rename_with(.cols = dplyr::matches("household_income.*percent$"), .fn = ~ stringr::str_replace(., "percent$", "pct")) %>%
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
      moes %>%
        dplyr::rename_with(.cols = dplyr::matches("household_income.*percent_M$"), .fn = ~ stringr::str_replace(., "percent_M$", "pct_M")),
      by = c("GEOID", "data_source_year"))

  ####----Calculate CVs----####
  attr(df_calculated_estimates, "codebook") = codebook

  suppressMessages({suppressWarnings({
    df_cvs = calculate_cvs(df_calculated_estimates) %>%
      {if (!needs_tigris || spatial == FALSE) . else dplyr::right_join(., geometries %>% dplyr::select(GEOID, data_source_year), by = c("GEOID", "data_source_year"), relationship = "one-to-one")}
  })})

  ## attach the codebook and resolved tables as attributes to the returned dataset
  attr(df_cvs, "codebook") = codebook %>%
    dplyr::select(calculated_variable, variable_type, definition, dplyr::everything())
  attr(df_cvs, "resolved_tables") = resolved_tables

  if (isTRUE(spatial)) { df_cvs = sf::st_as_sf(df_cvs) }

  return(df_cvs)
}

utils::globalVariables(c(
  "ALAND", "AWATER", "area_land_sq_kilometer", "area_water_sq_kilometer", "total_population_universe",
  "state", "GEOID", "data_source_year", ".",
  "state_code", "county_code", "county_fips", "state_name", "county",
  "needs_tigris", "resolved_tables"))
