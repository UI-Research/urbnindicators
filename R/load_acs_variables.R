#' @importFrom magrittr %>%

## Package-level in-memory cache of fetched variables tibbles, keyed by
## "<dataset>_<year>". Populated lazily; cleared when the R session ends.
.variables_cache = new.env(parent = emptyenv())

## Internal: keyed, cached fetch of the Census ACS variables metadata tibble.
##
## Workaround for the Census Bureau API now requiring an API key on the
## variables.json endpoint, which `tidycensus::load_variables()` does not
## currently send. Returns a tibble shaped identically to the result of
## `tidycensus::load_variables(year, "acs5")`. Cached in-memory for the
## duration of the R session, keyed by (dataset, year).
load_acs_variables = function(year, dataset = "acs5") {
  if (!identical(dataset, "acs5")) {
    cli::cli_abort(c(
      "Only {.val acs5} is supported for {.arg dataset}.",
      "i" = "Got {.val {dataset}}."))
  }

  cache_key = paste(dataset, year, sep = "_")
  if (!is.null(.variables_cache[[cache_key]])) {
    return(.variables_cache[[cache_key]])
  }

  api_key = Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(api_key)) {
    cli::cli_abort(c(
      "A Census Bureau API key is required to fetch ACS variables metadata.",
      "i" = "Set it with {.code tidycensus::census_api_key(\"YOUR_KEY\", install = TRUE)}",
      "i" = "or {.code Sys.setenv(CENSUS_API_KEY = \"YOUR_KEY\")} before calling this function.",
      "i" = "Request a free key at {.url https://api.census.gov/data/key_signup.html}."))
  }

  url = paste0("https://api.census.gov/data/", year, "/acs/acs5/variables.json")

  response = httr::GET(url, query = list(key = api_key))
  if (httr::status_code(response) == 404L) {
    cli::cli_abort(c(
      "Census API endpoint not found.",
      "i" = "Does the dataset exist for the specified year? See {.url https://api.census.gov/data.html}."))
  }
  if (httr::http_status(response)$category != "Success") {
    cli::cli_abort("Census API request failed: {httr::http_status(response)$message}")
  }

  ## Extract name/label/concept explicitly. Entries in `variables` are
  ## heterogeneous: data variables carry fields like `label`, `concept`,
  ## `predicateType`, `group`, `attributes`, and `predicateOnly`, while
  ## predicate-only entries (`for`, `in`, `SUMLEVEL`, `GEOCOMP`, `STATE`, etc.)
  ## omit several of these. Coercing the nested list with `tibble::as_tibble`
  ## therefore fails with a `recycle_columns()` size mismatch.
  parsed = httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  variables_filtered = purrr::imap(parsed$variables, function(meta, name) {
    tibble::tibble(
      name = name,
      label = if (is.null(meta$label)) NA_character_ else meta$label,
      concept = if (is.null(meta$concept)) NA_character_ else meta$concept)
  }) %>%
    purrr::list_rbind() %>%
    dplyr::arrange(name) %>%
    dplyr::filter(stringr::str_detect(
      name,
      "^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P.*[0-9]|^H.*[0-9]|^K[0-9]|^CP[0-9]|^T[0-9]")) %>%
    ## Only strip the trailing E/M when it follows the standard "_NNN" suffix
    ## shape, so table codes that legitimately end in E or M aren't corrupted.
    dplyr::mutate(name = stringr::str_replace(name, "(_[0-9]{3})(E|M)$", "\\1")) %>%
    dplyr::filter(!stringr::str_detect(label, "Margin Of Error|Margin of Error"))

  if (year > 2010) {
    ## tidycensus::acs5_geography maps table -> lowest published geography by
    ## year, but it trails the newest ACS releases (joining an uncovered year
    ## yields an all-NA geography column, which reads as "nothing is published
    ## at the block-group level"). Fall back to the nearest covered vintage.
    lookup_years = unique(tidycensus::acs5_geography$year)
    lookup_year = if (year %in% lookup_years) {
      year
    } else if (any(lookup_years < year)) {
      max(lookup_years[lookup_years < year])
    } else {
      min(lookup_years)
    }
    geography_lookup = tidycensus::acs5_geography %>%
      dplyr::filter(year == !!lookup_year)
    variables_filtered = variables_filtered %>%
      dplyr::mutate(table = stringr::str_remove(name, "_.*")) %>%
      dplyr::left_join(geography_lookup, by = "table") %>%
      dplyr::select(-year, -table)
  }

  result = tibble::as_tibble(variables_filtered)
  .variables_cache[[cache_key]] = result
  result
}

## Internal: keyed, cached fetch of the Census ACS groups metadata, mapping
## each ACS table code to its published universe statement (e.g.,
## "Households", "Population 25 years and over"). The groups.json endpoint
## does not require an API key. Universe statements are published for
## vintages 2020 and later; earlier vintages yield NA universes.
load_acs_groups = function(year, dataset = "acs5") {
  if (!identical(dataset, "acs5")) {
    cli::cli_abort(c(
      "Only {.val acs5} is supported for {.arg dataset}.",
      "i" = "Got {.val {dataset}}."))
  }

  cache_key = paste("groups", dataset, year, sep = "_")
  if (!is.null(.variables_cache[[cache_key]])) {
    return(.variables_cache[[cache_key]])
  }

  url = paste0("https://api.census.gov/data/", year, "/acs/acs5/groups.json")

  response = httr::GET(url)
  if (httr::status_code(response) == 404L) {
    cli::cli_abort(c(
      "Census API endpoint not found.",
      "i" = "Does the dataset exist for the specified year? See {.url https://api.census.gov/data.html}."))
  }
  if (httr::http_status(response)$category != "Success") {
    cli::cli_abort("Census API request failed: {httr::http_status(response)$message}")
  }

  parsed = httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  result = purrr::map(parsed$groups, function(group_meta) {
    ## the API returns the universe field name with a trailing space
    ## ("universe "); accept either spelling
    universe = group_meta[["universe "]]
    if (is.null(universe)) universe = group_meta[["universe"]]
    tibble::tibble(
      acs_table = group_meta[["name"]],
      universe = if (is.null(universe)) NA_character_ else universe)
  }) %>%
    purrr::list_rbind() %>%
    dplyr::distinct(acs_table, .keep_all = TRUE)

  .variables_cache[[cache_key]] = result
  result
}
