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
  cache_key = paste(dataset, year, sep = "_")
  if (!is.null(.variables_cache[[cache_key]])) {
    return(.variables_cache[[cache_key]])
  }

  api_key = Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(api_key)) {
    stop(
      "A Census Bureau API key is required to fetch ACS variables metadata. ",
      "Set it with `tidycensus::census_api_key(\"YOUR_KEY\", install = TRUE)` ",
      "or `Sys.setenv(CENSUS_API_KEY = \"YOUR_KEY\")` before calling this ",
      "function. Request a free key at https://api.census.gov/data/key_signup.html.",
      call. = FALSE)
  }

  ## map "acs5" -> "acs/acs5" path segment used by the Census API
  dataset_path = if (dataset == "acs5") "acs/acs5" else dataset
  url = paste0("https://api.census.gov/data/", year, "/", dataset_path, "/variables.json")

  response = httr::GET(url, query = list(key = api_key))
  if (httr::status_code(response) == 404L) {
    stop("Census API endpoint not found. Does this dataset exist for the ",
         "specified year? See https://api.census.gov/data.html.",
         call. = FALSE)
  }
  if (httr::http_status(response)$category != "Success") {
    stop("Census API request failed: ", httr::http_status(response)$message,
         call. = FALSE)
  }

  ## mirror tidycensus::load_variables()'s post-processing so output is a
  ## drop-in replacement
  variables_df = httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    purrr::modify_depth(2, function(x) { x$validValues = NULL; x }) %>%
    purrr::flatten_df(.id = "name") %>%
    dplyr::arrange(name)

  variables_filtered = variables_df[, 1:3]
  names(variables_filtered) = tolower(names(variables_filtered))
  variables_filtered = variables_filtered %>%
    dplyr::filter(stringr::str_detect(
      name,
      "^B[0-9]|^C[0-9]|^DP[0-9]|^S[0-9]|^P.*[0-9]|^H.*[0-9]|^K[0-9]|^CP[0-9]|^T[0-9]")) %>%
    dplyr::mutate(name = stringr::str_replace(name, "E$|M$", "")) %>%
    dplyr::filter(!stringr::str_detect(label, "Margin Of Error|Margin of Error"))

  if (dataset == "acs5" && year > 2010) {
    geography_lookup = tidycensus::acs5_geography %>%
      dplyr::filter(year == !!year)
    variables_filtered = variables_filtered %>%
      dplyr::mutate(table = stringr::str_remove(name, "_.*")) %>%
      dplyr::left_join(geography_lookup, by = "table") %>%
      dplyr::select(-year, -table)
  }

  result = tibble::as_tibble(variables_filtered)
  .variables_cache[[cache_key]] = result
  result
}
