## Internal: the shared "how to get and set a key" guidance, reused by every
## Census API key error message so the user always sees the same actionable
## next steps regardless of which check failed.
census_api_key_help = function() {
  c(
    "i" = "Request a free key at {.url https://api.census.gov/data/key_signup.html}.",
    "i" = "Set it with {.code tidycensus::census_api_key(\"YOUR_KEY\", install = TRUE)}",
    "i" = "or {.code Sys.setenv(CENSUS_API_KEY = \"YOUR_KEY\")} in your current session.")
}

## Internal: check that a plausibly-valid Census Bureau API key is available
## before any network request is made. This catches the two problems we can
## detect locally, without a round-trip to the Census API:
##   * no key at all (the CENSUS_API_KEY environment variable is unset or empty)
##   * a key too short or malformed to possibly be valid -- a real key is a long
##     hexadecimal-style string, so anything under 10 characters, or missing
##     either letters or numbers, cannot be a genuine key.
## A key that passes these checks may still be rejected by the API at request
## time (a well-formed but fake or deactivated key); that case is handled
## separately by abort_census_api_key_rejected(), because it can only be
## detected from the API's response.
## Returns the key invisibly on success.
validate_census_api_key = function(call = rlang::caller_env()) {
  api_key = Sys.getenv("CENSUS_API_KEY")

  if (!nzchar(api_key)) {
    cli::cli_abort(
      c("A Census Bureau API key is required to query the ACS.",
        "x" = "No key was found in the {.envvar CENSUS_API_KEY} environment variable.",
        census_api_key_help()),
      call = call)
  }

  key_length = nchar(api_key)
  has_letter = stringr::str_detect(api_key, "[A-Za-z]")
  has_number = stringr::str_detect(api_key, "[0-9]")
  if (key_length < 10 || !has_letter || !has_number) {
    missing = c(if (!has_letter) "letters", if (!has_number) "numbers")
    missing_note = if (length(missing) > 0) {
      paste0(" and is missing ", stringr::str_flatten(missing, collapse = " and "))
    } else {
      ""
    }
    cli::cli_abort(
      c("The value of {.envvar CENSUS_API_KEY} does not look like a valid Census Bureau API key.",
        "x" = "A valid key is at least 10 characters long and contains both letters and numbers.",
        "i" = "The current value is {key_length} character{?s} long{missing_note}.",
        census_api_key_help()),
      call = call)
  }

  invisible(api_key)
}

## Internal: decide whether an httr response is the Census API's "invalid key"
## or "missing key" error rather than real data. An invalid or inactive key is
## not returned with an HTTP error status: the API issues a 302 redirect to an
## HTML error page (invalid_key.html / missing_key.html) that resolves to
## HTTP 200, so the body is HTML rather than JSON. The two reliable signals are
## the final (post-redirect) URL and an HTML content type where JSON is
## expected.
census_key_error_from_response = function(response) {
  final_url = response$url
  if (is.null(final_url)) final_url = ""
  content_type = response$headers[["content-type"]]
  if (is.null(content_type)) content_type = ""

  stringr::str_detect(final_url, "invalid_key|missing_key") ||
    stringr::str_detect(stringr::str_to_lower(content_type), "text/html")
}

## Internal: probe whether the Census Bureau Data API is currently reachable,
## using the same keyless endpoint the Census status page
## (https://api.census.gov/status/) checks. Returns TRUE only when the API
## responds successfully with JSON; any network error, timeout, non-success
## status, or non-JSON (HTML error page) response counts as unreachable. This is
## what lets us tell a rejected key apart from an API outage, since both
## otherwise surface the same way (an HTML error page where JSON is expected).
census_api_is_reachable = function(timeout_seconds = 10) {
  probe_url = "https://api.census.gov/data/2019/acs/acs1/profile/tags.json"
  tryCatch({
    response = httr::GET(probe_url, httr::timeout(timeout_seconds))
    content_type = response$headers[["content-type"]]
    if (is.null(content_type)) content_type = ""
    is_success = httr::http_status(response)$category == "Success"
    is_json = stringr::str_detect(stringr::str_to_lower(content_type), "json")
    if (!is_success || !is_json) {
      return(FALSE)
    }
    ## a genuine JSON body confirms the API is serving data, not an error page
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    TRUE
  }, error = function(e) FALSE)
}

## Internal: abort with an actionable message when a Census Bureau API request
## returned an HTML error page where data (JSON) was expected. The key is
## well-formed (so it passed validate_census_api_key()), which leaves two
## possible causes: the key is fake/inactive, or the API is down. We probe the
## API's keyless status endpoint to tell them apart, so the message is
## definitive rather than a guess. Without this, the failure surfaces as an
## opaque JSON parse error ("lexical error: invalid char in json text ...").
abort_census_api_key_rejected = function(call = rlang::caller_env()) {
  if (census_api_is_reachable()) {
    ## the API is serving data, so the request-time rejection can only be the key
    cli::cli_abort(
      c("The Census Bureau API rejected the API key in {.envvar CENSUS_API_KEY}.",
        "x" = "The key is well-formed but is not a valid, active Census Bureau API key.",
        "v" = "The Census Bureau API is reachable right now, so this is a problem with the key, not an outage.",
        "i" = "Check the key for typos or stray characters, and confirm it has been activated (new keys are activated via the link in the sign-up email).",
        "i" = "If the key was never activated or has been revoked, request a new one.",
        census_api_key_help()),
      call = call)
  }

  ## the status probe also failed, so the API itself is unavailable; a bad key
  ## would look the same, so still point at it as a secondary cause
  cli::cli_abort(
    c("The Census Bureau API returned an error page instead of data, and a status check of the API also failed.",
      "x" = "The Census Bureau API appears to be experiencing a temporary outage.",
      "i" = "Check the current status at {.url https://api.census.gov/status/}, then try again later.",
      "i" = "If the status page shows the API is available, confirm the key in {.envvar CENSUS_API_KEY} is valid and active.",
      census_api_key_help()),
    call = call)
}
