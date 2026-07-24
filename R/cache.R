## Internal: directory where cached raw ACS query results are stored.
## Overridable via options(urbnindicators.cache_dir = ...) (used by tests).
acs_cache_dir = function() {
  getOption("urbnindicators.cache_dir", tools::R_user_dir("urbnindicators", which = "cache"))
}

## Internal: thin wrapper around tidycensus::get_acs(). Exists as a seam so
## tests can mock the network via testthat::local_mocked_bindings().
##
## When the Census API key is well-formed but fake or inactive, the API returns
## an HTML error page where JSON is expected, and tidycensus surfaces this as an
## opaque JSON parse error ("lexical error: invalid char in json text ...").
## We translate that specific failure into an actionable message about the key.
acs_query = function(args) {
  ## attribute any re-raised error to this frame, not the tryCatch handler
  query_call = rlang::current_env()
  tryCatch(
    do.call(tidycensus::get_acs, args),
    error = function(e) {
      message1 = conditionMessage(e)
      is_key_rejection = stringr::str_detect(
        message1,
        stringr::regex("lexical error|invalid char in json|invalid.?key|missing.?key",
                       ignore_case = TRUE))
      if (is_key_rejection) {
        abort_census_api_key_rejected(call = query_call)
      }
      cli::cli_abort("Census API request failed: {message1}", parent = e, call = query_call)
    })
}

## Internal: fetch one table's raw estimates for a single query, reading from /
## writing to the on-disk cache when `cache = TRUE`. `args` holds the
## tidycensus::get_acs() arguments for this chunk, including the chunk's named
## `variables` vector, so a change to a table's variable definitions produces
## a new cache key. `cache_stats` is an environment with `hits` and `total`
## counters, reported to the user once per compile_acs_data() call.
cached_get_acs = function(args, table_name, cache, cache_stats) {
  if (!cache) return(acs_query(args))

  cache_stats$total = cache_stats$total + 1
  key = rlang::hash(list(table_name = table_name, args = args))
  path = file.path(acs_cache_dir(), paste0("acs_", key, ".rds"))

  if (file.exists(path)) {
    ## a corrupt/unreadable file falls through to a refetch that overwrites it
    cached = tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(cached)) {
      cache_stats$hits = cache_stats$hits + 1
      return(cached)
    }
  }

  result = acs_query(args)
  dir.create(acs_cache_dir(), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, path)
  result
}

#' @title Clear the urbnindicators cache
#' @description Deletes all raw ACS query results cached by
#'    \code{compile_acs_data(cache = TRUE)}.
#' @details Cached files live in
#'    \code{tools::R_user_dir("urbnindicators", which = "cache")}, or in the
#'    directory named by \code{options(urbnindicators.cache_dir = ...)} when
#'    set. Because ACS estimates do not change, cached
#'    entries never go stale; clearing the cache is only useful to reclaim
#'    disk space.
#' @returns The number of cache files removed, invisibly.
#' @examples
#' \dontrun{
#' clear_acs_cache()
#' }
#' @export
clear_acs_cache = function() {
  cache_files = list.files(acs_cache_dir(), pattern = "^acs_.*\\.rds$", full.names = TRUE)
  removed = sum(file.remove(cache_files))
  cli::cli_inform("Removed {removed} cached ACS quer{?y/ies} from {.path {acs_cache_dir()}}.")
  invisible(removed)
}
