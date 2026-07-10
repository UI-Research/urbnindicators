# Clear the urbnindicators cache

Deletes all raw ACS query results cached by
`compile_acs_data(cache = TRUE)`.

## Usage

``` r
clear_acs_cache()
```

## Value

The number of cache files removed, invisibly.

## Details

Cached files live in
`tools::R_user_dir("urbnindicators", which = "cache")`, or in the
directory named by `options(urbnindicators.cache_dir = ...)` when set.
Because ACS estimates do not change, cached entries never go stale;
clearing the cache is only useful to reclaim disk space.

## Examples

``` r
if (FALSE) { # \dontrun{
clear_acs_cache()
} # }
```
