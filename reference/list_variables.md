# List all variables and their tables

Returns a tibble mapping all variables (raw ACS variables and computed
indicators) to their construct-level table name. This provides a
comprehensive view of every variable that
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
produces for registered tables. Variables from unregistered ACS tables
passed as raw codes (e.g., `"B25070"`) are not included here; they are
auto-generated at runtime.

## Usage

``` r
list_variables(year = latest_acs_year())
```

## Arguments

- year:

  The ACS year used to resolve variable names. Defaults to the most
  recent ACS five-year vintage expected to be available.

## Value

A tibble with columns `variable` and `table`.

## Examples

``` r
if (FALSE) { # \dontrun{
list_variables()
list_variables() %>% dplyr::filter(table == "age")
} # }
```
