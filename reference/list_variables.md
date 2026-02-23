# List all variables and their tables

Returns a tibble mapping all variables (raw ACS variables and computed
indicators) to their construct-level table name. This provides a
comprehensive view of every variable that
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
produces.

## Usage

``` r
list_variables(year = 2022)
```

## Arguments

- year:

  The ACS year used to resolve variable names (default 2022).

## Value

A tibble with columns `variable` and `table`.

## Examples

``` r
if (FALSE) { # \dontrun{
list_variables()
list_variables() %>% dplyr::filter(table == "age")
} # }
```
