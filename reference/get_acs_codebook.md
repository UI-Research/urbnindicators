# Browse the ACS codebook with clean variable names

Returns a tibble of ACS variables for the given year, with the parent
table code, raw variable code, and a cleaned snake_case name. Useful for
finding the table code to pass to `compile_acs_data(tables = ...)`.

## Usage

``` r
get_acs_codebook(year = 2024, table = NULL)
```

## Arguments

- year:

  A four-digit year for the five-year ACS estimates (default 2022).

- table:

  An optional ACS table code (e.g., `"B22003"`) to filter results to a
  single table.

## Value

A tibble with columns `table` (parent ACS table code), `variable_raw`
(ACS variable code), and `variable_clean` (snake_case name produced by
the package).

## Examples

``` r
if (FALSE) { # \dontrun{
## Browse all variables
get_acs_codebook()

## Filter to a specific table
get_acs_codebook(table = "B22003")

## Search for variables by keyword
get_acs_codebook() %>% dplyr::filter(stringr::str_detect(variable_clean, "snap"))
} # }
```
