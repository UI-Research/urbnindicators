# Return ACS variables codes and names

**\[deprecated\]**

Use
[`list_variables()`](https://ui-research.github.io/urbnindicators/reference/list_variables.md)
instead to see available variables, or pass `tables` to
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

## Usage

``` r
list_acs_variables(year = "2022", tables = NULL)
```

## Arguments

- year:

  The year for which variable names should be selected.

- tables:

  An optional character vector of table names from the table registry
  (e.g., `c("race", "snap")`). When provided, only variables for the
  specified tables are returned. Use
  [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)
  to see available table names.

## Value

`NULL`, invisibly. Previously returned a named vector of variable codes.

## Examples

``` r
if (FALSE) { # \dontrun{
list_acs_variables()
} # }
```
