# Calculate ACS measures

Calculates derived ACS indicators.

## Usage

``` r
internal_compute_acs_variables(.data)
```

## Arguments

- .data:

  The dataset returned from
  [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

## Value

A modified dataframe that includes newly calculated indicators.

## Details

An internal function used to calculate indicators and derived variable
definitions for the codebook.

## Examples

``` r
if (FALSE) { # \dontrun{
df = compile_acs_data(
  variables = list_acs_variables(year = 2022),
  years = c(2022),
  geography = "county",
  states = "NJ",
  counties = NULL,
  spatial = FALSE)
internal_compute_acs_variables(.data = df)
} # }
```
