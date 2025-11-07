# Calculate coefficients of variation

Calculate coefficients of variation

## Usage

``` r
calculate_cvs(.df)
```

## Arguments

- .df:

  The dataset returned from
  [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
  The argument to this parameter must have an attribute named `codebook`
  (as is true of results from `compile_acs_data())`.

## Value

A modified dataframe that includes newly calculated indicators.

## Details

Create CVs for all ACS estimates and derived indicators

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
cvs = calculate_cvs(df) %>%
 dplyr::select(matches("_CV$"))
} # }
```
