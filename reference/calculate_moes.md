# Calculate margins of error for derived variables

Calculate margins of error for derived variables

## Usage

``` r
calculate_moes(.df)
```

## Arguments

- .df:

  The dataset returned from
  [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
  The argument to this parameter must have an attribute named `codebook`
  (as is true of results from `compile_acs_data())`.

## Value

A modified dataframe that includes margins of error (suffixed `_M`) for
derived variables.

## Details

Calculates margins of error for all derived ACS estimates. Standard
errors are computed internally as an intermediate step but are not
included in the returned dataframe. Uses pre-parsed codebook columns
(numerator_vars, denominator_vars, se_calculation_type) to determine how
to calculate standard errors.
