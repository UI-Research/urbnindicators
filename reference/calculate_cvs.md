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

Create CVs for all ACS estimates and derived indicators. Uses pre-parsed
codebook columns (numerator_vars, denominator_vars, se_calculation_type)
to determine how to calculate standard errors.
