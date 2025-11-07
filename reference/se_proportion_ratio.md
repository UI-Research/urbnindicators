# Calculate a pooled standard error for a proportion or ratio

Calculate a pooled standard error for a proportion or ratio

## Usage

``` r
se_proportion_ratio(
  estimate_numerator,
  estimate_denominator,
  moe_numerator = NULL,
  moe_denominator = NULL,
  se_numerator = NULL,
  se_denominator = NULL,
  type = "proportion"
)
```

## Arguments

- estimate_numerator:

  The estimate of the numerator

- estimate_denominator:

  The estimate of the denominator

- moe_numerator:

  The margin of error of the numerator

- moe_denominator:

  The margin of error of the denominator

- se_numerator:

  The standard error of the numerator

- se_denominator:

  The standard error of the denominator

- type:

  The type of estimate being calculated, either "proportion" or "ratio"

## Value

A pooled 90% standard error

## Details

For an estimate derived using division–whether the resulting estimate is
a proportion or a ratio–calculate the pooled standard error. While there
are convenience parameters that support both SEs and MOEs for the
numerator and denominator, only one of these–either SEs or MOEs–can be
supplied–the other must be left NULL.
