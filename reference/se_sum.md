# Calculate a pooled standard error for a summed or subtracted estimate

Calculate a pooled standard error for a summed or subtracted estimate

## Usage

``` r
se_sum(errors, estimates, from = "moe")
```

## Arguments

- errors:

  A list of vectors. Either MOEs or SEs for each component (controlled
  by `from`).

- estimates:

  A list of vectors. The corresponding estimates for each component.
  Used for the Census Bureau zero-estimate rule.

- from:

  Either `"moe"` (default) or `"se"`. Tells `se_sum` whether `errors`
  contains MOEs or SEs.

## Value

A pooled 90% standard error

## Details

For an estimate derived by adding or subtracting multiple estimates,
calculate the pooled standard error. Inputs can be supplied as 90%
margins of error (`from = "moe"`, the default) or as standard errors
(`from = "se"`).
