# Division without NaNs

A division operation that returns `NA` instead of `NaN` or `Inf` when
the denominator is zero.

## Usage

``` r
safe_divide(x, y)
```

## Arguments

- x:

  A numeric vector or scalar.

- y:

  A numeric vector or scalar.

## Value

A numeric vector. See `Details` for the behavior at `y == 0`.

## Details

Returns the quotient `x / y`, except that any division by zero returns
`NA_real_`. This includes `0 / 0`: a ratio whose denominator is zero is
undefined, so, for example, the share of a tract's population with
health insurance is `NA` (not `0`) when the tract has no population.
Complements of such ratios (see
[`define_complement`](https://ui-research.github.io/urbnindicators/reference/define_complement.md))
are then `NA` as well, rather than `1`.

When `x` or `y` is `NA`, the result is `NA`.

## Examples

``` r
safe_divide(1, 2)   # 0.5
#> [1] 0.5
safe_divide(0, 0)   # NA
#> [1] NA
safe_divide(3, 0)   # NA
#> [1] NA
safe_divide(3, NA)  # NA
#> [1] NA
```
