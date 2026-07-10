# Division without NaNs

A division operation that distinguishes structurally-zero ratios from
undefined ones.

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

Returns the quotient `x / y` except in two cases:

- When both `x` and `y` are `0`, returns `0` (treating `0 / 0` as a
  structurally-zero ratio rather than NaN).

- When `y` is `0` and `x` is non-zero (positive or negative), returns
  `NA_real_` (the ratio is undefined).

When `x` or `y` is `NA`, the result is `NA`.

## Examples

``` r
safe_divide(1, 2)   # 0.5
#> [1] 0.5
safe_divide(0, 0)   # 0
#> [1] 0
safe_divide(3, 0)   # NA
#> [1] NA
safe_divide(3, NA)  # NA
#> [1] NA
```
