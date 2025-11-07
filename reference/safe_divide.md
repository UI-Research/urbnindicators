# Division without NAs

Return 0 when the divisor is 0.

## Usage

``` r
safe_divide(x, y)
```

## Arguments

- x:

  A numeric scalar.

- y:

  A numeric scalar.

## Value

The traditional dividend in all cases except where `y == 0`, in which
case it returns 0.

## Details

A modified division operation that returns zero when the divisor is zero
rather than returning NA. Otherwise returns the quotient.

## Examples

``` r
safe_divide(1, 2)
#> [1] 0.5
safe_divide(3, 0)
#> [1] 0
```
