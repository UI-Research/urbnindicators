# Calculate standard error for a product of two estimates

Calculate standard error for a product of two estimates

## Usage

``` r
se_product(
  estimate_x,
  estimate_y,
  se_x = NULL,
  se_y = NULL,
  moe_x = NULL,
  moe_y = NULL
)
```

## Arguments

- estimate_x:

  The first estimate (X)

- estimate_y:

  The second estimate (Y)

- se_x:

  The standard error of estimate X (or NULL if providing MOE)

- se_y:

  The standard error of estimate Y (or NULL if providing MOE)

- moe_x:

  The margin of error of estimate X (or NULL if providing SE)

- moe_y:

  The margin of error of estimate Y (or NULL if providing SE)

## Value

The standard error of the product X\*Y

## Details

Calculate the standard error for an estimate derived by multiplying two
estimates together. For example, multiplying a proportion by a
population count to get a subgroup count. Formula from Census Bureau ACS
Accuracy documentation: SE(X*Y) = sqrt((X*SE(Y))^2 + (Y\*SE(X))^2).
