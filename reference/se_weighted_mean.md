# Calculate standard error for population-weighted mean

Calculate standard error for population-weighted mean

## Usage

``` r
se_weighted_mean(
  values,
  weights,
  se_values = NULL,
  se_weights = NULL,
  moe_values = NULL,
  moe_weights = NULL
)
```

## Arguments

- values:

  A numeric vector of values being averaged (e.g., median incomes)

- weights:

  A numeric vector of population weights

- se_values:

  Standard errors for the values (or NULL if providing moe_values)

- se_weights:

  Standard errors for the weights (or NULL if providing moe_weights)

- moe_values:

  Margins of error for the values (or NULL if providing se_values)

- moe_weights:

  Margins of error for the weights (or NULL if providing se_weights)

## Value

The standard error of the weighted mean

## Details

Calculate the standard error for a population-weighted average, used
when aggregating median or average variables across geographies. Uses a
multi-step approach following Census Bureau guidance:

1.  Calculate SE for each product (value \* weight) using se_product()

2.  Calculate SE for the sum of products (numerator) using se_sum()

3.  Calculate SE for the sum of weights (denominator) using se_sum()

4.  Calculate SE for the ratio using se_proportion_ratio(type = "ratio")
