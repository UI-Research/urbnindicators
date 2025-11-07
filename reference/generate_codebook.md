# Document variables from `compile_acs_data()`

Define how variables produced via
[`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
are calculated.

## Usage

``` r
generate_codebook(.data)
```

## Arguments

- .data:

  The dataset returned from
  [`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

## Value

A tibble containing the names and definitions of variables returned from
[`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

## Details

Generates a tibble of variable names and definitions that describe how
each variable was created.

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
codebook = generate_codebook(.data = df)
} # }
```
