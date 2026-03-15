# Define a one-minus (complement) variable

Creates a definition that computes `1 - source_variable`.

## Usage

``` r
define_one_minus(output, source_variable)
```

## Arguments

- output:

  A string. The name of the output column to create.

- source_variable:

  A string. The column to subtract from 1.

## Value

A list with `type = "one_minus"` and the associated fields. Can be
passed in the `tables` parameter of
[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
