# Define a complement (1 - x) variable

Creates a definition that computes `1 - source`.

## Usage

``` r
define_complement(source, output)
```

## Arguments

- source:

  A string. The column to subtract from 1.

- output:

  A string. The name of the output column to create.

## Value

A list with `type = "one_minus"` and the associated fields. Can be
passed in the `tables` parameter of
[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
