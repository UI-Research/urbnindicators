# Define a metadata variable

Creates a definition for a non-computed variable that only generates a
codebook entry.

## Usage

``` r
define_metadata(output, definition_text)
```

## Arguments

- output:

  A string. The name of the metadata column.

- definition_text:

  A string. Human-readable description for the codebook.

## Value

A list with `type = "metadata"` and the associated fields. Can be passed
in the `tables` parameter of
[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
