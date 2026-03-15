# Define an across-sum variable

Creates a definition that sums each matched column with a corresponding
addend column. The addend and output names are determined by
user-supplied functions.

## Usage

``` r
define_across_sum(
  input_regex,
  addend_function,
  output_naming_function,
  exclude_regex = NULL
)
```

## Arguments

- input_regex:

  A regex pattern to match input columns.

- addend_function:

  A function that takes a matched column name and returns the name of
  the column to add.

- output_naming_function:

  A function that takes a matched column name and returns the output
  column name.

- exclude_regex:

  A regex pattern to exclude from matched columns.

## Value

A list with `type = "across_sum"` and the associated fields. Can be
passed in the `tables` parameter of
[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
