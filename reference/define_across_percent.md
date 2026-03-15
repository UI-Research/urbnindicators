# Define an across-percent variable

Creates a definition that computes a percentage for every column
matching a regex pattern. Each matched column becomes a numerator; the
denominator is either a fixed column or computed by a function.

## Usage

``` r
define_across_percent(
  input_regex,
  output_suffix,
  denominator = NULL,
  denominator_function = NULL,
  denominator_subtract = NULL,
  exclude_regex = NULL
)
```

## Arguments

- input_regex:

  A regex pattern to match input columns.

- output_suffix:

  A string appended to each matched column name to form the output
  column name (e.g., `"_percent"`).

- denominator:

  A string. A fixed denominator column name.

- denominator_function:

  A function that takes a matched column name and returns the
  denominator column name for that match.

- denominator_subtract:

  A string. A column to subtract from the denominator value.

- exclude_regex:

  A regex pattern to exclude from matched columns.

## Value

A list with `type = "across_percent"` and the associated fields. Can be
passed in the `tables` parameter of
[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
