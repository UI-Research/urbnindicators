# Define a percentage variable (simple or complex)

Creates a definition object for a derived percentage variable. When both
`numerator` and `denominator` are single strings and no other fields are
set, a `simple_percent` definition is returned. Otherwise a `complex`
definition is returned, allowing multi-variable numerators and
denominators.

## Usage

``` r
define_percent(
  output,
  numerator = NULL,
  denominator = NULL,
  numerator_variables = NULL,
  numerator_regex = NULL,
  numerator_exclude_regex = NULL,
  numerator_note = NULL,
  numerator_subtract_variables = NULL,
  numerator_subtract_regex = NULL,
  denominator_variables = NULL,
  denominator_regex = NULL,
  denominator_exclude_regex = NULL,
  subtract_variables = NULL,
  subtract_regex = NULL
)
```

## Arguments

- output:

  A string. The name of the output column to create.

- numerator:

  A string. Single numerator column name (simple case).

- denominator:

  A string. Single denominator column name (simple case).

- numerator_variables:

  A character vector of column names to sum for the numerator (complex
  case).

- numerator_regex:

  A regex pattern to match numerator columns (complex case).

- numerator_exclude_regex:

  A regex pattern to exclude from numerator matches.

- numerator_note:

  An optional annotation (not used in computation).

- numerator_subtract_variables:

  A character vector of column names to subtract from the numerator sum.

- numerator_subtract_regex:

  A regex pattern to match columns to subtract from the numerator.

- denominator_variables:

  A character vector of column names to sum for the denominator (complex
  case).

- denominator_regex:

  A regex pattern to match denominator columns (complex case).

- denominator_exclude_regex:

  A regex pattern to exclude from denominator matches.

- subtract_variables:

  A character vector of column names to subtract from the denominator
  sum.

- subtract_regex:

  A regex pattern to match columns to subtract from the denominator.

## Value

A list with a `type` field (`"simple_percent"` or `"complex"`) and the
associated fields. Can be passed in the `tables` parameter of
[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

## Examples

``` r
# Simple percentage
define_percent("snap_received_percent",
              numerator = "snap_received",
              denominator = "snap_universe")
#> $type
#> [1] "simple_percent"
#> 
#> $output
#> [1] "snap_received_percent"
#> 
#> $numerator
#> [1] "snap_received"
#> 
#> $denominator
#> [1] "snap_universe"
#> 

# Complex percentage with subtraction
define_percent("snap_not_received_percent",
              numerator_variables = c("snap_universe"),
              numerator_subtract_variables = c("snap_received"),
              denominator_variables = c("snap_universe"))
#> $type
#> [1] "complex"
#> 
#> $output
#> [1] "snap_not_received_percent"
#> 
#> $numerator_variables
#> [1] "snap_universe"
#> 
#> $numerator_subtract_variables
#> [1] "snap_received"
#> 
#> $denominator_variables
#> [1] "snap_universe"
#> 
```
