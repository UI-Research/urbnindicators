# Define a percentage variable

Creates a definition for a derived percentage (proportion) variable.
Handles both single-output and batch modes:

- **Single output** (`each = FALSE`): computes one percentage from the
  specified numerator and denominator columns.

- **Batch output** (`each = TRUE`): computes one percentage per column
  matching the `numerator` pattern. Output columns are named
  `{matched_column}_percent`.

## Usage

``` r
define_percent(
  numerator,
  denominator = NULL,
  output = NULL,
  each = FALSE,
  denominator_replace = NULL,
  subtract_from_numerator = NULL,
  subtract_from_denominator = NULL,
  exclude = NULL
)
```

## Arguments

- numerator:

  A column name (string), character vector of column names to sum, or
  regex pattern (when `each = TRUE`). When a character vector of length
  \> 1, columns are summed. When a single string and `each = FALSE`,
  treated as a column name if it contains no regex metacharacters,
  otherwise as a regex whose matches are summed.

- denominator:

  A column name (string) or character vector of column names to sum.
  When a single string, treated as a column name if it contains no regex
  metacharacters, otherwise as a regex whose matches are summed. Not
  required when `denominator_replace` is provided.

- output:

  A string. The output column name. Auto-inferred as
  `paste0(numerator, "_percent")` when `numerator` is a single non-regex
  string and `each = FALSE`. Required when `numerator` is a vector or
  regex. Ignored when `each = TRUE` (outputs are named
  `{matched_column}_percent`).

- each:

  Logical. When `TRUE`, `numerator` is treated as a regex pattern and
  one percentage is computed per matched column. Default `FALSE`.

- denominator_replace:

  A named character vector for deriving the denominator column name from
  the matched numerator column name via string replacement (e.g.,
  `c("below" = "universe")`). Only used when `each = TRUE`.

- subtract_from_numerator:

  Column name(s) to subtract from the numerator sum (string or character
  vector).

- subtract_from_denominator:

  Column name(s) to subtract from the denominator sum (string or
  character vector).

- exclude:

  A regex pattern to exclude columns from pattern matching.

## Value

A list with a `type` field and associated fields, suitable for passing
in the `tables` parameter of
[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

## Examples

``` r
# Simple percentage (output inferred as "snap_received_percent")
define_percent("snap_received", "snap_universe")
#> $type
#> [1] "complex"
#> 
#> $output
#> [1] "snap_received_percent"
#> 
#> $numerator_regex
#> [1] "snap_received"
#> 
#> $denominator_regex
#> [1] "snap_universe"
#> 

# Sum of columns as numerator
define_percent(c("age_under_5_years", "age_5_9_years"),
              denominator = "sex_by_age_universe",
              output = "age_under_10_percent")
#> $type
#> [1] "complex"
#> 
#> $output
#> [1] "age_under_10_percent"
#> 
#> $numerator_variables
#> [1] "age_under_5_years" "age_5_9_years"    
#> 
#> $denominator_regex
#> [1] "sex_by_age_universe"
#> 

# Batch: one percent per matched column
define_percent("^race_nonhispanic|^race_hispanic",
              denominator = "race_universe",
              each = TRUE)
#> $type
#> [1] "across_percent"
#> 
#> $input_regex
#> [1] "^race_nonhispanic|^race_hispanic"
#> 
#> $output_suffix
#> [1] "_percent"
#> 
#> $denominator
#> [1] "race_universe"
#> 
```
