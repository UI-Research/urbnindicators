# Define a sum variable

Creates a definition for a derived sum variable. Handles both
single-output and batch modes:

- **Single output** (`each = FALSE`): sums the specified columns into
  one output column.

- **Batch output** (`each = TRUE`): for each column matching the
  `columns` pattern, adds a corresponding column (derived via
  `add_replace`) and names the output via `output_replace`.

## Usage

``` r
define_sum(
  columns,
  output = NULL,
  each = FALSE,
  add_replace = NULL,
  output_replace = NULL,
  exclude = NULL
)
```

## Arguments

- columns:

  Column name(s) to sum (character vector), or a regex pattern when
  `each = TRUE`.

- output:

  A string. The output column name. Required when `each = FALSE`.

- each:

  Logical. When `TRUE`, `columns` is treated as a regex pattern and one
  pairwise sum is computed per matched column. Default `FALSE`.

- add_replace:

  A named character vector for deriving the addend column name from the
  matched column name via string replacement (e.g.,
  `c("female" = "male")`). Only used when `each = TRUE`.

- output_replace:

  A named character vector for deriving the output column name from the
  matched column name via string replacement (e.g.,
  `c("sex_by_age_female_" = "age_")`). Only used when `each = TRUE`.

- exclude:

  A regex pattern to exclude columns from pattern matching.

## Value

A list with `type` field and associated fields, suitable for passing in
the `tables` parameter of
[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

## Examples

``` r
# Simple sum
define_sum(c("col_a", "col_b", "col_c"), output = "total_abc")
#> $type
#> [1] "sum"
#> 
#> $output
#> [1] "total_abc"
#> 
#> $columns
#> [1] "col_a" "col_b" "col_c"
#> 

# Batch: pairwise sum per matched column
define_sum("sex_by_age_female_.*years($|_over$)",
           each = TRUE,
           add_replace = c("female" = "male"),
           output_replace = c("sex_by_age_female_" = "age_"))
#> $type
#> [1] "across_sum"
#> 
#> $input_regex
#> [1] "sex_by_age_female_.*years($|_over$)"
#> 
#> $addend_function
#> function (column) 
#> {
#>     purrr::reduce2(names(add_replace), add_replace, function(col, 
#>         pattern, replacement) {
#>         stringr::str_replace(col, pattern, replacement)
#>     }, .init = column)
#> }
#> <bytecode: 0x55afb94bbe58>
#> <environment: 0x55afb932b790>
#> 
#> $output_naming_function
#> function (column) 
#> {
#>     purrr::reduce2(names(output_replace), output_replace, function(col, 
#>         pattern, replacement) {
#>         stringr::str_replace_all(col, pattern, replacement)
#>     }, .init = column)
#> }
#> <bytecode: 0x55afb94b8d48>
#> <environment: 0x55afb932b790>
#> 
```
