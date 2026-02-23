# List available indicators

Returns a tibble of all derived indicator names and their parent tables,
for use with the `indicators` parameter of
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
Table names reflect construct-level names (e.g., `"age"` rather than
`"sex_by_age"`).

## Usage

``` r
list_indicators()
```

## Value

A tibble with columns `indicator` and `table`.
