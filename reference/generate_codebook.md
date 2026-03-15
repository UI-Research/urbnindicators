# Document variables from `compile_acs_data()`

Define how variables produced via
[`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
are calculated.

## Usage

``` r
generate_codebook(
  .data,
  resolved_tables = NULL,
  auto_table_entries = list(),
  user_definitions = list()
)
```

## Arguments

- .data:

  The dataset returned from
  [`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

- resolved_tables:

  A character vector of resolved table names from the table registry.
  When NULL (default), all registered tables are used.

- auto_table_entries:

  A list of auto-generated table entries from
  `build_auto_table_entry()`. Default is an empty list.

- user_definitions:

  A list of user-supplied DSL definition objects (e.g., from
  [`define_percent()`](https://ui-research.github.io/urbnindicators/reference/define_percent.md)).
  Default is an empty list.

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
  years = c(2024),
  geography = "county",
  states = "NJ",
  counties = NULL,
  spatial = FALSE) %>%
  dplyr::select(-dplyr::matches("_M$"))
codebook = generate_codebook(.data = df)
} # }
```
