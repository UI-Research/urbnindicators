# Easily filter ACS variables

Filter the the results of
[`select_variables_by_name()`](https://ui-research.github.io/urbnindicators/reference/select_variables_by_name.md)
based on their `match_type` relative to `match_string`.

## Usage

``` r
filter_variables(variable_vector, match_string, match_type = "positive")
```

## Arguments

- variable_vector:

  A named vector (intended for use with named ACS variables).

- match_string:

  A string on which to filter (or not filter) elements in
  `variable_vector`.

- match_type:

  Whether to include (`match_type = "positive"`) or exclude
  (`match_type = "negative"`) matching elements.

## Value

The elements from `variable_vector` that do/don't match `match_string`.

## Examples

``` r
if (FALSE) { # \dontrun{
codebook = tidycensus::load_variables(dataset = "acs5", year = 2022)
selected_variables = select_variables_by_name("B16005_", census_codebook = codebook)
filter_variables(
  variable_vector = selected_variables,
  match_string = "universe_$|native_$|foreign_born_$|only|very_well",
  match_type = "positive")
} # }
```
