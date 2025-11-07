# Easily rename ACS variables

Given the name of an ACS variable (or a string that matches one or more
such variables), generate a named character vector of original variable
names and more meaningful names.

## Usage

``` r
select_variables_by_name(variable_name, census_codebook)
```

## Arguments

- variable_name:

  A named vector (intended for use with named ACS variables).

- census_codebook:

  An object returned from
  [`tidycensus::load_variables()`](https://walker-data.com/tidycensus/reference/load_variables.html).

## Value

A named character vector containing the variables that matched
`variable_name` from the `census_codebook`, with semantically-meaningful
names derived from metadata fields contained in `census_codebook`.

## Examples

``` r
if (FALSE) { # \dontrun{
codebook = tidycensus::load_variables(dataset = "acs5", year = 2022)
select_variables_by_name("B16005_", census_codebook = codebook)
} # }
```
