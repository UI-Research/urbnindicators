# Return ACS variables codes and names

Generate meaningful names for ACS variable codes based on their metadata
and return these as a vector, along with their semantic names. Intended
for use with
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

## Usage

``` r
list_acs_variables(year = "2022")
```

## Arguments

- year:

  The year for which variable names should be selected.

## Value

A named vector of variable codes (as specified in the Census Bureau's
API) with semantically-meaningful names (e.g.,
"race_black_alone_nonhispanic").

## Examples

``` r
list_acs_variables(year = "2022") %>% head()
#>               total_population_universe_ 
#>                             "B01003_001" 
#>              public_assistance_universe_ 
#>                             "B19058_001" 
#>              public_assistance_received_ 
#>                             "B19058_002" 
#>                           snap_universe_ 
#>                             "B22003_001" 
#>                           snap_received_ 
#>                             "B22003_002" 
#> household_income_quintile_upper_limit_1_ 
#>                             "B19080_001" 
```
