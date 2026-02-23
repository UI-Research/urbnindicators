# Aggregate ACS data to custom geographies

Aggregate tract-level ACS data to user-defined custom geographies by
properly handling different variable types (counts, percentages,
medians, etc.) and recalculating all error measures appropriately.

## Usage

``` r
calculate_custom_geographies(
  .data,
  group_id,
  spatial = FALSE,
  weight_variable = "total_population_universe"
)
```

## Arguments

- .data:

  A dataframe returned from
  [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
  at the tract level. Must have a codebook attribute attached.

- group_id:

  Character. The name of a column in `.data` that contains the custom
  geography identifiers to aggregate to.

- spatial:

  Logical. If TRUE, dissolve tract geometries to create custom geography
  boundaries using
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html).
  Default is FALSE.

- weight_variable:

  Character. The variable name to use for population-weighted averages
  of non-aggregatable variables. Default is "total_population_universe".

## Value

A dataframe aggregated to custom geographies with recalculated
estimates, MOEs, SEs, and CVs. A modified codebook is attached as an
attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, create tract-level data
tract_data = compile_acs_data(
  years = 2022,
  geography = "tract",
  states = "DC"
)

# Add a custom geography column (e.g., from a crosswalk)
tract_data_with_neighborhoods = tract_data %>%
  dplyr::left_join(neighborhood_crosswalk, by = "GEOID")

# Aggregate to custom geographies
neighborhood_data = calculate_custom_geographies(
  .data = tract_data_with_neighborhoods,
  group_id = "neighborhood_id",
  spatial = TRUE
)
} # }
```
