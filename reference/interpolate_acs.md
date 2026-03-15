# Aggregate or interpolate ACS data to custom geographies

Aggregate or interpolate ACS data from source geographies to
user-defined target geographies. Supports two modes:

**Complete nesting** (`weight = NULL`): Each source geography maps
entirely to one target geography. Count variables are summed,
percentages are recalculated from summed components, and intensive
variables (medians, averages) are computed as population-weighted
averages.

**Fractional allocation** (`weight = "column_name"`): Source geographies
can be split across multiple targets using crosswalk weights. Count
variables and MOEs are multiplied by the weight before summing.
Percentages are recalculated from interpolated components. Intensive
variables use the allocated population as weights.

MOE propagation uses Census Bureau approximation formulas throughout.
Crosswalk weights are treated as constants (no sampling error).

## Usage

``` r
interpolate_acs(
  .data,
  target_geoid,
  weight = NULL,
  crosswalk = NULL,
  source_geoid = "GEOID",
  weight_variable = "total_population_universe"
)
```

## Arguments

- .data:

  A dataframe returned from
  [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
  Must have a codebook attribute attached.

- target_geoid:

  Character. Column name for target geography identifiers. Must exist in
  `.data` or in `crosswalk`. The result renames this column to `GEOID`.

- weight:

  Character or `NULL`. When `NULL` (default), assumes complete nesting
  where each source geography maps entirely to one target. When a column
  name is provided, performs fractional allocation using that column as
  weights. Weights should sum to approximately 1 per source geography.

- crosswalk:

  A data frame containing the crosswalk mapping. Optional in both modes.
  When provided, joined to `.data` via `source_geoid` before processing.
  Must include columns for `source_geoid` and `target_geoid` (and
  `weight` if fractional allocation is used).

- source_geoid:

  Character. Column name for source geography identifiers. Must exist in
  `.data` (and in `crosswalk` if provided). Default is `"GEOID"`.

- weight_variable:

  Character. Variable name used for population-weighted averages of
  intensive variables (medians, averages, etc.). Default is
  `"total_population_universe"`.

## Value

A dataframe aggregated to target geographies with recalculated
estimates, MOEs, and percentages. A modified codebook is attached as an
attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, create tract-level data
tract_data = compile_acs_data(
  tables = c("race", "snap"),
  years = 2022,
  geography = "tract",
  states = "DC"
)

# Complete nesting: each tract belongs to exactly one neighborhood
tract_data$neighborhood = c("Downtown", "Downtown", "Uptown", ...)
neighborhood_data = interpolate_acs(
  .data = tract_data,
  target_geoid = "neighborhood"
)

# Fractional allocation with a crosswalk
crosswalk = data.frame(
  GEOID = c("11001000100", "11001000100", "11001000201"),
  neighborhood = c("Downtown", "Chinatown", "Downtown"),
  alloc_weight = c(0.6, 0.4, 1.0)
)

neighborhood_data = interpolate_acs(
  .data = tract_data,
  target_geoid = "neighborhood",
  weight = "alloc_weight",
  crosswalk = crosswalk
)
} # }
```
