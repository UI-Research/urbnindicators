# ACS Table Structures

``` r

library(urbnindicators)
library(dplyr)
library(stringr)
library(tidycensus)
library(reactable)
```

This vignette explains how `urbnindicators` translates raw ACS data into
analysis-ready output. It covers:

1.  How ACS tables are structured at the Census Bureau
2.  How the package selects and renames variables
3.  How percentages are calculated
4.  What the output looks like and how to interpret it

## How ACS Tables Are Structured

The Census Bureau organizes ACS data into thousands of **tables**, each
identified by a code like `B22003` or `B01001`. Each table contains a
set of numbered variables—`B22003_001`, `B22003_002`, etc.—that report
counts for specific subgroups.

Tables have a hierarchical label structure. For example, table B22003
(Receipt of Food Stamps/SNAP) looks like:

| Variable | Label | Role |
|----|----|----|
| B22003_001 | Total | Universe (all households) |
| B22003_002 | Received Food Stamps/SNAP | Subgroup count |
| B22003_003 | … with 1 or more persons with a disability | Sub-subgroup |
| B22003_004 | … with no persons with a disability | Sub-subgroup |
| B22003_005 | Did not receive Food Stamps/SNAP | Subgroup count |

The first variable (`_001`) is always the table **total**, or
universe—the count of all observations to which the table applies.
Subsequent variables break down the total into subcategories, sometimes
nested several levels deep.

### Table prefixes

The Census Bureau uses prefixes to distinguish table types:

- **B** (Detailed Tables): The most granular tabulations, with the full
  set of categories.
- **C** (Collapsed Tables): Simplified versions of B tables with fewer
  categories. A given C table typically mirrors a B table but groups
  some subcategories together.
- **S** (Subject Tables): Pre-computed summaries, often including
  percentages and medians. These are not used by `urbnindicators`
  directly; the package computes its own percentages from B and C
  tables.

`urbnindicators` works with B and C tables. You can browse the full
Census Bureau variable catalog with
[`get_acs_codebook()`](https://ui-research.github.io/urbnindicators/reference/get_acs_codebook.md):

``` r

get_acs_codebook() %>%
  filter(table == "B22003") %>%
  head(6)
#> # A tibble: 6 × 3
#>   table  variable_raw variable_clean                                            
#>   <chr>  <chr>        <chr>                                                     
#> 1 B22003 B22003_001   receipt_food_stamps/snap_in_past_12_months_by_poverty_sta…
#> 2 B22003 B22003_002   receipt_food_stamps/snap_in_past_12_months_by_poverty_sta…
#> 3 B22003 B22003_003   receipt_food_stamps/snap_in_past_12_months_by_poverty_sta…
#> 4 B22003 B22003_004   receipt_food_stamps/snap_in_past_12_months_by_poverty_sta…
#> 5 B22003 B22003_005   receipt_food_stamps/snap_in_past_12_months_by_poverty_sta…
#> 6 B22003 B22003_006   receipt_food_stamps/snap_in_past_12_months_by_poverty_sta…
```

## Two Paths: Registered Tables and Auto-Processed Tables

[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
supports two kinds of table inputs, each processed differently.

### Registered tables

These are tables that the package has pre-built definitions for. They
have curated variable names, carefully chosen denominators, and tested
percentage calculations. Use
[`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)
to see what’s available:

``` r

list_tables()
#>  [1] "age"                          "computing_devices"           
#>  [3] "cost_burden"                  "disability"                  
#>  [5] "educational_attainment"       "employment"                  
#>  [7] "gini"                         "health_insurance"            
#>  [9] "household_size"               "income_quintiles"            
#> [11] "internet"                     "language"                    
#> [13] "median_household_income"      "median_housing_cost"         
#> [15] "median_income_by_tenure"      "mortgage_status"             
#> [17] "nativity"                     "occupants_per_room"          
#> [19] "owner_cost_burden"            "population_density"          
#> [21] "poverty"                      "public_assistance"           
#> [23] "race"                         "school_enrollment"           
#> [25] "sex"                          "snap"                        
#> [27] "tenure"                       "tenure_by_housing_costs"     
#> [29] "tenure_by_units_in_structure" "total_population"            
#> [31] "transportation_to_work"       "travel_time_to_work"         
#> [33] "units_in_structure"           "vehicles_available"          
#> [35] "year_structure_built"
```

When you request a registered table by name (e.g., `tables = "snap"`),
the package knows exactly which ACS variables to fetch and how to
calculate derived measures from them.

### Auto-processed tables

Any valid ACS table code can be passed directly (e.g.,
`tables = "B25070"`). The table is **auto-processed**: all of its
variables are fetched, names are generated algorithmically from the
Census Bureau’s labels, and percentages are computed based on the
table’s label hierarchy. This holds even when a registered table covers
the same code — `tables = "B22003"` returns the auto-processed table,
while `tables = "snap"` returns the registered version. (Requesting both
in the same call returns the registered version and drops the
auto-processed one with a warning, since the two would otherwise fetch
duplicate variables.)

Auto-processed tables are a flexible route for the thousands of ACS
tables that `urbnindicators` doesn’t have pre-built definitions for. The
tradeoff is that variable names tend to be longer and less polished
(they’re derived mechanically from Census labels).

## How Variables Are Selected and Renamed

### Registered tables

For registered tables, the package maintainers have manually identified
constructs that are widely used in analyses, vetted how to calculate
these constructs, and simplified the resulting variable names to follow
a concise and consistent structure:

| ACS code   | `urbnindicators` name |
|------------|-----------------------|
| B22003_001 | `snap_universe`       |
| B22003_002 | `snap_received`       |

These names follow the pattern:
`[concept]_[subconcept]_[characteristic]_[metric]`.

Some registered tables use **pattern-based selection** instead of
listing variables one by one. For example, the `sex_by_age` table
selects all variables matching the pattern `B01001_`, which pulls every
variable in table B01001 and renames them by parsing the Census Bureau’s
hierarchical labels into snake_case.

### Auto-processed tables

For auto-processed tables, variable names are generated by converting
the Census Bureau’s `concept` and `label` fields to snake_case. For
example, a variable from table B25070 with concept “GROSS RENT AS A
PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS” and label “Less
than 10.0 percent” becomes:

    gross_rent_as_a_percentage_household_income_in_past_12_months_less_than_10_0_pct

(Common connector words like “of” and “the” are collapsed, and a
trailing “percent” becomes `pct` so that computed percentage
variables–which end in `_percent`–remain distinguishable from raw ACS
variables that happen to measure percentages.)

These names are functional but verbose. To see what names a table will
produce before pulling data, you can use
[`get_acs_codebook()`](https://ui-research.github.io/urbnindicators/reference/get_acs_codebook.md):

``` r

get_acs_codebook(table = "B25070")
#> # A tibble: 11 × 3
#>    table  variable_raw variable_clean                                           
#>    <chr>  <chr>        <chr>                                                    
#>  1 B25070 B25070_001   gross_rent_as_a_percentage_household_income_in_past_12_m…
#>  2 B25070 B25070_002   gross_rent_as_a_percentage_household_income_in_past_12_m…
#>  3 B25070 B25070_003   gross_rent_as_a_percentage_household_income_in_past_12_m…
#>  4 B25070 B25070_004   gross_rent_as_a_percentage_household_income_in_past_12_m…
#>  5 B25070 B25070_005   gross_rent_as_a_percentage_household_income_in_past_12_m…
#>  6 B25070 B25070_006   gross_rent_as_a_percentage_household_income_in_past_12_m…
#>  7 B25070 B25070_007   gross_rent_as_a_percentage_household_income_in_past_12_m…
#>  8 B25070 B25070_008   gross_rent_as_a_percentage_household_income_in_past_12_m…
#>  9 B25070 B25070_009   gross_rent_as_a_percentage_household_income_in_past_12_m…
#> 10 B25070 B25070_010   gross_rent_as_a_percentage_household_income_in_past_12_m…
#> 11 B25070 B25070_011   gross_rent_as_a_percentage_household_income_in_past_12_m…
```

## How Percentages Are Calculated

### Registered tables

Registered tables define their percentages explicitly. Each table’s
definitions specify which variable(s) form the numerator, which form the
denominator, and what the output variable should be called. For example,
`snap_received_percent` is documented in the codebook as:

    Numerator = snap_received (B22003_002). Denominator = snap_universe (B22003_001).

Some percentage calculations are more complex. For example,
`disability_percent` sums numerous sex-by-age-by-disability variables
into a single numerator, then divides by the table universe.

All percentage variables end with `_percent`.

### Auto-processed tables

For auto-processed tables, the package builds a label hierarchy tree
from the Census Bureau’s ACS codebook. Each variable’s “parent” is
determined by its position in the hierarchy:

| Variable   | Label                  | Depth | Parent                        |
|------------|------------------------|-------|-------------------------------|
| B25070_001 | Total                  | 1     | *(none—this is the universe)* |
| B25070_002 | Less than 10.0 percent | 2     | B25070_001                    |
| B25070_003 | 10.0 to 14.9 percent   | 2     | B25070_001                    |

The `denominator` parameter on
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
controls which variable is used as the denominator for auto-computed
percentages:

- **`"parent"`** (default): Each variable is divided by its nearest
  parent subtotal in the label hierarchy. For flat tables (like B25070,
  where all variables are direct children of the total), this is
  equivalent to dividing by the table total. For nested tables with
  subtotals, this produces within-group percentages.
- **`"total"`**: Every variable is divided by the table total (`_001`),
  producing shares of the overall population regardless of hierarchy.
- **A specific ACS variable code** (e.g., `"B25070_001"`): Every
  variable is divided by that variable.

To illustrate: consider a table with structure Total \> Male \> Male
18-24, Male 25-34, … and Total \> Female \> Female 18-24, Female 25-34,
…

With `denominator = "parent"`, “Male 18-24” would be divided by “Male”
(its parent subtotal), yielding the share *of males* in that age group.
With `denominator = "total"`, “Male 18-24” would be divided by “Total”,
yielding the share *of all adults*.

The package skips percentage computation for tables that contain
medians, aggregates, averages, or other non-count measures.

## Understanding the Output

[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
returns a wide-format dataframe with one row per geography per year.
Here’s what the columns look like:

``` r

df = compile_acs_data(
  tables = "snap",
  years = 2024,
  geography = "county",
  states = "DC")

names(df)
#>  [1] "data_source_year"            "GEOID"                      
#>  [3] "NAME"                        "total_population_universe"  
#>  [5] "snap_universe"               "snap_received"              
#>  [7] "snap_received_percent"       "total_population_universe_M"
#>  [9] "snap_universe_M"             "snap_received_M"            
#> [11] "snap_received_percent_M"
```

### Column types

The output contains several types of columns, identifiable by their
naming conventions:

| Pattern | Type | Example | Description |
|----|----|----|----|
| `GEOID` | Identifier | `"11001"` | FIPS code for the geography |
| `NAME` | Identifier | `"District of Columbia"` | Human-readable name |
| `data_source_year` | Identifier | `2024` | The ACS release year (end year of the 5-year window) |
| `*_universe` | Count | `snap_universe` | Table total / denominator population |
| *(no suffix)* | Count | `snap_received` | Raw ACS estimate |
| `*_percent` | Derived | `snap_received_percent` | Percentage (0–1 scale) |
| `*_M` | MOE | `snap_received_M` | Margin of error (90% confidence) |

Every raw count variable has a corresponding `_M` column with its margin
of error, sourced directly from the Census Bureau. Every derived
percentage variable also has a `_M` column, with the margin of error
calculated by `urbnindicators` using Census Bureau-recommended formulas
for pooling errors.

### The codebook

Every dataframe returned by
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
carries a codebook as an attribute. The codebook documents how each
variable was calculated:

``` r

attr(df, "codebook")
#> # A tibble: 11 × 10
#>    calculated_variable          variable_type definition          numerator_vars
#>    <chr>                        <chr>         <chr>               <list>        
#>  1 total_population_universe    Count         This is a raw ACS … <chr [0]>     
#>  2 snap_universe                Count         This is a raw ACS … <chr [0]>     
#>  3 snap_received                Count         This is a raw ACS … <chr [0]>     
#>  4 snap_received_percent        Percent       Numerator = snap_r… <chr [1]>     
#>  5 data_source_year             Metadata      End year of five-y… <chr [0]>     
#>  6 GEOID                        Metadata      A federally-issued… <chr [0]>     
#>  7 NAME                         Metadata      The name of the ge… <chr [0]>     
#>  8 area_land_sq_kilometer       Metadata      Land area of the g… <chr [0]>     
#>  9 area_water_sq_kilometer      Metadata      Water area of the … <chr [0]>     
#> 10 area_land_water_sq_kilometer Metadata      Combined land and … <chr [0]>     
#> 11 geometry                     Metadata      The spatial geomet… <chr [0]>     
#> # ℹ 6 more variables: numerator_subtract_vars <list>, denominator_vars <list>,
#> #   denominator_subtract_vars <list>, se_calculation_type <chr>,
#> #   aggregation_strategy <chr>, universe <chr>
```

The **definition** column is especially important: raw ACS variables
read `"This is a raw ACS estimate."`, while derived variables spell out
their inputs alongside the original ACS codes (e.g.,
`"Numerator = snap_received (B22003_002). Denominator = snap_universe (B22003_001)."`).
The codebook’s component list-columns (`numerator_vars`,
`denominator_vars`, and their `_subtract_` counterparts) drive the
pooled margin-of-error calculations, so they always reflect the actual
calculation.

### GEOIDs

The `GEOID` column contains FIPS codes whose structure depends on the
geography level:

| Geography | GEOID format | Example         | Components             |
|-----------|--------------|-----------------|------------------------|
| State     | 2 digits     | `"11"`          | State                  |
| County    | 5 digits     | `"11001"`       | State + County         |
| Tract     | 11 digits    | `"11001001002"` | State + County + Tract |
| Place     | 7 digits     | `"1150000"`     | State + Place          |

You can parse these to join with other datasets. For example,
`substr(GEOID, 1, 2)` extracts the state code from any sub-state
geography.

## Discovering Available Data

`urbnindicators` provides several functions for exploring what data is
available:

| Function | Purpose | Requires API key? |
|----|----|----|
| [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md) | Names of all registered tables | No\* |
| [`list_variables()`](https://ui-research.github.io/urbnindicators/reference/list_variables.md) | Every variable mapped to its table | Yes |
| [`get_acs_codebook()`](https://ui-research.github.io/urbnindicators/reference/get_acs_codebook.md) | Browse all ACS variables with clean names | Yes |

\*`list_tables(geography = "block group")` also requires a key, because
block-group availability is read from the ACS variable dictionary. The
Census Bureau’s variables metadata endpoint requires an API key, so any
function that loads the dictionary needs `CENSUS_API_KEY` set (see
`tidycensus::census_api_key("YOUR_KEY", install = TRUE)`).

A typical discovery workflow:

1.  **Start with
    [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)**
    to see the registered tables.
2.  **Use
    [`list_variables()`](https://ui-research.github.io/urbnindicators/reference/list_variables.md)
    to preview** what a specific table will produce:
    `list_variables() %>% filter(table == "snap")`.
3.  **If your topic isn’t covered**, search the full ACS catalog:
    `get_acs_codebook() %>% filter(str_detect(variable_clean, "poverty"))`.
4.  **Pass the table code directly** to
    [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md):
    `compile_acs_data(tables = "B17001", ...)`.

## Registered Table Reference

The table below lists all pre-built tables available via
[`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md).
