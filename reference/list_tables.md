# List available table names

Returns the names of all registered ACS tables that can be requested via
the `tables` parameter of
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
Multi-construct tables (e.g., `sex_by_age`) are reported as their
individual constructs (e.g., `"age"` and `"sex"`). Note: only
pre-registered tables are listed here. Any valid ACS table code (e.g.,
`"B25070"`) can also be passed to `compile_acs_data(tables = ...)` and
will be auto-processed.

## Usage

``` r
list_tables(geography = NULL, year = latest_acs_year())
```

## Arguments

- geography:

  Optional geography type. When `"block group"`, only the tables
  published at the block-group level are returned (the ACS tabulates a
  limited subset of tables at this geography). When `NULL` (default),
  all registered tables are returned.

- year:

  The ACS year used to determine block-group availability when
  `geography = "block group"` (default 2022). Ignored otherwise.

## Value

A character vector of table names.

## Examples

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
if (FALSE) { # \dontrun{
list_tables(geography = "block group")
} # }
```
