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
list_tables()
```

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
#> [19] "population_density"           "poverty"                     
#> [21] "public_assistance"            "race"                        
#> [23] "school_enrollment"            "sex"                         
#> [25] "snap"                         "tenure"                      
#> [27] "tenure_by_housing_costs"      "tenure_by_units_in_structure"
#> [29] "total_population"             "transportation_to_work"      
#> [31] "travel_time_to_work"          "units_in_structure"          
#> [33] "vehicles_available"           "year_structure_built"        
```
