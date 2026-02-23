# How to Open a Pull Request

``` r
library(dplyr)
library(tidyr)
library(stringr)
library(urbnindicators)
library(tidycensus)
```

Pull requests (PRs) are a way to propose changes to the codebase of a
project. They allow you to suggest changes, improvements, or fixes to
the code, and they provide a platform for discussion and review before
those changes are merged into the main codebase.

For
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/),
a common use-case for a PR is to propose a new variable or series of
variables. This vignette will illustrate the complete PR process for a
new series of variables. The core of the PR is a single
`register_table()` call in `R/table_registry.R` that declares the raw
ACS variables and uses the `define_*` DSL to specify how derived
variables are computed. The codebook, coefficients of variation, and
variable listings are all generated automatically from this
registration.

## Overview

Adding a new table requires a single code change:

1.  **`R/table_registry.R`**: Add a `register_table()` call that
    declares (a) the raw ACS variable codes and (b) a `definitions` list
    that uses the `define_*` helpers to specify how derived variables
    (e.g., percentages) are calculated.

Everything else–the codebook, coefficients of variation, variable
listings, and the `tables`/`indicators` API–is generated automatically
from this registration. After writing the registration, the PR should
include quality checks to verify that the new variables are correctly
calculated and appropriately documented.

The quality-check steps are:

2.  **Codebook**: Verify that each new variable is documented in the
    codebook and that its documentation is accurate.
3.  **Coefficients of variation**: Verify that CVs appear reasonable.
4.  **Pretty names**: Verify that
    [`make_pretty_names()`](https://ui-research.github.io/urbnindicators/reference/make_pretty_names.md)
    produces reasonable labels.
5.  **Integration test**: Call
    [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
    end-to-end and inspect the results.

## Our Variable Series

We’re going to add a series of estimates that describe “Household Type”,
which are contained in table B11001. We can find the relevant variables
by navigating the codebook returned by
[`tidycensus::load_variables()`](https://walker-data.com/tidycensus/reference/load_variables.html).

``` r
codebook = load_variables(dataset = "acs5", year = 2023)

codebook %>%
  filter(str_detect(name, "B11001")) %>%
  head()
#> # A tibble: 6 × 4
#>   name        label                                            concept geography
#>   <chr>       <chr>                                            <chr>   <chr>    
#> 1 B11001A_001 Estimate!!Total:                                 Househ… block gr…
#> 2 B11001A_002 Estimate!!Total:!!Family households:             Househ… block gr…
#> 3 B11001A_003 Estimate!!Total:!!Family households:!!Married-c… Househ… block gr…
#> 4 B11001A_004 Estimate!!Total:!!Family households:!!Other fam… Househ… block gr…
#> 5 B11001A_005 Estimate!!Total:!!Family households:!!Other fam… Househ… block gr…
#> 6 B11001A_006 Estimate!!Total:!!Family households:!!Other fam… Househ… block gr…
```

## Step 1: Identify the Raw Variables

We can see how these variables will be renamed behind the scenes in
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
by providing the table name to
[`urbnindicators::select_variables_by_name()`](https://ui-research.github.io/urbnindicators/reference/select_variables_by_name.md).

``` r
select_variables_by_name("B11001_", census_codebook = codebook) %>%
  stats::setNames(names(.) %>% str_remove_all("including_living_alone_"))
#>                                                            household_type_universe_ 
#>                                                                        "B11001_001" 
#>                                                   household_type_family_households_ 
#>                                                                        "B11001_002" 
#>                             household_type_family_households_married_couple_family_ 
#>                                                                        "B11001_003" 
#>                                      household_type_family_households_other_family_ 
#>                                                                        "B11001_004" 
#>   household_type_family_households_other_family_male_householder_no_spouse_present_ 
#>                                                                        "B11001_005" 
#> household_type_family_households_other_family_female_householder_no_spouse_present_ 
#>                                                                        "B11001_006" 
#>                                                household_type_nonfamily_households_ 
#>                                                                        "B11001_007" 
#>                       household_type_nonfamily_households_householder_living_alone_ 
#>                                                                        "B11001_008" 
#>                   household_type_nonfamily_households_householder_not_living_alone_ 
#>                                                                        "B11001_009"
```

These are auto-named reasonably well, but we can make the names more
concise by removing the repeated substring `"including_living_alone_"`.
Note that each variable name ends in an `_`; this is intentional.

## Step 2: Choose Denominators for Derived Variables

Selecting an appropriate denominator for percentage variables is
critical, and at times, complex. The basic approach is to simply divide
every variable by the table universe, which should contain `universe` in
the variable name. But in other cases, alternate variables may make for
more insightful denominators. For example, if we were interested in the
share of family households that were headed by single male householders,
we would want to divide
`household_type_family_households_other_family_male_householder_no_spouse_present`
by `household_type_family_households_` rather than by
`household_type_universe_`.

At this point, we’ll want to obtain some sample data to verify that our
variable choices and denominator logic are correct:

``` r
sample_data = tidycensus::get_acs(
  years = 2022,
  geography = "county",
  state = "NJ",
  variables = select_variables_by_name("B11001_", census_codebook = codebook) %>%
    stats::setNames(names(.) %>% stringr::str_remove_all("including_living_alone_")),
  output = "wide") %>%
  select(GEOID, NAME, matches("_E")) %>%
  rename_with(cols = everything(), ~ str_remove_all(.x, "_E"))

sample_data %>%
  head()
#> # A tibble: 6 × 11
#>   GEOID NAME                       household_type_unive…¹ household_type_famil…²
#>   <chr> <chr>                                       <dbl>                  <dbl>
#> 1 34001 Atlantic County, New Jers…                 109557                  71330
#> 2 34003 Bergen County, New Jersey                  355127                 254633
#> 3 34005 Burlington County, New Je…                 177222                 120721
#> 4 34007 Camden County, New Jersey                  202540                 131748
#> 5 34009 Cape May County, New Jers…                  44884                  29154
#> 6 34011 Cumberland County, New Je…                  53781                  36601
#> # ℹ abbreviated names: ¹​household_type_universe,
#> #   ²​household_type_family_households
#> # ℹ 7 more variables:
#> #   household_type_family_households_married_couple_family <dbl>,
#> #   household_type_family_households_other_family <dbl>,
#> #   household_type_family_households_other_family_male_householder_no_spouse_present <dbl>,
#> #   household_type_family_households_other_family_female_householder_no_spouse_present <dbl>, …
```

We can test our percentage calculations on the sample data. Note the use
of
[`urbnindicators::safe_divide()`](https://ui-research.github.io/urbnindicators/reference/safe_divide.md),
which returns `0` rather than `NaN` when the denominator is zero.

``` r
sample_data %>%
  dplyr::transmute(
    dplyr::across(
      .cols = c(dplyr::matches("household_type"), -dplyr::matches("universe")),
      .fns = ~ safe_divide(.x, household_type_universe),
      .names = "{.col}_percent")) %>%
  head() %>% select(1:3)
#> # A tibble: 6 × 3
#>   household_type_family_househol…¹ household_type_famil…² household_type_famil…³
#>                              <dbl>                  <dbl>                  <dbl>
#> 1                            0.651                  0.448                  0.203
#> 2                            0.717                  0.555                  0.162
#> 3                            0.681                  0.521                  0.160
#> 4                            0.650                  0.435                  0.215
#> 5                            0.650                  0.516                  0.134
#> 6                            0.681                  0.442                  0.239
#> # ℹ abbreviated names: ¹​household_type_family_households_percent,
#> #   ²​household_type_family_households_married_couple_family_percent,
#> #   ³​household_type_family_households_other_family_percent
```

## Step 3: Write the `register_table()` Call

Once we’re satisfied with the raw variables and derived logic, we
express it as a `register_table()` call. This is the primary code change
in the PR.

The `definitions` list uses the `define_*` helpers to declaratively
specify how each derived variable is computed. The package uses this
specification to both execute the computation and auto-generate the
codebook documentation and CV calculations.

### The `define_*` DSL

| Helper                                                                                                       | Use case                                          | Key arguments                                                                                                             |
|--------------------------------------------------------------------------------------------------------------|---------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------|
| [`define_percent()`](https://ui-research.github.io/urbnindicators/reference/define_percent.md)               | Single output: numerator / denominator            | `output`, `numerator`, `denominator` (simple) or `numerator_variables`, `denominator_variables`, regex variants (complex) |
| [`define_across_percent()`](https://ui-research.github.io/urbnindicators/reference/define_across_percent.md) | Percentages across a regex-matched set of columns | `input_regex`, `output_suffix`, `denominator` or `denominator_function`, optional `exclude_regex`                         |
| [`define_across_sum()`](https://ui-research.github.io/urbnindicators/reference/define_across_sum.md)         | Summing paired columns (e.g., male + female)      | `input_regex`, `addend_function`, `output_naming_function`                                                                |
| [`define_one_minus()`](https://ui-research.github.io/urbnindicators/reference/define_one_minus.md)           | Complement of an existing percentage (1 - x)      | `output`, `source_variable`                                                                                               |
| [`define_metadata()`](https://ui-research.github.io/urbnindicators/reference/define_metadata.md)             | Non-computed variables (e.g., medians)            | `output`, `definition_text`                                                                                               |

### Household type registration

For our household type example, the `register_table()` call is:

``` r
register_table(list(
  name = "household_type",
  description = "Household type (including living alone)",
  acs_tables = "B11001",
  depends_on = character(0),
  raw_variable_source = list(type = "select_variables"),
  raw_variables_transform = function(vars) {
    stats::setNames(vars, names(vars) %>%
      stringr::str_remove_all("including_living_alone_"))
  },
  definitions = list(
    define_across_percent(
      input_regex = "^household_type",
      exclude_regex = "universe|percent",
      output_suffix = "_percent",
      denominator = "household_type_universe"))
))
```

This single block replaces what previously required separate changes
across three files (`R/list_acs_variables.R`, `R/compile_acs_data.R`,
and manual codebook verification).

### More examples

**Simple percentage** (one numerator, one denominator):

``` r
## from the SNAP table
definitions = list(
  define_percent("snap_received_percent",
                 numerator = "snap_received",
                 denominator = "snap_universe"))
```

**Across-percent with a complement** (percentages for all race
categories, plus a person-of-color complement):

``` r
## from the race table
definitions = list(
  define_across_percent(
    input_regex = "^race_nonhispanic|^race_hispanic",
    exclude_regex = NULL,
    output_suffix = "_percent",
    denominator = "race_universe"),
  define_one_minus("race_personofcolor_percent",
                   source_variable = "race_nonhispanic_white_alone_percent"))
```

**Across-sum followed by across-percent** (sum male + female counts into
combined age variables, then calculate percentages):

``` r
## from the sex_by_age table (age construct)
definitions = list(
  define_across_sum(
    input_regex = "sex_by_age_female_.*years($|_over$)",
    addend_function = function(column) {
      column %>% stringr::str_replace("female", "male")
    },
    output_naming_function = function(column) {
      column %>% stringr::str_replace("sex_by_age_female_", "age_")
    }),
  define_across_percent(
    input_regex = "^age.*years($|_over$)",
    output_suffix = "_percent",
    denominator = "sex_by_age_universe"))
```

### Placement in the file

The `register_table()` call should be added to `R/table_registry.R`
under the appropriate comment header. For our example, this would be
under `####----TABLE REGISTRATIONS: HOUSEHOLD COMPOSITION----####`.

If any new column names are created by `compute_fn` logic, add them to
the
[`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
call at the bottom of `R/table_registry.R` to avoid R CMD check notes.

## Step 4: Verify the Codebook

After writing the registration, load the package with
`devtools::load_all()` and run a test call. New variables should be
automatically documented by
[`generate_codebook()`](https://ui-research.github.io/urbnindicators/reference/generate_codebook.md)
and included in the codebook attribute of the dataframe returned by
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
Verify that each new variable is documented and that its documentation
is accurate. If the documentation is incorrect, the error we estimate
for derived variables will also be incorrect.

If the documentation is incorrect, the PR should note the issue; it
should not include changes to `R/generate_codebook.R`.

## Step 5: Verify Coefficients of Variation

Like the codebook, CVs are computed automatically from the `definitions`
metadata. Check that coefficients of variation appear reasonable.

Users should also check the magnitude of errors for all variables–raw
ACS estimates and `urbnindicators`-calculated variables alike–across
smaller geographies, such as all tracts in one or more states. If CVs
are large (e.g., over 50) for a large share of all tracts, this may
indicate that the series of interest is not appropriate for tract-level
analysis. Because `urbnindicators` is designed to facilitate tract-level
analysis, variables that are consistently unreliable at the tract level
will not be integrated into the codebase.

## Step 6: Verify Pretty Names

Ensure that the new variables have reasonable names:

``` r
sample_data %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(dplyr::matches("household_type"), -dplyr::matches("universe")),
      .fns = ~ .x / household_type_universe,
      .names = "{.col}_percent")) %>%
  urbnindicators::make_pretty_names() %>%
  colnames()
#>  [1] "Geoid"                                                                                 
#>  [2] "Name"                                                                                  
#>  [3] "Household Type (Universe)"                                                             
#>  [4] "Household Type Family Households"                                                      
#>  [5] "Household Type Family Households Married Couple Family"                                
#>  [6] "Household Type Family Households Other Family"                                         
#>  [7] "Household Type Family Households Other Family Male Householder No Spouse Present"      
#>  [8] "Household Type Family Households Other Family Female Householder No Spouse Present"    
#>  [9] "Household Type Nonfamily Households"                                                   
#> [10] "Household Type Nonfamily Households Householder Living Alone"                          
#> [11] "Household Type Nonfamily Households Householder Not Living Alone"                      
#> [12] "Household Type Family Households (%)"                                                  
#> [13] "Household Type Family Households Married Couple Family (%)"                            
#> [14] "Household Type Family Households Other Family (%)"                                     
#> [15] "Household Type Family Households Other Family Male Householder No Spouse Present (%)"  
#> [16] "Household Type Family Households Other Family Female Householder No Spouse Present (%)"
#> [17] "Household Type Nonfamily Households (%)"                                               
#> [18] "Household Type Nonfamily Households Householder Living Alone (%)"                      
#> [19] "Household Type Nonfamily Households Householder Not Living Alone (%)"
```

Though these names are far from perfect, they’re reasonably concise and
descriptive, and there are no problematic words that are lost in the
pretty-ifying process (e.g., acronyms, series of numbers, etc.). We can
leave it to users to make other adjustments, such as removing the
substring “Household Type”, if they want even more concise names.

## Step 7: Integration Test

Integrate the proposed changes into the codebase (on a branch), load the
current version of `urbnindicators` (via `devtools::load_all()`), and
call
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
Verify that the new table works both in isolation and as part of the
full suite:

``` r
## test the new table in isolation
compile_acs_data(
  tables = "household_type",
  years = 2022,
  geography = "county",
  states = "NJ")

## test the full suite
compile_acs_data(
  years = 2022,
  geography = "county",
  states = "NJ")
```

Interactively explore both the data and the codebook (accessed via
`attr(result, "codebook")`).

## Step 8: Quality Check Results

There are a few strategies for quality-checking the results of a series
of variables:

1.  Compare to a published benchmark value. This works well for derived
    variables, such as percentages, that are reported by the Census
    Bureau in the Subject Tables (tables prefixed with an `S`) but that
    are not directly available via the detailed tables (prefixed with a
    `B` or `C`). However, for this series of variables, there are not
    counterpart percentages reported as part of the subject tables. (An
    example of one of multiple reasons that `urbnindicators` exclusively
    uses data from the detailed tables.)

2.  Manually compute a benchmark value. This works well for all those
    derived variables that aren’t reported in any Census Bureau product.
    Identify the relevant numerator and denominator variables (in the
    case of a derived percentage) and manually calculate the derived
    variable, then compare the manually-computed benchmark to the
    programmatically-calculated version. This seems very simple for our
    example here, where each derived variable is a given variable
    divided by the table universe, but with more complex variables–e.g.,
    where a numerator is a summed variable itself–this is a very useful
    quality check.

3.  Plot a histogram of the computed variable(s) (if multiple variables
    in a series, use
    [`pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
    to turn the dataframe long, then use `facet_wrap()` to plot each
    histogram side-by-side. Check for unexpected spikes and outlier
    values.

4.  Check for missingness. Generally, derived variables should have low
    or no missingness, so any substantial number of missing observations
    may be an indication that a calculation has gone awry.

## Step 9: Open the PR

Once users are satisfied with the proposed code (and/or have noted any
issues), they should click on their branch in the GitHub repository and
click `New pull request`.
