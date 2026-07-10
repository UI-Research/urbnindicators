# Custom Derived Variables

``` r

library(dplyr)
library(tidyr)
library(stringr)
library(urbnindicators)
library(tidycensus)
```

[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
supports a large set of pre-built derived variables (percentages,
complements, etc.). But you can also define your own derived variables
using the `define_*()` helpers and then pass these definitions directly
to
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
via the `tables` parameter. Your custom variables automatically get
codebook entries and margins of error, just like the built-in ones.

This vignette walks through the process of identifying variables,
choosing denominators, and writing definitions, using sex-by-age data
for the White-alone population (ACS table B01001A) as a running example.

## The `define_*()` Helpers

| Helper | Use case | Key arguments |
|----|----|----|
| [`define_percent()`](https://ui-research.github.io/urbnindicators/reference/define_percent.md) | Single output: numerator / denominator | `numerator`, `denominator`, `output`, optional `subtract_from_numerator`, `subtract_from_denominator`, `exclude` |
| `define_percent(..., each = TRUE)` | Percentages across a regex-matched set of columns (one output per match, named `<matched_column>_percent`) | `numerator` (regex), `denominator` or `denominator_replace`, `exclude` |
| [`define_sum()`](https://ui-research.github.io/urbnindicators/reference/define_sum.md) | Sum one or more columns into a single output | `columns`, `output` |
| `define_sum(..., each = TRUE)` | Summing paired columns (e.g., male + female) for each match | `columns` (regex), `add_replace`, `output_replace`, `exclude` |
| [`define_complement()`](https://ui-research.github.io/urbnindicators/reference/define_complement.md) | Complement of an existing percentage (1 - x) | `source`, `output` |
| [`define_metadata()`](https://ui-research.github.io/urbnindicators/reference/define_metadata.md) | Non-computed variables (e.g., medians) | `output`, `definition` |

Definitions are passed to
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
alongside table names in a [`list()`](https://rdrr.io/r/base/list.html).
They operate on the columns produced by the tables you request, so make
sure to include the table(s) whose variables your definitions reference.

## Step 1: Identify the Raw Variables

We’re going to create a variable describing White-alone men aged 18 to
44 as a share of the total White-alone population (the table universe).
The needed raw variables are from ACS table B01001A, as described in the
codebook.

``` r

codebook = get_acs_codebook()

codebook %>%
  filter(table == "B01001A") %>%
  pull(variable_clean) %>%
  head()
#> [1] "sex_by_age_white_alone_universe"          
#> [2] "sex_by_age_white_alone_male"              
#> [3] "sex_by_age_white_alone_male_under_5_years"
#> [4] "sex_by_age_white_alone_male_5_9_years"    
#> [5] "sex_by_age_white_alone_male_10_14_years"  
#> [6] "sex_by_age_white_alone_male_15_17_years"
```

## Step 2: Choose Denominators

Selecting an appropriate denominator for percentage variables is
critical, and at times, complex. The basic approach is to simply divide
every variable by the table universe, which should contain `universe` in
the variable name. But in other cases, alternate variables may make for
more insightful denominators. For simplicity, we just use the table
universe here.

## Step 3: Write and Use Definitions

We express the new variable(s) using `define_*()` helpers and pass the
definitions directly to
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).

Our target variable requires summing several male age-group columns (in
B01001A these are collapsed brackets: 18-19, 20-24, 25-29, 30-34, and
35-44) and dividing by the table universe.
[`define_percent()`](https://ui-research.github.io/urbnindicators/reference/define_percent.md)
handles this: when its `numerator` is a regex, it matches all columns
fitting the pattern and sums them into the numerator.

The auto-processed column names from B01001A follow the pattern
`sex_by_age_white_alone_male_{age_range}_years`, so we can target the
18-44 age groups with a regex that matches the leading digits of each
range:

``` r

df = compile_acs_data(
  tables = list(
    "B01001A",
    define_percent(
      numerator = "sex_by_age_white_alone_male_(1[89]|2[0-9]|3[0-9]|4[0-4])",
      denominator = "sex_by_age_white_alone_universe",
      output = "white_male_18_44_percent",
      exclude = "percent")),
  years = 2024,
  geography = "county",
  states = "NJ")

df %>%
  select(GEOID, white_male_18_44_percent) %>%
  head()
#> # A tibble: 6 × 2
#>   GEOID white_male_18_44_percent
#>   <chr>                    <dbl>
#> 1 34001                    0.149
#> 2 34003                    0.150
#> 3 34005                    0.162
#> 4 34007                    0.164
#> 5 34009                    0.124
#> 6 34011                    0.160
```

The definition is executed after the raw ACS data are fetched and
renamed, and the results appear in the codebook with automatically
calculated margins of error:

``` r

attr(df, "codebook") %>%
  filter(calculated_variable == "white_male_18_44_percent") %>%
  pull(definition)
#> [1] "Numerator = sex_by_age_white_alone_male_18_19_years (B01001A_007), sex_by_age_white_alone_male_20_24_years (B01001A_008), sex_by_age_white_alone_male_25_29_years (B01001A_009), sex_by_age_white_alone_male_30_34_years (B01001A_010), sex_by_age_white_alone_male_35_44_years (B01001A_011). Denominator = sex_by_age_white_alone_universe (B01001A_001)."
```

## More Examples

### Simple percentage

One numerator, one denominator:

``` r

df_snap = compile_acs_data(
  tables = list(
    "snap",
    define_percent(
      "snap_received_percent_manual",
      numerator = "snap_received",
      denominator = "snap_universe")),
  years = 2024,
  geography = "county",
  states = "NJ")

## verify it matches the built-in version
df_snap %>%
  transmute(
    builtin = snap_received_percent,
    manual = snap_received_percent_manual,
    match = abs(builtin - manual) < 1e-10) %>%
  filter(!match)
#> # A tibble: 0 × 3
#> # ℹ 3 variables: builtin <dbl>, manual <dbl>, match <lgl>
```

### Batch percentages with a complement

Percentages for all race categories, plus a person-of-color complement.
This is how the built-in `race` table works internally. Passing
`each = TRUE` switches
[`define_percent()`](https://ui-research.github.io/urbnindicators/reference/define_percent.md)
into batch mode: the `numerator` is a regex, and one percentage is
produced per matched column (each named `<matched_column>_percent`).

``` r

compile_acs_data(
  tables = list(
    "race",
    define_percent(
      "^race_nonhispanic|^race_hispanic",
      denominator = "race_universe",
      each = TRUE),
    ## race_personofcolor_percent is the share of all individuals who are not
    ## non-Hispanic, White alone, i.e., the complement
    define_complement(
      source = "race_nonhispanic_white_alone_percent",
      output = "race_personofcolor_percent")),
  years = 2024,
  geography = "county",
  states = "DC")
```

### Batch sum followed by batch percent

Sum male + female counts into combined age variables, then calculate
percentages. This is how the built-in `sex_by_age` table works
internally.

This one is a bit tricky–the source table includes variables for each
age group, split by sex. To get age groups, we have to add the two
sex-specific estimates for the given age group. We pass a regex to
`define_sum(..., each = TRUE)` that selects all female-specific age
variables. `add_replace` programmatically identifies the same-named,
male-specific variables via string substitution. `output_replace`
simplifies the resulting combined variable name, removing the sex
category and other extraneous words.

``` r

compile_acs_data(
  tables = list(
    "sex_by_age",
    define_sum(
      "sex_by_age_female_.*years($|_over$)",
      each = TRUE,
      add_replace = c("female" = "male"),
      output_replace = c("sex_by_age_female_" = "age_")),
    define_percent(
      "^age.*years($|_over$)",
      denominator = "sex_by_age_universe",
      each = TRUE)),
  years = 2024,
  geography = "county",
  states = "DC")
```

### Complex percentage with subtraction

For cases where the numerator or denominator requires summing or
subtracting multiple variables, pass character vectors to `numerator` /
`denominator` and use the `subtract_from_numerator` /
`subtract_from_denominator` arguments:

``` r

df_complex = compile_acs_data(
  tables = list(
    "snap",
    define_percent(
      numerator = "snap_universe",
      denominator = "snap_universe",
      subtract_from_numerator = "snap_received",
      output = "snap_not_received_pct_complex")),
  years = 2024,
  geography = "county",
  states = "DC")

df_complex %>%
  mutate(sums_to_one = snap_received_percent + snap_not_received_pct_complex) %>%
  glimpse()
#> Rows: 1
#> Columns: 14
#> $ data_source_year                <dbl> 2024
#> $ GEOID                           <chr> "11001"
#> $ NAME                            <chr> "District of Columbia, District of Col…
#> $ total_population_universe       <dbl> 681294
#> $ snap_universe                   <dbl> 324491
#> $ snap_received                   <dbl> 46408
#> $ snap_received_percent           <dbl> 0.143
#> $ snap_not_received_pct_complex   <dbl> 0.857
#> $ total_population_universe_M     <dbl> 0
#> $ snap_universe_M                 <dbl> 1816
#> $ snap_received_M                 <dbl> 2088
#> $ snap_received_percent_M         <dbl> 0.0064
#> $ snap_not_received_pct_complex_M <dbl> 0.0071
#> $ sums_to_one                     <dbl> 1
```

## Verify Results

A few strategies for quality-checking custom derived variables:

1.  **Compare to a published benchmark.** Percentages reported by the
    Census Bureau in the Subject Tables (tables prefixed with `S`) can
    serve as reference values for derived variables computed from the
    detailed tables (prefixed with `B` or `C`).

2.  **Manually compute a benchmark.** Identify the relevant numerator
    and denominator variables and manually calculate the derived
    variable, then compare. This is especially useful for complex
    definitions where the numerator is itself a sum.

3.  **Plot histograms.** Use
    [`pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
    and `facet_wrap()` to check for unexpected spikes or outlier values
    across a series of related variables.

4.  **Check for missingness.** Derived variables should generally have
    low or no missingness; substantial missingness may indicate a
    calculation error.

5.  **Inspect the codebook.** Verify that `attr(df, "codebook")`
    accurately documents your custom variable’s definition. The
    definition string drives the margin of error calculation, so errors
    there will propagate to MOEs.
