# urbnindicators

``` r
library(urbnindicators)
library(tidycensus)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(urbnthemes) # install from https://github.com/UrbanInstitute/urbnthemes
```

This vignette is organized into three parts:

1.  We illustrate a typical workflow for obtaining American Community
    Survey (ACS) data using
    [`library(tidycensus)`](https://walker-data.com/tidycensus/).

2.  We show how
    [`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
    can produce similar outputs, but with much less code and effort.

3.  We highlight how
    [`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
    provides metadata and improves the quality and reliability of the
    analysis process.

## A Typical Workflow

[**tidycensus**](https://walker-data.com/tidycensus/index.html) provides
a suite of functions for working with ACS data in R. While it’s
versatile and comprehensive, it can require a significant amount of
knowledge and time to support a robust analysis, leading many users to
fall into common pitfalls without realizing they’ve made an error(s).

### Identify variables to query

We load the built-in codebook and search for our construct of interest
(disability). This leaves us 500 variables to choose from.

``` r
acs_codebook = load_variables(dataset = "acs5", year = 2022)

acs_codebook %>%
  filter(str_detect(concept, "Disability")) %>% 
  select(name, label, concept) %>%
  ## only printing three, for brevity
  head(3) %>%
  reactable::reactable()
```

Let’s imagine we’re interested in calculating the share of individuals
with a disability; this requires only two variables (conceptually): the
number of people with a disability and the number of all people.

So which variable(s) do we select? There’s not a clear answer. All
variables relating to disability reflect disability and at least one
other characteristic (e.g., “sex by age by disability status”). If we
want to calculate the percent of all individuals with a disability, we
want to do so using the most robust available variables (i.e., those
that reflect all individuals who reported their disability status),
whereas some variables that reflect disability may have smaller counts
because the other characteristics combined with disability status (e.g.,
health insurance coverage status) may be available only for a subset of
the individuals for whom disability status is available.

Let’s imagine we select the table of variables prefixed “B18101”, for
“Sex by Age by Disability”. We think that most respondents who respond
about their disability status will also have responded about their sex
and age. We then pass this to
[`library(tidycensus)`](https://walker-data.com/tidycensus/) as:

``` r
df_disability = get_acs(
  geography = "county",
  state = "NJ", 
  year = 2022,
  output = "wide",
  survey = "acs5",
  table = "B18101")
```

This returns us 21 observations–one for each county in NJ–along with an
intimidating 80 columns with unintelligble names along the lines of
`B18101_039E`.

### Calculating our measure of interest

Now we would need to figure out how to aggregate the needed variables
for both the denominator and numerator in order to calculate a valid “%
Disabled” measure, a task that is feasible but time-intensive and
error-prone.

For an analysis that leverages more than a single measure, and
especially when measures are required from distinct tables, this
workflow is burdensome and creates significant surface area for
undetected errors.

At the same time, many analysts will be overwhelmed by and unsure how to
combine the margins of error that are returned by
[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html)
to calculate pooled errors for the new percent-disabled variable, opting
simply to drop this critical information from their analysis. (See
[Quantifying Survey
Error](https://ui-research.github.io/urbnindicators/articles/quantified-survey-error.md)
to learn more about how
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
helps simplify this task.)

## Using urbnindicators

[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
abstracts the workflow above behind the scenes. Instead of a call to
[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html),
a call to
[`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
returns a dataset of both raw ACS measures and derived estimates (such
as the share of all individuals who are disabled).

### Acquire data

It’s as simple as the call below. Note that you can provide a vector of
years and/or states if you want data over different time periods or
geographies.

Note that selecting more tables or more geographic units–either by
selecting a `geography` option comprising more units, by selecting more
states, or selecting more years–can significantly increase the query
time.

Use
[`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)
to see some of the most commonly-used tables:

``` r
list_tables() |> head(10)
#>  [1] "age"                    "computing_devices"      "cost_burden"           
#>  [4] "disability"             "educational_attainment" "employment"            
#>  [7] "gini"                   "health_insurance"       "household_size"        
#> [10] "income_quintiles"
```

Or use
[`get_acs_codebook()`](https://ui-research.github.io/urbnindicators/reference/get_acs_codebook.md)
to see every table supported by the Census Bureau API:

``` r
get_acs_codebook() |>
  filter(str_detect(variable_clean, "snap")) |>
  head(10)
#> # A tibble: 10 × 3
#>    table  variable_raw variable_clean                                           
#>    <chr>  <chr>        <chr>                                                    
#>  1 B09010 B09010_001   receipt_supplemental_security_income_ssi_cash_public_ass…
#>  2 B09010 B09010_002   receipt_supplemental_security_income_ssi_cash_public_ass…
#>  3 B09010 B09010_003   receipt_supplemental_security_income_ssi_cash_public_ass…
#>  4 B09010 B09010_004   receipt_supplemental_security_income_ssi_cash_public_ass…
#>  5 B09010 B09010_005   receipt_supplemental_security_income_ssi_cash_public_ass…
#>  6 B09010 B09010_006   receipt_supplemental_security_income_ssi_cash_public_ass…
#>  7 B09010 B09010_007   receipt_supplemental_security_income_ssi_cash_public_ass…
#>  8 B09010 B09010_008   receipt_supplemental_security_income_ssi_cash_public_ass…
#>  9 B09010 B09010_009   receipt_supplemental_security_income_ssi_cash_public_ass…
#> 10 B09010 B09010_010   receipt_supplemental_security_income_ssi_cash_public_ass…
```

Here we request just two tables–`disability` and
`transportation_to_work`.

``` r
df_urbnindicators = compile_acs_data(
  years = 2024,
  tables = c("disability", "transportation_to_work"),
  geography = "county",
  states = "NJ",
  spatial = TRUE)
```

You can also pass vectors of years and/or states to pull data across
multiple time periods or geographies in a single call:

``` r
df_multi = compile_acs_data(
  years = c(2019, 2024),
  tables = "disability",
  geography = "county",
  states = c("NJ", "NY"))

df_multi %>%
  count(data_source_year)
#> # A tibble: 2 × 2
#>   data_source_year     n
#>              <dbl> <int>
#> 1             2019    83
#> 2             2024    83
```

Alternately, you can pass the name of a variable or table from
[`get_acs_codebook()`](https://ui-research.github.io/urbnindicators/reference/get_acs_codebook.md)
to
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
The equivalent of the first call above would be:

``` r
df_urbnindicators = compile_acs_data(
  years = 2024,
  tables = c("sex_by_age_by_disability_status_universe", "B08301"),
  geography = "county",
  states = "NJ",
  spatial = TRUE)
```

### Analyze or visualize data

And now we’re ready to analyze or plot our data. Simplistically:

``` r
df_urbnindicators %>%
  ggplot() +
    geom_sf(aes(fill = disability_percent)) +
    theme_urbn_map() +
    scale_fill_continuous(labels = scales::percent, transform = "reverse") +
    labs(
      title = "Disability Rates Appear Higher in Southern NJ",
      subtitle = "Disability rates by county, NJ, 2020-2024 ACS",
      fill = "Population with an ACS-defined disability (%)" %>% str_wrap(20))
```

![](urbnindicators_files/figure-html/unnamed-chunk-9-1.png)

### Document data

There’s a lot happening behind the scenes, so it’s important to
understand what each variable represents and how it was calculated.
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
includes a codebook as an attribute of the dataframe returned from
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
View and navigate through the [full codebook
here](https://ui-research.github.io/urbnindicators/articles/codebook.md).

Access the codebook with [`attr()`](https://rdrr.io/r/base/attr.html):

``` r
codebook = attr(df_urbnindicators, "codebook")

codebook %>%
  head(5)
#> # A tibble: 5 × 9
#>   calculated_variable                    variable_type definition numerator_vars
#>   <chr>                                  <chr>         <chr>      <list>        
#> 1 total_population_universe              Count         This is a… <chr [0]>     
#> 2 means_transportation_work_universe     Count         This is a… <chr [0]>     
#> 3 means_transportation_work_car_truck_v… Count         This is a… <chr [0]>     
#> 4 means_transportation_work_car_truck_v… Count         This is a… <chr [0]>     
#> 5 means_transportation_work_car_truck_v… Count         This is a… <chr [0]>     
#> # ℹ 5 more variables: numerator_subtract_vars <list>, denominator_vars <list>,
#> #   denominator_subtract_vars <list>, se_calculation_type <chr>,
#> #   aggregation_strategy <chr>
```

The codebook has three columns:

- **calculated_variable** – the variable name as it appears in the
  dataframe.
- **variable_type** – whether the variable is a `count` (raw ACS
  estimate), a `percent` (derived ratio), or `metadata` (e.g., a median
  or geographic identifier).
- **definition** – a formula showing how the variable was calculated.
  For raw ACS variables, this is the original Census Bureau variable
  code (e.g., `B22003_002`). For derived variables, this is an
  expression like `snap_received / snap_universe`.

These definition strings are also used internally to calculate margins
of error for derived variables (see [Quantifying Survey
Error](https://ui-research.github.io/urbnindicators/articles/quantified-survey-error.md)),
so their accuracy is critical.

Some definitions are quite complex. For example, `disability_percent` is
the sum of all of the sex-by-age groupings for people with disabilities
(numerator) divided by the table universe:

``` r
codebook %>%
  filter(calculated_variable == "disability_percent") %>%
  pull(definition)
#> [1] "Numerator = sex_by_age_by_disability_status_male_under_5_years_with_a_disability (B18101_004), sex_by_age_by_disability_status_male_5_17_years_with_a_disability (B18101_007), sex_by_age_by_disability_status_male_18_34_years_with_a_disability (B18101_010), sex_by_age_by_disability_status_male_35_64_years_with_a_disability (B18101_013), sex_by_age_by_disability_status_male_65_74_years_with_a_disability (B18101_016), sex_by_age_by_disability_status_male_75_years_over_with_a_disability (B18101_019), sex_by_age_by_disability_status_female_under_5_years_with_a_disability (B18101_023), sex_by_age_by_disability_status_female_5_17_years_with_a_disability (B18101_026), sex_by_age_by_disability_status_female_18_34_years_with_a_disability (B18101_029), sex_by_age_by_disability_status_female_35_64_years_with_a_disability (B18101_032), sex_by_age_by_disability_status_female_65_74_years_with_a_disability (B18101_035), sex_by_age_by_disability_status_female_75_years_over_with_a_disability (B18101_038). Denominator = sex_by_age_by_disability_status_universe (B18101_001)."
```

### Create your own derived variables

For tables from
[`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md),
raw ACS variables and derived variables are automatically returned. But
for other tables, there are no pre-computed (by `urbnindicators`)
derived variables. And even for tables reflected in
[`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md),
you may want alternate or additional derived variables. `urbnindicators`
provides a suite of helper functions (`define_*()`) that allow you to
specify how you want to create these derived variables; these helper
functions abstract away the actual calculations and ensure that you get
an updated codeboook and correctly-pooled margins of error for each of
your newly-derived variables. See [Custom Derived
Variables](https://ui-research.github.io/urbnindicators/articles/custom-derived-variables.md)
for more.

### Interpolate data to custom geographies

ACS data are available for many statistical and political geographies,
but many analyses rfocus on other geographies like neighborhoods or
planning districts.
[`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md)
translates data from ACS-supported geographies to any user-defined
geography, properly re-deriving percentages and propagating margins of
error. See [Translating ACS Data to Custom
Geographies](https://ui-research.github.io/urbnindicators/articles/custom-geographies.md)
for a worked example.
