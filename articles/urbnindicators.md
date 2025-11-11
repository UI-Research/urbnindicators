# urbnindicators

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
a suite of functions for working with ACS data in R. In fact,
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
is built on top of
[`library(tidycensus)`](https://walker-data.com/tidycensus/), and we
highly encourage those who are unfamiliar to explore
[`library(tidycensus)`](https://walker-data.com/tidycensus/) before
using
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/).

While `tidycensus` is versatile and allows users to access many more
datasets (and variables within those datasets) than does
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/),
it can require a significant amount of knowledge and time to support a
robust analysis, leading many users to fall into common pitfalls without
realizing they’ve made an error(s).

### Identify variables to query

We load the built-in codebook and search for our construct of interest
(disability). This leaves us 500 variables to choose from.

``` r
acs_codebook = tidycensus::load_variables(dataset = "acs5", year = 2022)

acs_codebook %>%
  dplyr::filter(stringr::str_detect(concept, "Disability")) %>% 
  head() %>%
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

Putting these challenges aside, let’s imagine we select the table of
variables prefixed “B18101”, for “Sex by Age by Disability”. We think
that most respondents who respond about their disability status will
also have responded about their sex and age. We then pass this to
[`library(tidycensus)`](https://walker-data.com/tidycensus/) as:

``` r
df_disability = get_acs(
  geography = "county",
  state = "NJ", 
  year = 2022,
  output = "wide",
  survey = "acs5",
  table = "B18101")

df_disability %>% dim()
#> [1] 21 80
df_disability %>% colnames() %>% head(5)
#> [1] "GEOID"       "NAME"        "B18101_001E" "B18101_001M" "B18101_002E"
```

This returns us 21 observations–one for each county in NJ–along with an
intimidating 80 columns with meaningless names along the lines of
`B18101_039E`.

### Calculating our measure of interest

Now we would need to figure out how to aggregate the needed variables
for both the denominator and numerator in order to calculate a valid “%
Disabled” measure, a task that is feasible but time-intensive and
error-prone.

For an analysis that leverages more than a single measure, and
especially when measures are required from distinct tables, this
workflow is burdensome and creates significant surface area for
undetected errors. To acquire data for multiple years, or for multiple
different types of geographies, also requires repeated calls to
[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html).

At the same time, many analysts will be overwhelmed by and unsure how to
combine the margins of error that are returned by
[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html),
opting simply to drop this critical information from their analysis as
they go about calculating “% Disabled”.

## Using urbnindicators

[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
abstracts the workflow above behind the scenes. Instead of a call to
[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html),
a call to
[`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
returns a dataset of both raw ACS measures and derived estimates (such
as the share of all individuals who are disabled). And that dataset
includes a range of measures–-spanning things such as health insurance,
employment, housing costs, and race and ethnicity–-not just one variable
or table from the ACS.

### Acquire data

It’s as simple as the call below. Note that you can provide a vector of
years and/or states if you want data over different time periods or
geographies.

Note that selecting more geographic units–either by selecting a
`geography` option comprising more units, by selecting more states, or
selecting more years–can significantly increase the query time. A
tract-level query of the entire US can take 30+ minutes.

``` r
df_urbnindicators = urbnindicators::compile_acs_data(
  years = 2022,
  geography = "county",
  states = "NJ",
  spatial = TRUE)
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  21%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |==============================                                        |  42%  |                                                                              |========================================                              |  56%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  75%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  92%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
```

### Analyze or visualize data

And now we’re ready to analyze or plot our data. Simplistically:

``` r
df_urbnindicators %>%
  select(matches("disability.*percent$"), geometry) %>%
  ggplot() +
    geom_sf(aes(fill = disability_percent)) +
    theme_urbn_map() +
    scale_fill_continuous(labels = scales::percent, trans = "reverse") +
    labs(
      title = "Disability rates appear higher in southern NJ",
      subtitle = "Disability rates by county, NJ, 2018-2022 ACS",
      fill = "Population with an ACS-defined disability (%)" %>% str_wrap(20))
```

![](urbnindicators_files/figure-html/unnamed-chunk-5-1.png)

### Document data

There’s a lot happening behind the scenes, so it’s important to
understand what each variable represents and how it was calculated.
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
includes a codebook as an attribute of the dataframe returned from
[`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
View and navigate through the [full codebook
here](https://ui-research.github.io/urbnindicators/articles/articles/codebook.md).

``` r
df_urbnindicators %>%
  attr("codebook") %>%
  head(20) %>%
  reactable::reactable()
```

The codebook specifies the variable type and provides a definition of
how the variable was calculated. Most (though not all) variables that
are directly available from the ACS are count variables, such as the
number of people receiving public assistance. Many of the variables that
are calculated by
[`library(urbnindicators)`](https://ui-research.github.io/urbnindicators/)
are percent variables, where we divide two count variables.

``` r
df_urbnindicators %>%
  attr("codebook") %>%
  filter(str_detect(calculated_variable, "public_assistance")) %>%
  reactable::reactable()
```

The most common convention is that a percent variable is calculated by
dividing a count variable (e.g., `public_assistance_received`) by the
universe variable for the corresponding table (e.g.,
`public_assistance_universe`). But other derived variables are more
complex, such as those for commute mode. Here, we’ve aggregated three
counts representing different types of individual motor vehicle
transportation to create the numerator, while the denominator is the
table universe minus individuals who work from home.

``` r
df_urbnindicators %>%
  attr("codebook") %>%
  filter(str_detect(calculated_variable, "means.*motor_vehicle")) %>%
  reactable::reactable()
```

This allows us say something along the lines of: “Of individuals who
commute to work, XX% use a motor vehicle as their primary commute mode.”

## So What’s the Value Add?

Hopefully the process above has illustrated some of the advantages,
which fall into two buckets: efficiency and reliability.

1.  Speed:

    1.  Sensible decisions about variable and table selection eliminate
        the need to hunt for the right variables.

    2.  Raw ACS variables are included alongside pre-calculated
        variables, such as percentages, that are not directly available
        from the ACS.

    3.  Query multiple years and/or multiple states in a single function
        call– no need to loop over your desired years or states.

2.  Accuracy:

    1.  Alphanumeric variable codes (e.g., `B18101_001`) are replaced
        with meaningful variable names (e.g.,
        `disability_percent`)–eliminating a common source of confusion
        and error.

    2.  Data are returned with a codebook that documents how variables
        were created and what they represent.

    3.  Behind-the-scenes quality checks help to ensure (though do not
        guarantee) that calculations are correct, though users still
        should verify results themselves.

    4.  Both straight-from-the-ACS and derived variables include margins
        of error and coefficients of variation that enable users to
        conduct statistical significance testing and can inform other
        data quality decision-making, such as whether to suppress
        certain observations or whether data should be aggregated–across
        geographies or across variables, or both–to improve the accuracy
        of estimates.
