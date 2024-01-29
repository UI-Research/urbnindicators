
<!-- README.md is generated from README.Rmd. Please edit that file -->

# urbnindicators

<!-- badges: start -->
<!-- badges: end -->

The goal of `urbnindicators` is to provide users with a simple interface
to generate a ready-for-analysis dataset of common social sciences
measures. It is built on top of the `tidycensus` package and abstracts
common query and cleaning tasks–such as identifying required variable
codes to pass to `tidycensus::get_acs()`, calculating percentages and
other measures of interest that are derived from count estimates, and
renaming variables such that they are semantically-meaningful (but still
programmatically functional). In addition to simplicity, it includes a
series of data quality checks to help ensure that measures are
accurately computed and to provide users with quantifications of
measures’ reliability (using coefficients of variation) rather than
merely returning margins of error (which are often immediately dropped
from analysis workflows without further consideration.

(aspirational) `urbnindicators` also provides a set of related tools to
facilitate common tasks when working with Census data (and ACS data in
particular), such as calculating calculating measures of segregation,
interpolating data across geographies and over time, and providing
change-over-time measures.

## Installation

Install the development version of urbnindicators from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UI-Research/urbnindicators")
```

This package is under active development with frequent updates–install
an up-to-date version frequently!

## urbnindicators in action

``` r
library(urbnindicators)
library(ggplot2)

acs_df = compile_acs_data(
  variables = NULL,
  years = c(2021, 2022),
  geography = "county",
  states = "NJ",
  counties = NULL,
  retain_moes = TRUE)
#> Warning in compile_acs_data(variables = NULL, years = c(2021, 2022), geography = "county", : Variable names and geographies for ACS data products can change between years.
#> Changes to geographies are particularly significant across decades (e.g., from 2019 to 2020), but these changes can occur in any year.
#> Users should ensure that the logic embedded in this function--which was developed around five-year ACS estimates for 2017-2021--remains accurate for their use cases.
#> Evaluation of measures and geographies over time should be thoroughly quality checked.
#> Getting data from the 2017-2021 5-year ACS
#> Warning: • You have not set a Census API key. Users without a key are limited to 500
#> queries per day and may experience performance limitations.
#> ℹ For best results, get a Census API key at
#> http://api.census.gov/data/key_signup.html and then supply the key to the
#> `census_api_key()` function to use it throughout your tidycensus session.
#> This warning is displayed once per session.
#> Getting data from the 2018-2022 5-year ACS

acs_df %>% 
  dplyr::select(c(
    NAME,
    data_source_year, 
    race_personofcolor_percent)) %>%
  dplyr::mutate(county_name = NAME %>% stringr::str_remove(" County, New Jersey")) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(county_name, race_personofcolor_percent), 
      y = race_personofcolor_percent, 
      color = factor(data_source_year))) +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "Change in NJ Counties' Populations of Color, 2021 to 2022",
    x = "County", 
    y = "Share of population who are people of color",
    color = "Year") +
  ggplot2::scale_y_continuous(
    breaks = c(0, .25, .50, .75, 1.0),
    labels = scales::percent) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 9, face = "italic"),
    plot.title = ggplot2::element_text(size = 11, face = "bold"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(size = 9, face = "bold"),
    legend.text = ggplot2::element_text(size = 8))
```

<img src="man/figures/README-example-1.png" width="100%" />
