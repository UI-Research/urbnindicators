
<!-- README.md is generated from README.Rmd. Please edit that file -->

# urbnindicators

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

# Overview

The goal of **`urbnindicators`** is to provide users with analysis-ready
data from the American Community Survey (ACS). With a single function
call, you get:

- Hundreds of standardized variables, such as percentages, in addition
  to the raw count variables used to produce them.

- Meaningful, consistent variable names.

- A codebook that describes how each variable is calculated.

- The built-in capacity to pull data for multiple years and multiple
  states.

- Supplemental measures, such as population density, that aren’t
  available from the ACS.

- Built-in quality checks to help ensure that calculated variables are
  accurate.

# Installation

Install the development version of `urbnindicators` from
[GitHub](https://github.com/) with:

    install.packages("renv")
    renv::install("UI-Research/urbnindicators")

Note that this package is under active development with frequent
updates–check to ensure you have the most recent version installed!

# Use

``` r
set_urbn_defaults(style = "print")

acs_df = compile_acs_data(
  variables = NULL,
  years = c(2017, 2022),
  geography = "county",
  states = "NJ",
  counties = NULL,
  spatial = TRUE) 

plot_data = acs_df %>% 
  transmute(
    county_name = NAME %>% str_remove(" County, New Jersey"), 
    race_personofcolor_percent, 
    data_source_year)

state_averages = plot_data %>%
  group_by(data_source_year) %>%
  summarize(mean_race_personofcolor_percent = mean(race_personofcolor_percent)) %>%
  arrange(data_source_year) %>%
  pull()

dumbbell_data = plot_data %>% 
  pivot_wider(
    names_from = data_source_year, 
    values_from = race_personofcolor_percent, 
    names_prefix = "year_")

ggplot() +
  geom_segment(
    data = dumbbell_data,
    aes(
      x = reorder(county_name, year_2017),
      y = year_2017,
      yend = year_2022),
    color = palette_urbn_main[7],
    linewidth = 1) +
  geom_point(
    data = plot_data, 
    aes(
      x = reorder(county_name, race_personofcolor_percent), 
      y = race_personofcolor_percent, 
      color = factor(data_source_year))) +
  annotate(
    "text", 
    y = .31,  
    x = 21.5, 
    label = "State mean (2017)", 
    fontface = "bold.italic", 
    color = palette_urbn_main[1]) +
  annotate(
    "text", 
    y = .47,  
    x = 21.5, 
    label = "State mean (2022)", 
    fontface = "bold.italic", 
    color = palette_urbn_main[2]) +
  geom_hline(
    yintercept = state_averages[1], 
    linetype = "dashed", 
    color = palette_urbn_main[1]) +
  geom_hline(
    yintercept = state_averages[2], 
    linetype = "dashed", 
    color = palette_urbn_main[2]) +
  labs(
    title = "Change in NJ Counties' Racial Composition, 2017 to 2022",
    subtitle = paste0(
      "On average, counties in NJ saw a ", 
      round((state_averages[2] - state_averages[1]), digits = 3) * 100, 
      " pecentage point increase in the share of the population who are people of color from 2017 to 2022.") %>%
      str_wrap(120), 
    x = "County", 
    y = "Share of population who are people of color",
    color = "Year") +
  scale_x_discrete(expand = expansion(mult = c(.03, 0.04))) +
  scale_y_continuous(
    breaks = c(0, .25, .50, .75, 1.0),
    limits = c(0, .75),
    labels = scales::percent) +
  coord_flip()
```

<img src="man/figures/README-example-1.png" width="100%" />

# Credits

This package is built on top of and enormously indebted to
`library(tidycensus)`, which provides the core functionality for
accessing the Census Bureau API.

For users who want additional variables, `library(tidycensus)` exposes
the entire range of pre-tabulated variables available from the ACS, as
well as access to ACS microdata and other Census Bureau datasets. Learn
more here: <https://walker-data.com/tidycensus/index.html>.
