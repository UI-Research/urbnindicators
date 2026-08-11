# The urbnindicators Codebook

``` r

library(urbnindicators)
library(dplyr)
library(reactable)
```

Every dataframe returned by
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
includes a codebook as an attribute. The codebook documents every
variable in the dataset–what it represents, how it was calculated, and
whether it is a raw ACS estimate or a derived measure.

## Accessing the codebook

The codebook is stored as an attribute of the dataframe and can be
retrieved with [`attr()`](https://rdrr.io/r/base/attr.html):

``` r

df = compile_acs_data(years = 2024, geography = "us")
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   4%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |========                                                              |  11%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  15%  |                                                                              |============                                                          |  17%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |==================                                                    |  25%  |                                                                              |===================                                                   |  27%  |                                                                              |======================                                                |  31%  |                                                                              |=========================                                             |  35%  |                                                                              |===========================                                           |  38%  |                                                                              |============================                                          |  40%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |====================================                                  |  51%  |                                                                              |======================================                                |  55%  |                                                                              |=========================================                             |  59%  |                                                                              |===========================================                           |  61%  |                                                                              |==============================================                        |  65%  |                                                                              |================================================                      |  69%  |                                                                              |===================================================                   |  73%  |                                                                              |======================================================                |  77%  |                                                                              |===========================================================           |  84%  |                                                                              |==============================================================        |  88%  |                                                                              |================================================================      |  92%  |                                                                              |======================================================================| 100%
codebook = attr(df, "codebook")
```

## Understanding the columns

The codebook’s three primary columns are:

- **calculated_variable**: The variable name as it appears in the
  dataframe (e.g., `snap_received_percent`).

- **variable_type**: The kind of variable. Values include `Count` (a raw
  ACS estimate directly from the API), `Percent` (a derived ratio),
  `Sum` (a derived count), `Median`, `Median ($)`, `Average`,
  `Quintile ($)`, `Index`, and `Metadata` (a non-computed field such as
  a geographic identifier).

- **definition**: A description of how the variable was calculated. Raw
  ACS variables read `"This is a raw ACS estimate."`; derived variables
  spell out their inputs alongside the original Census Bureau variable
  codes, e.g.,
  `"Numerator = snap_received (B22003_002). Denominator = snap_universe (B22003_001)."`
  Subtracted components appear after a `-`.

The codebook also carries a `universe` column giving the Census Bureau’s
published universe statement for each variable–the population the
variable describes, e.g., `Households` or
`Population 25 years and over`. Derived variables inherit the universe
of their numerator. (The Census API publishes universe statements for
the 2020 and later vintages; earlier vintages yield `NA`.) Supporting
columns are used for margin-of-error calculation and
re-aggregation–list-columns naming each variable’s numerator/denominator
components (`numerator_vars`, `denominator_vars`, and their `_subtract_`
counterparts), an `se_calculation_type`, and an `aggregation_strategy`
(how
[`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md)
treats the variable). Because margins of error for derived variables are
computed from these components, their accuracy is critical.

## Browse the codebook

The interactive table below shows the three primary columns. Use the
search box to filter by variable name, type, or definition text. Note
that this codebook reflects all variables from the tables returned by
[`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md),
but if you were to specify different tables in your
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
call, your codebook would comprise different variable listings.
