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
df = compile_acs_data(year = 2024, geography = "us")
#>   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |======                                                                |   8%  |                                                                              |=======                                                               |  10%  |                                                                              |=========                                                             |  12%  |                                                                              |==========                                                            |  14%  |                                                                              |============                                                          |  16%  |                                                                              |=============                                                         |  18%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  23%  |                                                                              |==================                                                    |  25%  |                                                                              |===================                                                   |  27%  |                                                                              |=====================                                                 |  29%  |                                                                              |======================                                                |  31%  |                                                                              |=======================                                               |  33%  |                                                                              |=========================                                             |  35%  |                                                                              |============================                                          |  39%  |                                                                              |==============================                                        |  43%  |                                                                              |================================                                      |  46%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=======================================                               |  55%  |                                                                              |===========================================                           |  61%  |                                                                              |=============================================                         |  65%  |                                                                              |================================================                      |  69%  |                                                                              |===================================================                   |  73%  |                                                                              |=====================================================                 |  76%  |                                                                              |========================================================              |  80%  |                                                                              |===========================================================           |  84%  |                                                                              |==============================================================        |  88%  |                                                                              |================================================================      |  92%  |                                                                              |===================================================================   |  96%  |                                                                              |======================================================================| 100%
codebook = attr(df, "codebook")
```

## Understanding the columns

The codebook has three columns:

- **Variable**: The variable name as it appears in the dataframe (e.g.,
  `snap_received_percent`).

- **Type**: The kind of variable. Common types include:

  - `count` – a raw ACS estimate directly from the API
  - `percent` – a derived ratio, typically a count divided by a universe
    variable
  - `metadata` – a non-computed variable such as a median or geographic
    identifier

- **Definition**: A formula describing how the variable was calculated.
  For raw ACS variables, this is the original Census Bureau variable
  code (e.g., `B22003_002`). For derived variables, this is an
  expression like `snap_received / snap_universe` that shows the
  numerator and denominator. More complex definitions may reference
  multiple raw variables joined with `+` in the numerator or
  denominator.

These definition strings are also used internally to calculate margins
of error for derived variables, so their accuracy is critical.

## Browse the codebook

Use the search box below to filter by variable name, type, or definition
text. Note that this codebook reflects all variables from the tables
returned by
[`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md),
but if you were to specify different tables in your
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
call, your codebook would comprise different variable listings.
