# Analysis-ready social science measures

Construct measures frequently used in social sciences research,
leveraging
[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html)
to acquire raw estimates from the Census Bureau API.

## Usage

``` r
compile_acs_data(
  variables = NULL,
  years = c(2022),
  geography = "county",
  states = NULL,
  counties = NULL,
  spatial = FALSE
)
```

## Arguments

- variables:

  A named vector of ACS variables such as that returned from
  [`urbnindicators::list_acs_variables()`](https://ui-research.github.io/urbnindicators/reference/list_acs_variables.md).

- years:

  A character vector (or coercible to the same) comprising one or more
  four-digit years for which to pull five-year American Community Survey
  estimates.

- geography:

  A geography type that is accepted by
  [`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html),
  e.g., "tract", "county", "state", among others. Geographies below the
  tract level are not supported.

- states:

  A vector of one or more state names, abbreviations, or codes as
  accepted by
  [`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html).

- counties:

  A vector of five-digit county FIPS codes. If specified, this parameter
  will override the `states` parameter. If `NULL`, all counties in the
  the state(s) specified in the `states` parameter will be included.

- spatial:

  Boolean. Return a simple features (sf), spatially-enabled dataframe?

## Value

A dataframe containing the requested `variables`, their MOEs, a series
of derived variables, such as percentages, and the year of the data.
Returned data are formatted wide. A codebook generated with
[`generate_codebook()`](https://ui-research.github.io/urbnindicators/reference/generate_codebook.md)
is attached and can be accessed via
`compile_acs_data() %>% attr("codebook")`.

## See also

[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html),
which this function wraps.

## Examples

``` r
if (FALSE) { # \dontrun{
acs_variables = list_acs_variables(year = "2022")
df = compile_acs_data(
  variables = acs_variables,
  years = c(2021, 2022),
  geography = "county",
  states = "NJ",
  counties = NULL,
  spatial = FALSE)
  } # }
```
