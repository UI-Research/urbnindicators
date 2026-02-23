# Analysis-ready social science measures

Construct measures frequently used in social sciences research,
leveraging
[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html)
to acquire raw estimates from the Census Bureau API.

## Usage

``` r
compile_acs_data(
  tables = NULL,
  indicators = NULL,
  years = c(2022),
  geography = "county",
  states = NULL,
  counties = NULL,
  spatial = FALSE,
  ...
)
```

## Arguments

- tables:

  A character vector of table names to include (e.g.,
  `c("race", "snap")`). Use
  [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)
  to see available tables. When NULL (default) and `indicators` is also
  NULL, all tables are included.

- indicators:

  A character vector of indicator names to include (e.g.,
  `c("snap_received_percent")`). Each indicator's parent table is
  automatically included.

- years:

  A numeric vector of four-digit years for which to pull five-year
  American Community Survey estimates.

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

- ...:

  Deprecated arguments. If `variables` is passed, a deprecation warning
  is issued and the value is ignored.

## Value

A dataframe containing the requested variables, their MOEs, a series of
derived variables, such as percentages, and the year of the data.
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
## Pull all tables (default, backward-compatible)
df = compile_acs_data(years = c(2022), geography = "county", states = "NJ")

## Pull specific tables
df = compile_acs_data(tables = c("race", "snap"), years = 2022,
                      geography = "county", states = "NJ")

## Pull by indicator name (returns the full parent table)
df = compile_acs_data(indicators = c("snap_received_percent"),
                      years = 2022, geography = "county", states = "NJ")
  } # }
```
