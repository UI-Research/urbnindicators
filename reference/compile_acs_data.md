# Analysis-ready social science measures

Construct measures frequently used in social sciences research,
leveraging
[`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html)
to acquire raw estimates from the Census Bureau API.

## Usage

``` r
compile_acs_data(
  tables = NULL,
  years = c(2024),
  geography = "county",
  states = NULL,
  counties = NULL,
  spatial = FALSE,
  denominator = "parent",
  ...
)
```

## Arguments

- tables:

  A character vector of table names to include. Two formats are
  accepted:

  - **Registered table names** (e.g., `"race"`, `"snap"`). These are
    pre-built tables with curated variable definitions. Use
    [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)
    to see all available registered tables.

  - **Raw ACS table codes** (e.g., `"B25070"`, `"C15002B"`). Any valid
    ACS Detailed or Collapsed table code can be passed directly. These
    are auto-processed at runtime: raw variables are fetched, the label
    hierarchy is parsed, and percentages are computed automatically. Use
    the `denominator` parameter to control how percentages are
    calculated for these tables.

  Both formats can be mixed freely (e.g., `c("snap", "B25070")`). If an
  ACS code corresponds to an already-registered table, the registered
  version is used automatically. When NULL (default), all registered
  tables are included (unregistered ACS tables must be requested
  explicitly).

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

- denominator:

  Controls how auto-computed percentages choose their denominator.
  `"parent"` (default) uses the nearest parent subtotal from the ACS
  label hierarchy. `"total"` uses the table total (variable `_001`). A
  specific ACS variable code (e.g., `"B25070_001"`) uses that variable.
  Only affects unregistered (auto) tables; registered tables always use
  their predefined definitions.

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

## Pull an unregistered ACS table by code
df = compile_acs_data(tables = "B25070", years = 2022,
                      geography = "state", states = "DC")

## Mix registered and unregistered tables
df = compile_acs_data(tables = c("snap", "B25070"), years = 2022,
                      geography = "state", states = "DC")

## Use table total as denominator instead of parent subtotals
df = compile_acs_data(tables = "B25070", denominator = "total",
                      years = 2022, geography = "state", states = "DC")
  } # }
```
