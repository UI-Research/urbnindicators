# Get analysis-ready estimates and errors from the ACS

Obtain raw and construct derived measures (primarily percentages) from
the ACS, along with appropriately-pooled margins of error.

## Usage

``` r
compile_acs_data(
  tables = NULL,
  years = latest_acs_year(),
  geography = "county",
  states = NULL,
  counties = NULL,
  spatial = FALSE,
  denominator = "parent",
  cache = FALSE,
  ...
)
```

## Arguments

- tables:

  A character vector, list, or NULL specifying which data to include.
  Three kinds of elements are accepted and can be mixed freely inside a
  [`list()`](https://rdrr.io/r/base/list.html):

  - **Registered table names** (e.g., `"race"`, `"snap"`). These are
    pre-built tables with non-standard variable definitions. Use
    [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)
    to see all available registered tables.

  - **Raw ACS table codes** (e.g., `"B25070"`, `"C15002B"`). Any valid
    ACS Detailed or Collapsed table code can be passed directly. These
    return the full table along with percentage-based measures that are
    calculated on the fly. Use the `denominator` parameter to control
    how percentages are calculated.

  - **Custom data specifications** created with
    [`define_percent`](https://ui-research.github.io/urbnindicators/reference/define_percent.md),
    [`define_sum`](https://ui-research.github.io/urbnindicators/reference/define_sum.md),
    [`define_complement`](https://ui-research.github.io/urbnindicators/reference/define_complement.md),
    or
    [`define_metadata`](https://ui-research.github.io/urbnindicators/reference/define_metadata.md).
    These let you compute custom derived variables from the columns
    produced by the tables you request.

  When mixing strings and definitions, wrap everything in
  [`list()`](https://rdrr.io/r/base/list.html) (e.g.,
  `list("snap", define_percent(...))`). Raw ACS codes are always
  auto-processed, even when a registered table covers the same code
  (e.g., `"B22003"` returns the auto-processed table, not the registered
  `"snap"` table); registered tables are returned only when requested by
  name. If a requested code overlaps a registered table included in the
  same call, the registered version is returned and the auto-processed
  version is dropped with a warning. When NULL (default), all registered
  tables are included (unregistered ACS tables must be requested
  explicitly).

- years:

  A numeric vector of four-digit years for which to pull five-year
  American Community Survey estimates.

- geography:

  A geography type that is accepted by
  [`tidycensus::get_acs()`](https://walker-data.com/tidycensus/reference/get_acs.html),
  e.g., "tract", "county", "state", among others. `"block group"` is
  supported for years 2013 and later and requires an explicit `states`
  argument; because the ACS publishes only a limited subset of tables at
  the block-group level, requested tables that are not available there
  are dropped with a warning (use
  `list_tables(geography = "block group")` to see what is available).
  Block-group estimates carry large margins of error and should be used
  with care. Census blocks (geography = "block") are not supported, as
  the ACS publishes no block-level data.

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

- cache:

  Boolean. When `TRUE`, raw ACS query results are cached on disk and
  reused across calls and R sessions. Results are cached one file per
  geography-year-state-table combination, so subsequent calls
  re-download only what is not already cached – including when the
  `tables` selection changes. The cache lives at
  `tools::R_user_dir("urbnindicators", which = "cache")` (override via
  `options(urbnindicators.cache_dir = ...)`). Entries never expire,
  because published five-year ACS estimates do not change; use
  [`clear_acs_cache()`](https://ui-research.github.io/urbnindicators/reference/clear_acs_cache.md)
  to delete them and reclaim disk space.

- ...:

  Deprecated arguments. If `variables` is passed, a deprecation warning
  is issued and the value is ignored.

## Value

A dataframe containing the requested variables, their MOEs, a series of
derived variables, such as percentages, and the year of the data.
Returned data are formatted wide. A codebook is attached as an attribute
and can be accessed via `compile_acs_data() %>% attr("codebook")`. The
codebook is a tibble with these columns (treated as a stable interface):

- `calculated_variable` - the column name in the returned data

- `variable_type` - one of `"Count"`, `"Percent"`, `"Sum"`, `"Median"`,
  `"Median ($)"`, `"Average"`, `"Quintile ($)"`, `"Index"`, `"Metadata"`

- `definition` - human-readable description of the variable

- `numerator_vars`, `numerator_subtract_vars`, `denominator_vars`,
  `denominator_subtract_vars` - list-columns of clean column names used
  in the numerator/denominator (positive and subtractive terms) of a
  derived variable

- `se_calculation_type` - one of `"raw"`, `"sum"`, `"simple_percent"`,
  `"complex_numerator"`, `"complex_denominator"`, `"complex_both"`,
  `"one_minus"`, `"weighted_average"`, `"metadata"`, `"unknown"`;
  indicates which MOE-propagation formula is appropriate

- `aggregation_strategy` - one of `"sum"`, `"recalculate_percent"`,
  `"weighted_average"`, `"metadata"`, `"unknown"`; used by
  [`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md)

The resolved tables are also attached as a `"resolved_tables"` attribute
(used by
[`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md)).

Margins of error for derived variables (suffixed `_M`) are
approximations calculated per Census Bureau guidance for derived
estimates; they are an experimental feature and should be interpreted
with care.

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

## Pull block-group data (2013+, requires states; unavailable tables are dropped)
df = compile_acs_data(tables = c("race", "tenure"), years = 2022,
                      geography = "block group", states = "NJ")

## Pull an unregistered ACS table by code
df = compile_acs_data(tables = "B25070", years = 2022,
                      geography = "state", states = "DC")

## Mix registered and unregistered tables
df = compile_acs_data(tables = c("snap", "B25070"), years = 2022,
                      geography = "state", states = "DC")

## Use table total as denominator instead of parent subtotals
df = compile_acs_data(tables = "B25070", denominator = "total",
                      years = 2022, geography = "state", states = "DC")

## Add a custom derived variable alongside a registered table
df = compile_acs_data(
  tables = list(
    "snap",
    define_percent("snap_universe", "snap_universe",
                   subtract_from_numerator = "snap_received",
                   output = "snap_not_received_percent")),
  years = 2022, geography = "county", states = "DC")
  } # }
```
