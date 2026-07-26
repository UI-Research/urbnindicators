---
name: urbnindicators
description: >
  Acquire and analyze five-year American Community Survey (ACS) data using the
  urbnindicators package in R. Provides correct treatment of margins of error (MOEs),
  allows for interpolation, and returns human-readable variable names and an associated
  codebook. Use when the user wants ACS or Census data; mentions urbnindicators; 
  or needs a raw ACS table code like B25070, margins of error, or a variable codebook.
metadata:
  author: Will Curran-Groome (@wcurrangroome)
  version: "1.0"
license: GPL-3
---

# Pulling ACS data with urbnindicators

`urbnindicators` returns analysis-ready five-year ACS
data: raw counts, derived percentages, margins of error, and an attached
codebook. One `compile_acs_data()` call usually answers the whole question.

## Prerequisites

Run R non-interactively: `Rscript -e "..."`.

An API key is required. Check it exists:

```sh
Rscript -e 'nchar(Sys.getenv("CENSUS_API_KEY"))'
```

If it is absent, tell the user to request one at
<https://api.census.gov/data/key_signup.html> and install it with
`tidycensus::census_api_key("KEY", install = TRUE)`. 

The package is GitHub-only: `renv::install("UI-Research/urbnindicators")`.

## Required workflow — never guess a variable name

The package's value is its semantic variable names
(`race_nonhispanic_white_alone_percent`). They are not guessable. Discover them
first, every time:

```r
list_tables()                              # which registered table covers the topic
list_variables(year = 2024)                # exact column names, with their table
get_acs_codebook(2024, table = "B25070")   # raw ACS variables behind a code
compile_acs_data(tables = ..., states = ..., years = ...)   # one narrow call
attr(df, "codebook")                       # confirm each variable means what it seems to
```

**Rule: do not write a `compile_acs_data()` call containing a table or variable
name that has not been returned by `list_tables()` or `list_variables()` in this
session.** 

`list_variables(year)` returns a tibble of every variable — raw and computed —
mapped to its table, so grep it for the concept rather than guessing:

```r
Rscript -e 'library(urbnindicators)
  list_variables(2024) |> subset(grepl("cost_burden", variable))'
```

## compile_acs_data()

```r
compile_acs_data(tables = NULL, years = latest_acs_year(), geography = "county",
                 states = NULL, counties = NULL, spatial = FALSE,
                 denominator = "parent", cache = FALSE, ...)
```

`tables` accepts three kinds of element, mixable:

- **Registered table names** — `"snap"`, `"race"`, `"cost_burden"`. See
  `list_tables()` for the full set.
- **Raw ACS table codes** — `"B25070"`, `"C15002B"`. Auto-processed: label
  hierarchy parsed, percentages computed.
- **`define_*()` objects** — custom derived variables. See
  `references/custom-variables.md`.

Mixing strings and definitions **requires `list()`, never `c()`**:

```r
compile_acs_data(
  tables = list("snap", define_percent("snap_received", "snap_universe")),
  years = 2024, geography = "county", states = "DC")
```

Other behavior worth knowing:

- `years` defaults to *current year − 2*. That is a guess at the latest release,
  not a validated one; it passes validation and can still fail at the API.
  **Pass an explicit year.** Valid range is 2009 onward.
- A raw code requested alongside a registered table covering the same ACS table
  returns the **registered** version, with a warning. Request the code alone to
  get the auto-processed version.
- `total_population` is always included.
- `denominator` controls auto-table percentages only: `"parent"` (nearest
  subtotal, default), `"total"` (`_001`), or a specific code like `"B25070_001"`.
  Percentages are skipped for tables of medians, averages, and aggregates.

## Scoping and cost

These are rules, not suggestions.

- **Always pass `tables`.** `tables = NULL` pulls *every* registered table —
  30-plus tables and many hundreds of columns.
- **Always pass `states`** (or `counties`) for `"county"`, `"place"`,
  `"county subdivision"`, `"tract"`, and `"block group"`. Only block group
  *enforces* this — it aborts without `states`. Everywhere else, omitting it
  silently pulls all 50 states plus DC. Tract-level is the dangerous case:
  nothing stops a national pull of ~85,000 tracts.
- **Pass `cache = TRUE`** for exploratory or repeated work. Entries are keyed
  per geography × year × state × table and never expire, because published
  five-year estimates do not change. `clear_acs_cache()` reclaims the space.
- **Confirm with the user before** a national tract-level pull, tract data
  across more than 5 states, or a large multi-year × multi-state combination.

## Reading the output

A wide data frame. `data_source_year` and `GEOID` lead; `NAME` is retained.

| Suffix | Meaning |
|---|---|
| `_universe` | the denominator/total for that table |
| `_percent` | **proportion, 0–1** |
| `_M` | margin of error for the same-named estimate |

**`_percent` columns are proportions between 0 and 1, not 0–100.** Multiply by
100 exactly once when presenting to a human. A value of `0.23` is 23%.

Two attributes ride along:

- `attr(df, "codebook")` — a tibble with stable columns `calculated_variable`,
  `variable_type`, `definition`, `numerator_vars` / `denominator_vars` (and
  `_subtract_` variants), `se_calculation_type`, `aggregation_strategy`. Read
  the `definition` for any variable before reporting it.
- `attr(df, "resolved_tables")` — used by `interpolate_acs()`.

**Attributes survive subsetting but not reshaping.** Verified behavior:

| Preserved | Dropped |
|---|---|
| `filter()`, `select()`, `mutate()`, `arrange()`, `slice()`, `distinct()` | `summarise()` (with or without `group_by()`) |
| `left_join()`, `bind_rows()`, `rbind()` | `pivot_longer()`, `pivot_wider()` |
| base `[`, `subset()`, `as.data.frame()` | |
| `sf::st_transform()`, `st_drop_geometry()`, `st_centroid()` | |

The rule of thumb: anything that **aggregates or reshapes** rebuilds the data
frame and loses the attributes. `interpolate_acs()` and `view_acs_data()` then
abort. Do those steps last, or capture and re-attach:

```r
cb = attr(df, "codebook")
x = df %>% group_by(region) %>% summarise(across(everything(), sum))
attr(x, "codebook") = cb
```

When in doubt, check rather than assume: `!is.null(attr(x, "codebook"))`.

## Margins of error

Report estimates with their `_M`. Derived `_M` values are calculated using Census Bureau
approximation formulas; interpret with care.

**Before claiming two estimates differ, test them.** ACS estimates at tract and
block-group level are frequently not statistically distinguishable:

```r
tidycensus::significance(est1 = a, est2 = b, moe1 = a_M, moe2 = b_M, clevel = 0.90)
```

Returns `TRUE` when the two estimates are significantly different. Name the
arguments — the order is `(est1, est2, moe1, moe2)`, not interleaved.

90% confidence is the convention used in the package's own
`vignette("quantified-survey-error")`. Do not describe a difference as real,
higher, or lower without this test. Block-group estimates carry especially
large MOEs — say so whenever using them.

## view_acs_data()

An interactive Shiny map viewer. Requires `spatial = TRUE` and an explicit
`geography_extent`. An agent cannot drive it — **offer the call for the user to
run**, rather than launching it in a non-interactive session:

```r
df = compile_acs_data(tables = "snap", years = 2024, geography = "tract",
                      states = "NJ", spatial = TRUE)
view_acs_data(df, geography = "tract", geography_extent = c("county", "state"))
```

## References

Read these only when the task calls for them:

- **`references/custom-variables.md`** — the `define_percent()` /
  `define_sum()` / `define_complement()` / `define_metadata()` Domain Specific Language (DSL)
  functions that allow for constructing derived custom variables with correct margins of error.
- **`references/geographies-and-interpolation.md`** — which geographies are
  supported and what each requires, block-group rules and limits, the 2020
  tract boundary break, and `interpolate_acs()` for aggregating to custom
  geographies.
- **`references/troubleshooting.md`** — error message → cause → fix, covering
  the four API-key failure states, unknown tables and variables, dropped
  codebooks, block-group drops, and the auto/registered overlap warning.
