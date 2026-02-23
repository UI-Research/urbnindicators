# CLAUDE.md

## What this package does

**urbnindicators** is an R package that provides analysis-ready American
Community Survey (ACS) data with minimal user effort. The main entry
point is
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md),
which pulls hundreds of standardized variables (raw counts + calculated
percentages), generates a codebook, and computes margins of error and
coefficients of variation.

- Five-year ACS estimates only; tract-level geography and up (no block
  groups)
- Lifecycle stage: experimental
- Repository: <https://github.com/UI-Research/urbnindicators>

## Build and test

``` r
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-compile_acs_data.R")

# Check package (full R CMD check)
devtools::check()

# Rebuild documentation (after editing roxygen comments)
devtools::document()
```

Tests use pre-saved `.rds` files in `inst/test-data/` for
reproducibility and speed. The test framework is testthat edition 3.

CI runs on GitHub Actions: `test-coverage.yaml` (push/PR to main) and
`pkgdown.yaml` (site deployment).

## Code style

- **Assignment**: use `=` (not `<-`)
- **Indentation**: 2 spaces
- **Naming**: `snake_case` for functions and variables
- **Variable naming pattern**:
  `[concept]_[subconcept]_[characteristic]_[metric]` (e.g.,
  `race_nonhispanic_white_alone_percent`)
- **Variable suffixes**: `_percent` for percentages, `_universe` or
  `_universe_` for universe variables, `_M` for margins of error, `_CV`
  for coefficients of variation, `_SE` for standard errors
- **Documentation**: roxygen2 (v7.3.2) with markdown mode enabled
- **Conditionals**:
  [`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
  (not base [`ifelse()`](https://rdrr.io/r/base/ifelse.html))
- **Division**: use `safe_divide(x, y)` for percentage calculations
  (returns 0 instead of NaN)
- **Iteration/Loops**: use functions from
  [`library(purrr)`](https://purrr.tidyverse.org/)–avoid for and while
  loops

## Architecture

### Table registry (`R/table_registry.R`)

The table registry is the central data structure that defines all ACS
tables the package supports. It is a package-level environment
(`.table_registry`) that stores table definitions. Each table definition
is a list containing:

- `name` - table identifier (e.g., `"snap"`, `"race"`)
- `description` - human-readable description
- `acs_tables` - ACS table codes (e.g., `"B22003"`)
- `depends_on` - other tables this table requires (e.g.,
  `population_density` depends on `total_population`)
- `constructs` - (optional) list of construct definitions for
  multi-construct tables; each has `name` and `variable_pattern`
- `raw_variable_source` - how raw variables are obtained: `"manual"`
  (listed explicitly) or `"select_variables"` (resolved at runtime via
  [`select_variables_by_name()`](https://ui-research.github.io/urbnindicators/reference/select_variables_by_name.md))
- `raw_variables` - named vector of ACS variable codes (for manual
  sources)
- `compute_fn` - function that takes `.data` and returns `.data` with
  derived columns added
- `codebook_entries` - structured metadata for codebook generation

There are 30+ registered internal tables.

### Table selection API

Users can request specific subsets of data:

``` r
# Pull specific tables (using construct-level names)
compile_acs_data(tables = c("race", "snap"), years = 2022, geography = "county", states = "NJ")

# Pull by indicator name (returns the full parent table)
compile_acs_data(indicators = c("snap_received_percent"), years = 2022, geography = "county", states = "NJ")

# Discover available tables, indicators, and variables
list_tables()
list_variables()  # tibble of all variables and their table names
```

**Construct-level table names**: Some ACS tables contain multiple
constructs. These are split into separate user-facing tables: -
`sex_by_age` (internal) → `age` + `sex` (user-facing) -
`nativity_language` (internal) → `nativity` + `language` (user-facing)

Both construct names and internal names are accepted by
`compile_acs_data(tables = ...)` and `resolve_tables()`.

When `tables`/`indicators` are specified: 1. `resolve_tables()`
determines which tables are needed (always includes `total_population`)
2. `collect_raw_variables()` builds the named ACS variable vector for
those tables 3. Only those tables’ `compute_fn` functions are called 4.
Codebook and CVs are generated only for returned variables 5. Tigris
geometry is fetched only when `spatial = TRUE` or `"population_density"`
is in the resolved tables

### Key source files

1.  **`R/table_registry.R`** - Central registry: table definitions,
    [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md),
    [`list_indicators()`](https://ui-research.github.io/urbnindicators/reference/list_indicators.md),
    `resolve_tables()`, `collect_raw_variables()`,
    `expand_codebook_entry()`, and all `register_table()` calls.
2.  **`R/list_acs_variables.R`** -
    [`list_acs_variables()`](https://ui-research.github.io/urbnindicators/reference/list_acs_variables.md)
    (supports optional `tables` param),
    [`select_variables_by_name()`](https://ui-research.github.io/urbnindicators/reference/select_variables_by_name.md),
    [`filter_variables()`](https://ui-research.github.io/urbnindicators/reference/filter_variables.md).
3.  **`R/compile_acs_data.R`** -
    [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
    (with `tables`, `indicators`, deprecated `variables`),
    `internal_compute_acs_variables()` (legacy),
    [`safe_divide()`](https://ui-research.github.io/urbnindicators/reference/safe_divide.md).
4.  **`R/generate_codebook.R`** -
    [`generate_codebook()`](https://ui-research.github.io/urbnindicators/reference/generate_codebook.md)
    (registry-based) and `generate_codebook_legacy()` (AST-based, for
    deprecated `variables` path).
5.  **`R/calculate_cvs.R`** - Computes standard errors and coefficients
    of variation. Parses codebook definition text strings. No changes
    needed when adding tables.
6.  **`R/make_pretty_names.R`** - Converts variable names to
    publication-ready labels.
7.  **`R/utils-pipe.R`** - Re-exports `%>%`.

### Exported functions

- `compile_acs_data(tables, indicators, ...)` - Pull and compute ACS
  data
- [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md) -
  Available table names for the `tables` parameter (construct-level
  names)
- [`list_indicators()`](https://ui-research.github.io/urbnindicators/reference/list_indicators.md) -
  Available indicator names for the `indicators` parameter
- `list_variables(year)` - Tibble mapping all variables (raw + computed)
  to their table name
- `list_acs_variables(year, tables)` - Named vector of ACS variable
  codes
- `select_variables_by_name(variable_name, census_codebook)` - Filter
  variables by pattern
- `filter_variables(variable_vector, match_string, match_type)` -
  Further filter variables
- `make_pretty_names(.data, .case)` - Publication-ready variable names
- `safe_divide(x, y)` - Safe division (0 instead of NaN)

## Contributing: adding new tables

To add a new ACS table to the package:

1.  **Add a `register_table()` call in `R/table_registry.R`** with:
    - `raw_variables` (manual) or `raw_variable_source`
      (select_variables) for raw ACS variables
    - `compute_fn` that calculates derived indicators using
      [`safe_divide()`](https://ui-research.github.io/urbnindicators/reference/safe_divide.md)
      and
      [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html)
    - `codebook_entries` with structured entries (types:
      `simple_percent`, `across_percent`, `across_sum`, `complex`,
      `one_minus`, `metadata`)
2.  **Add any new global variables** to the
    [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
    call at the bottom of `R/table_registry.R`
3.  **Verify**: `devtools::load_all()` then
    [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)
    shows your table;
    [`list_indicators()`](https://ui-research.github.io/urbnindicators/reference/list_indicators.md)
    shows your indicators
4.  **Verify codebook**: the codebook auto-generates from
    `codebook_entries` – no changes to `R/generate_codebook.R` needed
5.  **Verify CVs**: `R/calculate_cvs.R` parses codebook definition
    strings – no changes needed if definitions follow standard patterns
6.  **Update pretty names** if needed (`R/make_pretty_names.R` – rarely
    needed)

### Codebook entry types

| Type             | Use case                                                                                          | Key fields                                                                                                                                               |
|------------------|---------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| `simple_percent` | Single numerator / denominator                                                                    | `output`, `numerator`, `denominator`                                                                                                                     |
| `across_percent` | [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html) percentages                | `input_regex`, `exclude_regex`, `output_suffix`, `denominator` or `denominator_fn`                                                                       |
| `across_sum`     | [`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html) sums (e.g., male + female) | `input_regex`, `addend_fn`, `output_naming_fn`                                                                                                           |
| `complex`        | Multi-variable numerator/denominator                                                              | `output`, `numerator_regex` or `numerator_variables`, `denominator_variables`, optional `subtract_*` (denominator) or `numerator_subtract_*` (numerator) |
| `one_minus`      | Complement (1 - x)                                                                                | `output`, `source_variable`                                                                                                                              |
| `metadata`       | Non-computed variables                                                                            | `output`, `definition_text`                                                                                                                              |

### Quality checks for new variables

- Percentages must be 0-1 bounded
- All measures must have meaningful, non-missing values
- At least 2 distinct values per measure
- CVs should be reasonable for tract-level data (flag if \>50 for many
  tracts)
- Compare to published Census Bureau benchmarks when available

## Legacy path

The `variables` parameter on
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
is deprecated (with
[`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)).
When used, it triggers the legacy code path:
`internal_compute_acs_variables()` for computation and
`generate_codebook_legacy()` for AST-based codebook generation. Both are
preserved for backward compatibility.

## Search strategy

Prefer **Grep** and **Glob** over the Explore agent for code search.
This codebase is small enough that targeted searches are almost always
sufficient and far more token-efficient. Use parallel Grep/Glob calls
when multiple patterns are needed.

- **Glob**: find files by name pattern (e.g., `**/*registry*.R`)
- **Grep**: find code by content pattern (e.g., `right_join.*geometries`
  in `R/compile_acs_data.R`)
- **Read**: examine a specific file once you know where to look

Only use the Explore agent for genuinely open-ended questions where you
don’t know what to search for (e.g., “how does the authentication system
work across the whole codebase”). For bug fixes, error tracebacks almost
always point to the right file and line — start there with Grep/Read.

## Dependencies

Core: `tidycensus`, `dplyr`, `tidyr`, `purrr`, `stringr`, `sf`,
`tigris`, `magrittr`, `rlang`, `tibble`, `janitor`, `lifecycle`

Dependency management uses `renv` (lockfile: `renv.lock`).
