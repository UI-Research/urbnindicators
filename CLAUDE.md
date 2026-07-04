# CLAUDE.md

## What this package does

**urbnindicators** is an R package that provides analysis-ready American Community Survey (ACS) data with minimal user effort. The main entry point is `compile_acs_data()`, which pulls hundreds of standardized variables (raw counts + calculated percentages), generates a codebook, and computes margins of error.

- Five-year ACS estimates only; block-group geography and up. Block groups are supported for 2013+ and require an explicit `states` argument; only the subset of tables the ACS publishes at the block-group level is returned (others are dropped with a warning). See "Block-group geography" below.
- Lifecycle stage: experimental
- Repository: https://github.com/UI-Research/urbnindicators

## Build and test

```r
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

Tests use pre-saved `.rds` fixtures (compiled output + codebook) in `tests/testthat/fixtures/`, which are committed to the repository so fixture-gated tests run on CI and fresh clones. Regeneration recipes live in the header comments of the test files that use them (regenerate after changing any table definition, then re-run the suite). Tests that hit the live Census API are gated on `skip_if_no_census_key()` (see `tests/testthat/helper-skips.R`); the variables metadata endpoint requires a `CENSUS_API_KEY` even for discovery functions. The test framework is testthat edition 3.

CI runs on GitHub Actions: `test-coverage.yaml` (push/PR to main) and `pkgdown.yaml` (site deployment).

## Code style

- **Assignment**: use `=` (not `<-`)
- **Indentation**: 2 spaces
- **Naming**: `snake_case` for functions and variables
- **Variable naming pattern**: `[concept]_[subconcept]_[characteristic]_[metric]` (e.g., `race_nonhispanic_white_alone_percent`)
- **Variable suffixes**: `_percent` for percentages, `_universe` or `_universe_` for universe variables, `_M` for margins of error
- **Documentation**: roxygen2 (v7.3.2) with markdown mode enabled
- **Conditionals**: `dplyr::if_else()` (not base `ifelse()`)
- **Division**: use `safe_divide(x, y)` for percentage calculations (returns 0 instead of NaN)
- **Iteration/Loops**: use functions from `library(purrr)`--avoid for and while loops

## Architecture

### Table registry (`R/table_registry.R`)

The table registry is the central data structure that defines all ACS tables the package supports. It is a package-level environment (`.table_registry`) that stores table definitions. Each table definition is a list containing:

- `name` - table identifier (e.g., `"snap"`, `"race"`)
- `description` - human-readable description
- `acs_tables` - ACS table codes (e.g., `"B22003"`)
- `depends_on` - other tables this table requires (e.g., `population_density` depends on `total_population`)
- `constructs` - (optional) list of construct definitions for multi-construct tables; each has `name` and `variable_pattern`
- `raw_variable_source` - how raw variables are obtained: `list(type = "manual")` (listed explicitly) or `list(type = "select_variables", calls = list(...))` (resolved at runtime by pattern). All currently-registered tables use the manual form.
- `raw_variables` - named vector of ACS variable codes (for manual sources)
- `definitions` - list of DSL objects (`define_percent()`, `define_sum()`, `define_complement()`, `define_metadata()`) describing derived variables. Codebook entries and MOE-propagation strategy are derived from each object's `type`.

There are 34 registered internal tables.

### Table selection API

Users can request specific subsets of data:

```r
# Pull specific tables (using construct-level names)
compile_acs_data(tables = c("race", "snap"), years = 2022, geography = "county", states = "NJ")

# Discover available tables and variables
list_tables()
list_variables()       # tibble of all variables and their table names
get_acs_codebook()     # browse ACS variables with clean names and table codes
```

**Construct-level table names**: Some ACS tables contain multiple constructs. These are split into separate user-facing tables:
- `sex_by_age` (internal) → `age` + `sex` (user-facing)
- `nativity_language` (internal) → `nativity` + `language` (user-facing)

Both construct names and internal names are accepted by `compile_acs_data(tables = ...)` and `resolve_tables()`.

`tables` can contain three kinds of elements (mix freely inside a `list()`):
- **Registered table names** (e.g., `"race"`, `"snap"`) — use `list_tables()` to see them all.
- **Raw ACS table codes** (e.g., `"B25070"`, `"C15002B"`) — any valid Detailed/Collapsed code is auto-processed at runtime: raw variables are fetched, the label hierarchy is parsed, and percentages are computed automatically. The `denominator` parameter controls the percentage denominator (`"parent"` for nearest subtotal, `"total"` for `_001`, or a specific code like `"B25070_001"`). If a raw code is already covered by a registered table, the registered version is used.
- **DSL definition objects** from `define_percent()`/`define_sum()`/`define_complement()`/`define_metadata()` — let users layer custom derived variables on top of the requested tables; results land in the returned data frame and the codebook with MOEs computed automatically.

When `tables` are specified:
1. `resolve_tables()` determines which registered tables are needed (always includes `total_population`).
2. `collect_raw_variables()` builds the named ACS variable vector for those tables.
3. `build_auto_table_entry()` synthesizes registry-like entries for any raw ACS codes.
4. `execute_definitions()` runs each table's `definitions` list (registered, then auto, then user) against the fetched data.
5. Codebook and MOEs are generated only for returned variables.
6. Tigris geometry is fetched only when `spatial = TRUE` or `"population_density"` is in the resolved tables.

### Block-group geography

`geography = "block group"` is supported for years **2013+** (the floor for `tigris::block_groups(cb = TRUE)`) and **requires an explicit `states` argument** (national block-group pulls are disallowed). Queries iterate county-by-county within each state.

The ACS publishes only a subset of detailed tables at the block-group level. **Availability is read from the codebook, not by querying data**: `tidycensus::load_variables(year, "acs5")` includes a `geography` column giving each variable's *lowest published geography*; a variable is block-group-available iff `geography == "block group"`. This is authoritative, year-specific, and free (the codebook is already loaded).

- `bg_partition_tables()` (in `R/table_registry.R`) classifies resolved tables into `keep` / `dropped` / `partial` using that column. Derived tables with no raw variables of their own (e.g., `population_density`) are always kept.
- In `compile_acs_data()`, the partition runs *before* querying: dropped tables and partial-table sub-variables are warned about as early as possible; the function errors only if **none** of the user's requested tables are available. Auto/raw ACS codes are checked the same way (tidycensus returns an uninformative error for these, so we drop them with a clear message instead).
- `collect_raw_variables(geography = "block group")` filters the variable vector to block-group-available codes (this is what drops a partial table's unavailable lines, e.g., the `B19013A–I` race iterations of median household income).
- `list_tables(geography = "block group")` returns the available subset.

Note: don't hard-code a list of block-group tables — derive it from the codebook so it stays correct across vintages.

### Key source files

1. **`R/table_registry.R`** - Central registry, DSL constructors (`define_percent()`, `define_sum()`, `define_complement()`, `define_metadata()`), `validate_definition()`, `execute_definitions()`, `resolve_tables()`, `collect_raw_variables()`, `expand_codebook_entry()`, `list_tables()`, and all `register_table()` calls.
2. **`R/compile_acs_data.R`** - `compile_acs_data()` (entry point), `fetch_acs()` (per-year/per-state tidycensus calls + ZCTA-style "super-state" geographies), `safe_divide()`.
3. **`R/auto_percent.R`** - Auto-table support for raw ACS table codes: `is_raw_acs_code()`, `resolve_to_acs_table()`, `build_auto_table_entry()`, `generate_auto_definitions()`.
4. **`R/interpolate_acs.R`** - `interpolate_acs()` plus internal aggregation helpers; uses codebook attributes to dispatch per-variable aggregation strategy.
5. **`R/list_acs_variables.R`** - `get_acs_codebook()` (exported), and internal helpers `select_variables_by_name()` and `filter_variables()` used by the registry's `"select_variables"` path. (The deprecated `list_acs_variables()` stub was removed before 0.1.0.)
6. **`R/load_acs_variables.R`** - Session-cached fetch of the Census `variables.json` metadata. Workaround for the API now requiring a key on the variables endpoint, which `tidycensus::load_variables()` doesn't send. Requires `CENSUS_API_KEY`.
7. **`R/generate_codebook.R`** - `generate_codebook()` builds the codebook tibble from registered + auto + user definitions.
8. **`R/calculate_cvs.R`** - Computes margins of error for derived variables (standard errors used internally). Drives off the codebook's `se_calculation_type` column; no per-table changes needed when adding new tables.
9. **`R/make_pretty_names.R`** - Converts variable names to publication-ready labels.
10. **`R/utils-clean-names.R`**, **`R/utils-pipe.R`** - Shared helpers; the latter re-exports `%>%`.

### Exported functions

- `compile_acs_data(tables, years, geography, states, counties, spatial, denominator, ...)` - Pull and compute ACS data.
- `interpolate_acs(.data, target_geoid_column, weight = NULL, crosswalk = NULL, source_geoid = "GEOID", weight_variable = "total_population_universe")` - Aggregate or interpolate ACS data to custom geographies. `weight = NULL` for complete nesting (direct aggregation); pass a weight column name for fractional allocation via crosswalk.
- `define_percent()`, `define_sum()`, `define_complement()`, `define_metadata()` - DSL constructors for derived variables. Use inside `register_table(definitions = list(...))` or pass directly to `compile_acs_data(tables = list(...))`.
- `list_tables()` - Available registered table names for the `tables` parameter (construct-level names).
- `list_variables(year)` - Tibble mapping all variables (raw + computed) to their table name.
- `get_acs_codebook(year, table)` - Browse ACS variables with clean names and table codes.
- `make_pretty_names(.data, .case)` - Publication-ready variable names.
- `safe_divide(x, y)` - Safe division (0 instead of NaN; NA when denominator is 0 and numerator is non-zero).

`select_variables_by_name()` and `filter_variables()` exist in `R/list_acs_variables.R` but are internal helpers used by the registry's `"select_variables"` source type; they are not exported.

## Contributing: adding new tables

Tables are defined in `R/table_registry.R`. Each table is registered via a `register_table(list(...))` call describing its raw ACS variables and the derived computations it should produce. Derived computations are expressed via the DSL functions `define_percent()`, `define_sum()`, `define_complement()`, and `define_metadata()` (see the next section). Most of the codebook, MOE propagation, and aggregation behavior is driven by these structured definitions, so adding a new table usually means: write the registry entry, run `devtools::load_all()`, confirm `list_tables()` shows it, and confirm `compile_acs_data(tables = "your_table")` returns the expected columns.

To add a new ACS table to the package:

1. **Add a `register_table()` call in `R/table_registry.R`** with:
   - `name` — table identifier (e.g., `"snap"`)
   - `description` — human-readable description
   - `acs_tables` — ACS table codes (e.g., `"B22003"`)
   - `depends_on` — other registered tables this one needs (often `character(0)`)
   - `raw_variable_source` — `list(type = "manual")` for an explicit list of variables, or `list(type = "select_variables", calls = list(list(pattern = "B22003_", filter = ...)))` for pattern-based selection
   - `raw_variables` — named character vector mapping `clean_name_` → `"BNNNNN_NNN"` (when source is manual)
   - `definitions` — a `list()` of DSL objects produced by `define_percent()`, `define_sum()`, `define_complement()`, `define_metadata()`
2. **Verify**: `devtools::load_all()`, then check `list_tables()` and `list_variables(year = ...) |> dplyr::filter(table == "<your_table>")`.
3. **Codebook and MOEs are automatic.** `expand_codebook_entry()` (in `R/table_registry.R`) sets `se_calculation_type` directly from each DSL object's `type`, so `calculate_moes()` and `interpolate_acs()` need no per-table changes when definitions follow the DSL.
4. **Pretty names**: update `R/make_pretty_names.R` only if your variable names need new title-case fixups.

### DSL functions for definitions

| Function | Use case | Key params |
|---|---|---|
| `define_percent(numerator, denominator)` | Single percentage; `output` is inferred as `<numerator>_percent` when `numerator` is a plain (non-regex) string | `numerator`, `denominator`, `output`, `subtract_from_numerator`, `subtract_from_denominator`, `exclude` |
| `define_percent(numerator, denominator, each = TRUE)` | Batch percentages — one output per column matching `numerator` regex; `output` is ignored (a warning is emitted if set) | `numerator` (regex), `denominator` or `denominator_replace`, `exclude` |
| `define_sum(columns, output)` | Sum columns into a single output | `columns` (character vector), `output` |
| `define_sum(columns, each = TRUE, add_replace, output_replace)` | Batch pairwise sums (e.g., female+male) | `columns` (regex), `add_replace`, `output_replace`, `exclude` |
| `define_complement(source, output)` | Complement (1 - x) | `source`, `output` |
| `define_metadata(output, definition)` | Non-computed variables (placeholder codebook entry only) | `output`, `definition` |

Each DSL function returns a list with a `type` field. `compile_acs_data()` accepts these objects directly in the `tables` argument (mixed with strings) so users can also define custom derived variables on top of the registered set.

### Quality checks for new variables

- Percentages must be 0-1 bounded
- All measures must have meaningful, non-missing values
- At least 2 distinct values per measure
- MOEs should be reasonable for tract-level data
- Compare to published Census Bureau benchmarks when available

## Legacy path

The `variables` parameter on `compile_acs_data()` is deprecated (with `lifecycle::deprecate_warn()`). Passing it now emits a deprecation warning and the value is ignored; use `tables` instead.

## Search strategy

Prefer **Grep** and **Glob** over the Explore agent for code search. This codebase is small enough that targeted searches are almost always sufficient and far more token-efficient. Use parallel Grep/Glob calls when multiple patterns are needed.

- **Glob**: find files by name pattern (e.g., `**/*registry*.R`)
- **Grep**: find code by content pattern (e.g., `right_join.*geometries` in `R/compile_acs_data.R`)
- **Read**: examine a specific file once you know where to look

Only use the Explore agent for genuinely open-ended questions where you don't know what to search for (e.g., "how does the authentication system work across the whole codebase"). For bug fixes, error tracebacks almost always point to the right file and line — start there with Grep/Read.

## Dependencies

Core: `tidycensus`, `dplyr`, `tidyr`, `purrr`, `stringr`, `sf`, `tigris`, `magrittr`, `rlang`, `tibble`, `janitor`, `lifecycle`

Dependency management uses `renv` (lockfile: `renv.lock`).
