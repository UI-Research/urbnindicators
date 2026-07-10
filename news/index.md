# Changelog

## urbnindicators 0.1.0

First tagged release. This version consolidates a major overhaul of the
package’s API and internals.

### Breaking changes

- [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
  is now driven by a **`tables` parameter**: request registered tables
  by name (see
  [`list_tables()`](https://ui-research.github.io/urbnindicators/reference/list_tables.md)),
  raw ACS table codes (e.g., `"B25070"`, auto-processed at runtime),
  and/or custom variable definitions — mixed freely inside a
  [`list()`](https://rdrr.io/r/base/list.html). The old behavior (all
  tables) remains the default when `tables` is omitted. The `variables`
  parameter is deprecated and ignored.
- The definition helpers were consolidated:
  [`define_percent()`](https://ui-research.github.io/urbnindicators/reference/define_percent.md)
  (with `each = TRUE` for batch mode) replaces
  `define_across_percent()`;
  [`define_sum()`](https://ui-research.github.io/urbnindicators/reference/define_sum.md)
  replaces `define_across_sum()`;
  [`define_complement()`](https://ui-research.github.io/urbnindicators/reference/define_complement.md)
  replaces `define_one_minus()`. Single-string numerators/denominators
  are interpreted as regular expressions.
- `select_variables_by_name()` and `filter_variables()` are no longer
  exported; use
  [`get_acs_codebook()`](https://ui-research.github.io/urbnindicators/reference/get_acs_codebook.md)
  for variable discovery.
- `list_acs_variables()` was removed; use
  [`list_variables()`](https://ui-research.github.io/urbnindicators/reference/list_variables.md).
- Literal `$` characters are now stripped from generated variable names,
  so income- and rent-band columns (e.g., from B25074/B25106) are
  syntactic: `household_income_..._less_than_$10000` becomes
  `household_income_..._less_than_10000`.
- The renter cost-burden measures now carry an explicit tenure label:
  `cost_burdened_30percentormore_*` and
  `cost_burdened_50percentormore_*` are now
  `cost_burdened_renter_30percentormore_*` and
  `cost_burdened_renter_50percentormore_*`, disambiguating them from the
  new owner-side measures.
- Two variables were renamed for accuracy and consistency:
  `employment_civilian_labor_force_percent` is now
  `employment_civilian_labor_force_employed_percent` (it is the
  employment rate — employed persons over the civilian labor force), and
  `median_household_income_nhpi` is now
  `median_household_income_nhpi_alone`, matching its sibling
  race-iteration names.
- A **Census API key is now required** for all functions that read the
  ACS variable dictionary (including
  [`list_variables()`](https://ui-research.github.io/urbnindicators/reference/list_variables.md)
  and
  [`get_acs_codebook()`](https://ui-research.github.io/urbnindicators/reference/get_acs_codebook.md)),
  because the Census Bureau’s variables metadata endpoint now requires a
  key. Set one with
  `tidycensus::census_api_key("YOUR_KEY", install = TRUE)`.
- [`safe_divide()`](https://ui-research.github.io/urbnindicators/reference/safe_divide.md)
  now returns `NA` (not `0`) when a nonzero numerator is divided by
  zero; `0/0` still returns `0`.
- Raw ACS table codes are now **always auto-processed**, even when a
  registered table covers the same code: `tables = "B22003"` returns the
  auto-processed table with label-derived names, while `tables = "snap"`
  returns the registered version (previously the raw code was silently
  upgraded to the registered table). Requesting both in one call returns
  the registered version and drops the auto-processed one with a
  warning, since the two would fetch duplicate variables.

### New features

- **Block-group geography**:
  `compile_acs_data(geography = "block group")` is supported for 2013
  and later (explicit `states` required). Tables the ACS does not
  publish at the block-group level are dropped with a warning;
  `list_tables(geography = "block group")` lists what is available.
- **Custom geographies**:
  [`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md)
  aggregates (complete nesting) or interpolates (fractional allocation
  via crosswalk weights) estimates to user-defined geographies,
  recalculating percentages and propagating margins of error with Census
  Bureau approximation formulas.
- **Custom derived variables**: pass
  [`define_percent()`](https://ui-research.github.io/urbnindicators/reference/define_percent.md)
  /
  [`define_sum()`](https://ui-research.github.io/urbnindicators/reference/define_sum.md)
  /
  [`define_complement()`](https://ui-research.github.io/urbnindicators/reference/define_complement.md)
  /
  [`define_metadata()`](https://ui-research.github.io/urbnindicators/reference/define_metadata.md)
  objects to `compile_acs_data(tables = ...)`; results are computed,
  documented in the codebook, and receive margins of error
  automatically.
- **Interactive viewer**:
  [`view_acs_data()`](https://ui-research.github.io/urbnindicators/reference/view_acs_data.md)
  launches a local Shiny app to map results, interpolate to uploaded or
  hand-drawn target geographies, benchmark differences for statistical
  significance, and export data and images.
- **Caching**: `compile_acs_data(cache = TRUE)` caches raw ACS query
  results on disk (one entry per geography-year-state-table) and reuses
  them across calls and R sessions, so repeat and overlapping requests –
  including changed `tables` selections – re-download only what is
  missing. Entries never expire (published five-year ACS estimates do
  not change); a new
  [`clear_acs_cache()`](https://ui-research.github.io/urbnindicators/reference/clear_acs_cache.md)
  deletes them to reclaim disk space.
- **Universe statements in the codebook.** The codebook gains a
  `universe` column carrying the Census Bureau’s published universe
  statement for each variable (e.g., “Households”, “Population 25 years
  and over”); derived variables inherit their numerator’s universe.
  Available for the 2020+ vintages (the API does not publish universes
  for earlier years).
- **Owner cost-burden measures.** `tenure_by_housing_costs` (B25106) now
  publishes owner cost-burden percentages (all incomes, incomes below
  \$35,000, incomes below \$50,000) and an all-tenures headline measure;
  a new `owner_cost_burden` table (B25091) adds 30-percent-or-more and
  50-percent-or-more (severe) burden shares by mortgage status, plus a
  combined all-owners severe-burden measure. Denominators exclude
  households whose cost ratio is not computable (zero/negative income;
  renters paying no cash rent), consistent with the renter measures.
- Unregistered ACS tables requested by code are auto-processed: the
  label hierarchy is parsed and percentages are computed against the
  nearest parent subtotal (configurable via the `denominator`
  parameter).
- `years` defaults to the most recent ACS five-year vintage expected to
  be available, rather than a hard-coded year.

### Bug fixes and accuracy corrections

- Fixed cost-burden indicators (`cost_burdened_*`): the “50.0 percent or
  more” rent-burden category was excluded from numerators, making the
  50-percent-or-more measures zero and understating the
  30-percent-or-more measures.
- Fixed six indicators whose definitions over-matched subcategory
  columns after the definition-helper consolidation
  (`sex_female_percent` / `sex_male_percent`,
  `nativity_native_born_percent` / `nativity_foreign_born_percent`,
  `overcrowding_morethan1_ppr_renteroccupied_percent`, health-insurance
  covered-employed/unemployed shares, and commute-mode shares).
- [`generate_codebook()`](https://ui-research.github.io/urbnindicators/reference/generate_codebook.md)
  and variable resolution now honor the requested year rather than
  resolving against the 2022 dictionary unconditionally, and block-group
  availability no longer breaks for vintages newer than the installed
  `tidycensus` geography lookup.
- Margin-of-error calculations were vectorized (orders-of-magnitude
  faster on tract-level, multi-state pulls).
- The auto-table classifier now matches its skip keywords on word
  boundaries, so tables whose concept contains “Means” (e.g., B08301,
  Means of Transportation to Work) are no longer misclassified as
  non-count tables (“mean”) and receive auto-computed percentages.
- The auto-table classifier now requires a table’s `_001` label to end
  with `:` (or be exactly `Estimate!!Total`) to compute percentages, so
  dollar-valued tables like B19080 (Household Income Quintile Upper
  Limits) no longer produce nonsense percentages of one quintile limit
  over another.
- Fixed a typo in
  [`make_pretty_names()`](https://ui-research.github.io/urbnindicators/reference/make_pretty_names.md)
  output (“renter-occuiped”).

### Infrastructure

- Test fixtures are committed, so fixture-gated data-quality tests run
  on CI and fresh clones.
- Added an R CMD check workflow; CI workflows now pass `CENSUS_API_KEY`
  through to tests and site builds.

## urbnindicators 0.0.0.9401

- Code-reviewed calculation of measures of error
- Code-reviewed calculations of derived measures
- Moved a number of functions from external to internal
- Removed `calculate_segregation_metrics()`, which was not a clear fit
  within package’s scope at this time.

## urbnindicators 0.0.0.9301

- Significantly overhauled calculations of coefficients of variation

## urbnindicators 0.0.0.9300

- Adding
  [`make_pretty_names()`](https://ui-research.github.io/urbnindicators/reference/make_pretty_names.md)
- Reorganizing References
- Updating Getting Started vignette
- Removing Coefficients of Variation vignette until this functionality
  is more robust
- Updating README
- Adding
  [`generate_codebook()`](https://ui-research.github.io/urbnindicators/reference/generate_codebook.md)
  and `calculate_cvs()` (0.0.0.9100)
