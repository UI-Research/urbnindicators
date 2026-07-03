# urbnindicators 0.1.0

First tagged release. This version consolidates a major overhaul of the
package's API and internals.

## Breaking changes

* `compile_acs_data()` is now driven by a **`tables` parameter**: request
  registered tables by name (see `list_tables()`), raw ACS table codes (e.g.,
  `"B25070"`, auto-processed at runtime), and/or custom variable definitions —
  mixed freely inside a `list()`. The old behavior (all tables) remains the
  default when `tables` is omitted. The `variables` parameter is deprecated
  and ignored.
* The definition helpers were consolidated: `define_percent()` (with
  `each = TRUE` for batch mode) replaces `define_across_percent()`;
  `define_sum()` replaces `define_across_sum()`; `define_complement()`
  replaces `define_one_minus()`. Single-string numerators/denominators are
  interpreted as regular expressions.
* `select_variables_by_name()` and `filter_variables()` are no longer
  exported; use `get_acs_codebook()` for variable discovery.
* `list_acs_variables()` is deprecated; use `list_variables()`.
* A **Census API key is now required** for all functions that read the ACS
  variable dictionary (including `list_variables()` and
  `get_acs_codebook()`), because the Census Bureau's variables metadata
  endpoint now requires a key. Set one with
  `tidycensus::census_api_key("YOUR_KEY", install = TRUE)`.
* `safe_divide()` now returns `NA` (not `0`) when a nonzero numerator is
  divided by zero; `0/0` still returns `0`.

## New features

* **Block-group geography**: `compile_acs_data(geography = "block group")`
  is supported for 2013 and later (explicit `states` required). Tables the
  ACS does not publish at the block-group level are dropped with a warning;
  `list_tables(geography = "block group")` lists what is available.
* **Custom geographies**: `interpolate_acs()` aggregates (complete nesting)
  or interpolates (fractional allocation via crosswalk weights) estimates to
  user-defined geographies, recalculating percentages and propagating
  margins of error with Census Bureau approximation formulas.
* **Custom derived variables**: pass `define_percent()` / `define_sum()` /
  `define_complement()` / `define_metadata()` objects to
  `compile_acs_data(tables = ...)`; results are computed, documented in the
  codebook, and receive margins of error automatically.
* **Interactive viewer**: `view_acs_data()` launches a local Shiny app to
  map results, interpolate to uploaded or hand-drawn target geographies,
  benchmark differences for statistical significance, and export data and
  images.
* Unregistered ACS tables requested by code are auto-processed: the label
  hierarchy is parsed and percentages are computed against the nearest
  parent subtotal (configurable via the `denominator` parameter).
* `years` defaults to the most recent ACS five-year vintage expected to be
  available, rather than a hard-coded year.

## Bug fixes and accuracy corrections

* Fixed cost-burden indicators (`cost_burdened_*`): the "50.0 percent or
  more" rent-burden category was excluded from numerators, making the
  50-percent-or-more measures zero and understating the 30-percent-or-more
  measures.
* Fixed six indicators whose definitions over-matched subcategory columns
  after the definition-helper consolidation (`sex_female_percent` /
  `sex_male_percent`, `nativity_native_born_percent` /
  `nativity_foreign_born_percent`,
  `overcrowding_morethan1_ppr_renteroccupied_percent`, health-insurance
  covered-employed/unemployed shares, and commute-mode shares).
* `generate_codebook()` and variable resolution now honor the requested
  year rather than resolving against the 2022 dictionary unconditionally,
  and block-group availability no longer breaks for vintages newer than the
  installed `tidycensus` geography lookup.
* Margin-of-error calculations were vectorized (orders-of-magnitude faster
  on tract-level, multi-state pulls).
* Fixed a typo in `make_pretty_names()` output ("renter-occuiped").

## Infrastructure

* Test fixtures are committed, so fixture-gated data-quality tests run on
  CI and fresh clones.
* Added an R CMD check workflow; CI workflows now pass `CENSUS_API_KEY`
  through to tests and site builds.

# urbnindicators 0.0.0.9401

* Code-reviewed calculation of measures of error
* Code-reviewed calculations of derived measures
* Moved a number of functions from external to internal
* Removed `calculate_segregation_metrics()`, which was not a clear fit within
  package's scope at this time.

# urbnindicators 0.0.0.9301

* Significantly overhauled calculations of coefficients of variation

# urbnindicators 0.0.0.9300

* Adding `make_pretty_names()`
* Reorganizing References
* Updating Getting Started vignette
* Removing Coefficients of Variation vignette until this functionality is more robust
* Updating README
* Adding `generate_codebook()` and `calculate_cvs()` (0.0.0.9100)
