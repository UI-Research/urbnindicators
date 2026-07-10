# Release decisions — handoff notes (updated 2026-07-04, second session)

State when written: PRs #89–#98 merged; version 0.1.0 (no tag yet). Items 1–5
below were decided on 2026-07-04 and implemented on branch
`release-prep-0.1.0`; item 6 remains open.

## Still needs the user (blocked for the agent by permissions)

1. ~~Add the Census API key as a repository secret~~ **DONE 2026-07-04**
   (set by the user; pkgdown re-run triggered on PR #99).

2. ~~Kill the overnight caffeinate~~ **DONE 2026-07-04**.

## Decisions made (2026-07-04) — implemented on `release-prep-0.1.0`

1. **`$` in column names — DECIDED: strip.** `clean_acs_names()` gained a
   `"\\$" = ""` rule. Verified against the 2023 codebook: 3,175 names change
   (exactly those with `$` in labels), no new duplicate names, all nine
   cost_burden regexes match identical column sets before/after.
2. **Misleading names — DECIDED: rename both.**
   `employment_civilian_labor_force_percent` →
   `employment_civilian_labor_force_employed_percent`;
   `median_household_income_nhpi_` → `median_household_income_nhpi_alone_`.
3. **`list_acs_variables()` stub — DECIDED: deleted** (function, test,
   man page, pkgdown entry, NAMESPACE export).
4. **`se_*`/`cv` family — DECIDED: keep internal** for 0.1.0; exporting later
   is non-breaking.
5. **R-CMD-check — tightened** to `error-on: '"warning"'`; watch the PR's CI
   run for anything that surfaces.

All three breaking changes are recorded in NEWS.md under 0.1.0. Fixtures
(`test_data_2026-02-08.rds`, `codebook_2026-02-08.rds`) were regenerated; the
block-group fixture was untouched (its tables have no `$` labels and none of
the renamed variables).

## Still open

(nothing — all handoff items are resolved)

6. ~~`tenure_by_housing_costs` (B25106) indicators~~ **DECIDED &
   IMPLEMENTED 2026-07-04** (PR #100, stacked on #99): owner 30%+ shares
   (all incomes, <$35k, <$50k) + all-tenures headline from B25106; new
   `owner_cost_burden` table (B25091) with 30%+/50%+ by mortgage status and
   combined owner severe burden; zero/negative-income, no-cash-rent, and
   not-computed households excluded from denominators; existing renter
   measures renamed `cost_burdened_renter_*` (breaking, pre-tag). PR #100
   also adds a codebook `universe` column (Census universe statements,
   2020+ vintages).

## Housekeeping

- Three stashes exist (the original handoff said two): `stash@{0}` ("WIP on
  pretty-names"), `stash@{1}` ("WIP on codebook"), `stash@{2}` ("WIP on cov").
  Review or drop.
- No 0.1.0 git tag yet; tag once `release-prep-0.1.0` merges (it contains the
  breaking changes).
- renv reports the project out-of-sync (renv 1.1.8 loaded vs 1.2.3 in
  lockfile) — pre-existing, not touched.
