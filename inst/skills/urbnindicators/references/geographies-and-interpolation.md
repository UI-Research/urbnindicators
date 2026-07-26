# Geographies, block groups, and custom areas

## Supported geographies

Five-year ACS estimates only, **2009 and later**. `geography` accepts any value
`tidycensus::get_acs()` accepts; `geography` defaults to `"county"`.

| `geography` | `states` | Notes |
|---|---|---|
| `"us"`, `"region"`, `"division"`, `"cbsa"`, `"zcta"`, `"urban area"` | ignored | returned nationally |
| `"state"` | optional (filters) | states and DC |
| `"county"` (default), `"place"`, `"county subdivision"` | **recommended** | omitting it pulls every state — slow |
| `"tract"` | **required in practice** | not enforced — omitting it silently pulls every state |
| `"block group"` | **required (enforced)** | aborts without it; 2013+ only; limited table subset; large MOEs |

Census **blocks are not supported** — the ACS publishes no block-level data.

For `"county"`, `"county subdivision"`, `"tract"`, and `"block group"` you can
pass `counties` (five-digit FIPS codes) instead of `states`; `counties`
overrides `states`.
More than 5 counties triggers a warning — county-by-county queries are slow, so
prefer pulling the state and filtering afterward. `counties` is ignored on the
national "super-state" geographies in the first row.

## The 2020 boundary break

The Census Bureau reconfigured tract and block-group boundaries in 2020.
**Tract- and block-group-level statistics cannot validly be compared across
that line.** Requesting years spanning 2020 at these geographies emits a
warning; do not silently proceed to a trend claim. Crosswalks are available from
NHGIS or via `renv::install("UI-Research/crosswalk")`.

## Block groups

- Years **2013+** only.
- `states` is **required**; national block-group pulls are disallowed.
- The ACS publishes only a subset of detailed tables at this geography.
  Unavailable tables are dropped with a warning **before** the query runs;
  partial tables lose their unavailable variables. The call errors only if none
  of the requested tables are available.
- Check availability first — it is year-specific:

  ```r
  list_tables(geography = "block group", year = 2023)
  ```

- Queries iterate county-by-county within each state, so they are slow. Use
  `cache = TRUE`.
- **Block-group MOEs are large.** Always report them, and state the limitation
  whenever presenting block-group estimates.

## interpolate_acs(): aggregating to custom geographies

```r
interpolate_acs(.data, target_geoid_column, weight = NULL, crosswalk = NULL,
                source_geoid = "GEOID",
                weight_variable = "total_population_universe")
```

`.data` must be output of `compile_acs_data()` **with its `codebook` attribute
intact** — the function aborts without it. Counts are summed, percentages are
recalculated from summed components (not averaged), and intensive variables
(medians, averages) become population-weighted averages. MOEs propagate using
Census approximation formulas.

The `target_geoid_column` is **renamed to `GEOID`** in the result.

### Mode 1 — complete nesting (`weight = NULL`)

Each source geography belongs entirely to one target. Add the target column to
the data, or supply a crosswalk:

```r
tract_data$neighborhood = assign_neighborhoods(tract_data$GEOID)
nbhd = interpolate_acs(tract_data, target_geoid_column = "neighborhood")
```

### Mode 2 — fractional allocation (`weight = "column"`)

Source geographies split across multiple targets. The crosswalk needs the
`source_geoid` column, the `target_geoid_column`, and the weight column; weights
should sum to ~1 per source geography.

```r
crosswalk = data.frame(
  GEOID        = c("11001000100", "11001000100", "11001000201"),
  neighborhood = c("Downtown",    "Chinatown",   "Downtown"),
  alloc_weight = c(0.6,           0.4,           1.0))

nbhd = interpolate_acs(tract_data, target_geoid_column = "neighborhood",
                       weight = "alloc_weight", crosswalk = crosswalk)
```

In this mode the area variables (`area_land_sq_kilometer`,
`area_water_sq_kilometer`, `area_land_water_sq_kilometer`) and
`population_density_land_sq_kilometer` (with its MOE) are set to `NA` — weights
represent population shares, not area shares. Join your own area measures if you
need density for the targets.

### Geometry is dropped

**`interpolate_acs()` always returns a non-spatial data frame.** Geometry is
dropped even when the input was `sf`. To map the result, dissolve the source
geometry separately and re-join:

```r
nbhd_geometry = tract_data %>%
  group_by(neighborhood) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

nbhd_sf = nbhd_geometry %>%
  rename(GEOID = neighborhood) %>%
  left_join(nbhd, by = "GEOID")
```

Note that `sf::st_transform()` drops the `codebook` attribute — transform after
interpolating, or re-attach the attribute.

### NA propagation

`NA` in any source geography propagates to its target for that column, with a
one-time warning listing affected columns. Filter or impute upstream if you want
different behavior.

See `vignette("custom-geographies")` for a full worked example.
