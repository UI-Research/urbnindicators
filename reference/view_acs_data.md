# View ACS data in a local Shiny app

Launch a small, locally-run Shiny app that visualizes the spatial output
of
[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
as an interactive choropleth map. Colors, breaks, and popups are derived
from the codebook attribute attached to the input.

## Usage

``` r
view_acs_data(
  .data,
  geography_extent,
  variables = NULL,
  geography = NULL,
  target_geographies = NULL,
  ...
)
```

## Arguments

- .data:

  An `sf` object produced by
  [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)
  with `spatial = TRUE`. Must retain its `"codebook"` attribute.

- geography_extent:

  Required. A character vector declaring which parent geographies are
  appropriate statistical benchmarks for `.data` – any of `"county"`,
  `"state"`, `"nation"` (alias `"national"`), or `"none"` to disable
  benchmarking. The caller is responsible for this judgment: only
  declare `"state"`, say, when `.data` covers the full state(s) it
  spans. Declared levels are further filtered to those structurally
  valid for the observation geography (e.g. state-level data can only be
  benchmarked against the nation) and to those passing a basic coverage
  check: the `"nation"` benchmark requires all 51 states to be present;
  `"state"` requires more than one county per present state
  (single-county states such as DC are exempt); and `"county"` requires
  more than one tract/block group per present county. Levels failing
  these checks are dropped with a warning.

- variables:

  Optional character vector restricting which variables appear in the
  picker. Accepts either raw column names (e.g.,
  `"race_nonhispanic_white_alone_percent"`) or pretty labels as produced
  by
  [`make_pretty_names()`](https://ui-research.github.io/urbnindicators/reference/make_pretty_names.md).
  When `NULL` (default), all eligible numeric variables from the
  codebook are shown.

- geography:

  Geography level of `.data`, matching the value passed to
  [`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md).
  Used to identify the observation level for benchmarking – it
  disambiguates 5-character county GEOIDs from ZCTA/CBSA codes of the
  same length. When `NULL`, the level is inferred from GEOID length
  where unambiguous (block group, tract, state); county-level data
  requires this argument for benchmarking.

- target_geographies:

  Optional `sf` polygon object whose rows define target geographies onto
  which the source data are interpolated via
  [`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md)
  using area-weighted overlap. Must contain a `GEOID` column that
  uniquely identifies each polygon; an optional `NAME` column is used in
  popups when present. When supplied (or once polygons are drawn
  in-app), a "Geography" toggle appears in the sidebar that switches the
  map between the source and the target dataset. Benchmark values remain
  computed from the source data; in Target view, each target polygon is
  mapped to a parent (county / state / national) by majority-area
  overlap.

- ...:

  Forwarded to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.,
  `port`, `host`, `launch.browser`).

## Value

A `shiny.appobj` (invisibly). When called from an interactive session,
the app is launched in the default browser.

## Details

The map uses MapLibre via the mapgl package and the CARTO Positron
basemap (no API token required). Color palettes default to those in the
[urbnthemes](https://github.com/UrbanInstitute/urbnthemes) package
(sequential cyan for counts/medians/averages, quintile cyan for
percentages, diverging cyan/orange for index variables).

A variable picker is shown in the sidebar; if `.data` spans multiple
years a year picker is also shown. Below the picker an interactive
histogram of the selected variable is rendered, and users can drag a
horizontal selection on it to restrict the choropleth's color scale.
Polygons whose value falls outside the selected range render in neutral
grey, and their popups note the out-of-range status. The brushable range
defaults to `c(0, 1)` for percent variables and to the observed data
range for everything else.

A "Statistical benchmark" dropdown appears when `geography_extent`
declares one or more parent geographies that are valid comparisons for
`.data` and the data pass a basic coverage check. It recolors the map by
whether each polygon's estimate is statistically significantly larger
than, smaller than, or indistinguishable from the chosen
higher-geography benchmark. Block-group and tract data can be
benchmarked against their county or state, county data against its
state, and state data against the nation. Benchmark values are
aggregated from `.data` via
[`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md)
(so the result may differ slightly from a separately-pulled ACS estimate
at that geography), and significance is tested via
[`tidycensus::significance()`](https://walker-data.com/tidycensus/reference/significance.html)
at the 90 percent confidence level (matching the *quantified survey
error* vignette). Variables without a margin-of-error column fall back
to the default coloring.

Regardless of `target_geographies`, the map carries a drawing toolbar at
its top-left with "Point", "Line", and "Polygon" tools. Each drawn
feature gets a text field in the sidebar where it can be named; the
names appear in popups and the exported data. Draw one or more polygons
and click "Interpolate to drawn area" in the sidebar to interpolate the
source data onto them via
[`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md);
the map then switches to a "Target" view of the result, and benchmarking
(if available) compares each drawn polygon to the parent geography it
mostly overlaps. "Clear target" discards the drawn polygons, reverting
to `target_geographies` when one was supplied and otherwise to the
source view.

Interpolation is only accurate where a target polygon is fully covered
by the source geographies. Any drawn polygon (or supplied
`target_geographies` row) that extends beyond the source extent cannot
be accurately interpolated, so its values are set to `NA` (rendered in
neutral grey) and a warning is shown in the app.

A "Download" section in the sidebar offers two downloads. "Download
data" opens a dialog to pick a file format – CSV, GeoPackage (`.gpkg`),
GeoJSON, GeoParquet (`.parquet`), or non-spatial Parquet (`.parquet`) –
and, when an interpolated (target) dataset exists, whether to export the
interpolated geographies only or those plus the source geographies
(distinguished by a `geography_type` column). The spatial formats retain
geometry (and, in Target view, the user-supplied names of the drawn
geographies); the export includes each variable's margin of error
(`<var>_M` columns) and, when a statistical benchmark is selected, the
benchmark value, its margin of error, and the significance category for
the selected variable. GeoParquet requires the `sfarrow` package and
non-spatial Parquet requires `arrow`; if a chosen format's packages
aren't installed, the data is written to CSV instead. "Download figure"
opens a dialog for choosing an export resolution (1x-4x the on-screen
size), then saves the live map exactly as displayed – basemap, active
layer, current zoom and pan, and any drawn areas – as a `.png`. The
capture is performed in the browser, so it works only in an interactive
(locally-run) session.

The sidebar is organized into collapsible sections – "Data" (variable,
year, statistical benchmark, and the data-distribution histogram),
"Interpolate" (the source/target toggle and custom-geography
drawing/naming), "Visual parameters", and "Download" – with only "Data"
open at launch. The "Interpolate" section auto-expands whenever a
polygon is drawn.

The "Visual parameters" section exposes display controls that don't
change the data: a polygon-opacity slider, a legend-title override
(blank uses the variable's pretty name), a basemap picker (CARTO
Positron / Dark Matter / Voyager plus tokenless ESRI satellite and
OpenTopoMap topographic rasters), and checkboxes that toggle a scale bar
and a cardinal-direction compass (with N/E/S/W labels and an "N" over
the north arrow; click it to reset north). A layers toggle at the map's
bottom-right separately shows or hides the base-geography choropleth and
any drawn features.

`view_acs_data()` requires the `shiny`, `mapgl`, `bslib`, and `ggplot2`
packages. These are listed in `Suggests` and checked at runtime.

## See also

[`compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md),
[`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md),
[`tidycensus::significance()`](https://walker-data.com/tidycensus/reference/significance.html)

## Examples

``` r
if (FALSE) { # \dontrun{
df = compile_acs_data(
  tables    = c("race", "snap"),
  years     = 2022,
  geography = "tract",
  states    = "NJ",
  spatial   = TRUE)

## `df` is a full-state NJ tract pull, so county and state are valid benchmarks.
view_acs_data(df, geography = "tract", geography_extent = c("county", "state"))

## Restrict the picker to a few variables:
view_acs_data(df, geography = "tract", geography_extent = c("county", "state"),
              variables = c("snap_received_percent",
                            "race_nonhispanic_white_alone_percent"))

## Disable benchmarking (e.g. when the data are only a partial extent):
view_acs_data(df, geography = "tract", geography_extent = "none")

## Interpolate onto custom target polygons (e.g., neighborhoods):
neighborhoods = sf::st_read("path/to/neighborhoods.geojson")
# neighborhoods must have a `GEOID` column identifying each polygon.
view_acs_data(df, geography = "tract", geography_extent = c("county", "state"),
              target_geographies = neighborhoods)
} # }
```
