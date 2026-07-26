# Explore ACS Data Interactively

``` r

library(urbnindicators)
```

[`view_acs_data()`](https://ui-research.github.io/urbnindicators/reference/view_acs_data.md)
opens an interactive dashboard for exploring an ACS dataset: a
choropleth map, a searchable table of measures, on-the-fly benchmarking
against parent geographies, and tools for interpolating to custom areas.

The short demo below walks through the interface before we build one
ourselves.

Your browser does not support embedded video. [Download the
demo](https://ui-research.github.io/urbnindicators/view_acs_data-demo.mp4).

``` r

df1 = compile_acs_data(
  tables = "cost_burden",
  states = "CA",
  counties = "06037",
  years = "2024",
  geography = "tract", 
  spatial = TRUE)
#> ℹ Margins of error for derived variables (suffixed `_M`) are approximations
#>   that follow Census Bureau guidance and are an experimental feature; interpret
#>   them with care.
#> This message is displayed once per session.

places1 = tigris::places(state = "NJ", cb = TRUE, year = 2024) %>%
  sf::st_filter(df1)
#>   |                                                                              |                                                                      |   0%  |                                                                              |===                                                                   |   4%  |                                                                              |======                                                                |   9%  |                                                                              |==========                                                            |  14%  |                                                                              |==============                                                        |  20%  |                                                                              |==================                                                    |  25%  |                                                                              |=============================                                         |  41%  |                                                                              |==========================================                            |  60%  |                                                                              |=====================================================                 |  76%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |====================================================================  |  97%  |                                                                              |======================================================================| 100%
```

The `geography_extent` argument is required: it declares which higher
geographies are valid *statistical benchmarks* for these data. Because
`df1` was pulled for a single county (`counties = "06037"`, Los Angeles
County), only a **county** benchmark is appropriate – the tracts here
don’t cover all of California, so a state benchmark would compare each
tract to a partial, misleading “state” value. Had we pulled the full
state (`counties = NULL`), we could declare
`geography_extent = c("county", "state")`. Pass
`geography_extent = "none"` to turn benchmarking off entirely.

``` r

view_acs_data(df1, geography = "tract", geography_extent = "county")
```

## Custom target geographies

[`view_acs_data()`](https://ui-research.github.io/urbnindicators/reference/view_acs_data.md)
can also interpolate the source data onto a user-supplied polygon set
(e.g., neighborhoods, council districts, school catchments) via
area-weighted overlap. Pass an `sf` object to `target_geographies`; it
must contain a `GEOID` column that uniquely identifies each target
polygon. A “Geography” toggle then appears in the sidebar so you can
switch between the source view and the interpolated target view.

Benchmark values continue to be computed from the source data. In Target
view, each target polygon is mapped to a parent (county / state /
national) by majority-area overlap of its source-geography components.

Any `sf` polygon layer with a unique `GEOID` works. Here we use Census
“places” (incorporated cities and towns) as the target geographies; an
optional `NAME` column, when present, is shown in the map popups:

``` r

view_acs_data(
  df1,
  geography          = "tract",
  geography_extent   = "county",
  target_geographies = places1)
```

Under the hood, this builds a fractional crosswalk via
[`sf::st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
and feeds it to
\[[`interpolate_acs()`](https://ui-research.github.io/urbnindicators/reference/interpolate_acs.md)\].

## Draw your own geographies

You don’t need a target layer in hand. Every map includes a drawing
toolbar at its top-left: draw one or more polygons, then click
**Interpolate to drawn area** in the sidebar. The source data are
interpolated onto your polygons with the same area-weighted crosswalk,
the map switches to **Target** view, and benchmarking (when available)
compares each drawn area to the parent geography it mostly overlaps.
**Clear target** discards the polygons and returns you to the source
view (or to a `target_geographies` layer, if you supplied one).
