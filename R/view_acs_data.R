#' @title View ACS data in a local Shiny app
#' @description Launch a small, locally-run Shiny app that visualizes the spatial
#' output of [compile_acs_data()] as an interactive choropleth map. Colors,
#' breaks, and popups are derived from the codebook attribute attached to the
#' input.
#' @details
#' The map uses MapLibre via the mapgl package and the CARTO Positron basemap
#' (no API token required). Color palettes default to those in the
#' \href{https://github.com/UrbanInstitute/urbnthemes}{urbnthemes} package
#' (sequential cyan for counts/medians/averages, quintile cyan for percentages,
#' diverging cyan/orange for index variables).
#'
#' A variable picker is shown in the sidebar; if `.data` spans multiple years a
#' year picker is also shown. Below the picker an interactive histogram of the
#' selected variable is rendered, and users can drag a horizontal selection on
#' it to restrict the choropleth's color scale. Polygons whose value falls
#' outside the selected range render in neutral grey, and their popups note the
#' out-of-range status. The brushable range defaults to `c(0, 1)` for percent
#' variables and to the observed data range for everything else.
#'
#' When `geography` is one of `"tract"`, `"county"`, or `"state"`, a
#' "Statistical benchmark" dropdown appears that recolors the map by whether
#' each polygon's estimate is statistically significantly larger than, smaller
#' than, or indistinguishable from a higher-geography benchmark. Benchmark
#' values are aggregated from `.data` via [interpolate_acs()] (so the result
#' may differ slightly from a separately-pulled ACS estimate at that geography),
#' and significance is tested via [tidycensus::significance()] at the
#' 90 percent confidence level (matching the *quantified survey error*
#' vignette). Variables without a margin-of-error column fall back to the
#' default coloring.
#'
#' Regardless of `target_geographies`, the map carries a drawing toolbar at its
#' top-left. Draw one or more polygons and click "Interpolate to drawn area" in
#' the sidebar to interpolate the source data onto them via [interpolate_acs()];
#' the map then switches to a "Target" view of the result, and benchmarking (if
#' available) compares each drawn polygon to the parent geography it mostly
#' overlaps. "Clear target" discards the drawn polygons, reverting to
#' `target_geographies` when one was supplied and otherwise to the source view.
#'
#' Interpolation is only accurate where a target polygon is fully covered by the
#' source geographies. Any drawn polygon (or supplied `target_geographies` row)
#' that extends beyond the source extent cannot be accurately interpolated, so
#' its values are set to `NA` (rendered in neutral grey) and a warning is shown
#' in the app.
#'
#' An "Export" section in the sidebar offers two downloads. "Download data"
#' writes the currently-visualized data to CSV, including each variable's margin
#' of error (`<var>_M` columns) and, when a statistical benchmark is selected,
#' the benchmark value, its margin of error, and the significance category for
#' the selected variable. "Download figure" opens a dialog for choosing an
#' export resolution (1x-4x the on-screen size), then saves the live map exactly
#' as displayed — basemap, active layer, current zoom and pan, and any drawn
#' areas — as a `.png`. The capture is performed in the browser, so it works
#' only in an interactive (locally-run) session.
#'
#' `view_acs_data()` requires the `shiny`, `mapgl`, `bslib`, and `ggplot2`
#' packages. These are listed in `Suggests` and checked at runtime.
#' @param .data An `sf` object produced by [compile_acs_data()] with
#'   `spatial = TRUE`. Must retain its `"codebook"` attribute.
#' @param variables Optional character vector restricting which variables appear
#'   in the picker. Accepts either raw column names (e.g.,
#'   `"race_nonhispanic_white_alone_percent"`) or pretty labels as produced by
#'   [make_pretty_names()]. When `NULL` (default), all eligible numeric
#'   variables from the codebook are shown.
#' @param geography Geography level of `.data`, matching the value passed to
#'   [compile_acs_data()]. When `"tract"`, `"county"`, or `"state"`, the
#'   "Statistical benchmark" dropdown is shown with appropriate higher-level
#'   geographies. Any other value (or `NULL`) hides the dropdown.
#' @param target_geographies Optional `sf` polygon object whose rows define
#'   target geographies onto which the source data are interpolated via
#'   [interpolate_acs()] using area-weighted overlap. Must contain a `GEOID`
#'   column that uniquely identifies each polygon; an optional `NAME` column
#'   is used in popups when present. When supplied (or once polygons are drawn
#'   in-app), a "Geography" toggle appears in the sidebar that switches the map
#'   between the source and the target dataset. Benchmark values remain
#'   computed from the source data;
#'   in Target view, each target polygon is mapped to a parent (county /
#'   state / national) by majority-area overlap.
#' @param ... Forwarded to [shiny::runApp()] (e.g., `port`, `host`,
#'   `launch.browser`).
#' @returns A `shiny.appobj` (invisibly). When called from an interactive
#'   session, the app is launched in the default browser.
#' @examples
#' \dontrun{
#' df = compile_acs_data(
#'   tables    = c("race", "snap"),
#'   years     = 2022,
#'   geography = "tract",
#'   states    = "NJ",
#'   spatial   = TRUE)
#'
#' view_acs_data(df, geography = "tract")
#'
#' ## Restrict the picker to a few variables:
#' view_acs_data(df, geography = "tract",
#'               variables = c("snap_received_percent",
#'                             "race_nonhispanic_white_alone_percent"))
#'
#' ## Interpolate onto custom target polygons (e.g., neighborhoods):
#' neighborhoods = sf::st_read("path/to/neighborhoods.geojson")
#' # neighborhoods must have a `GEOID` column identifying each polygon.
#' view_acs_data(df, geography = "tract",
#'               target_geographies = neighborhoods)
#' }
#' @seealso [compile_acs_data()], [interpolate_acs()],
#'   [tidycensus::significance()]
#' @export
view_acs_data = function(.data, variables = NULL, geography = NULL,
                         target_geographies = NULL, ...) {
  rlang::check_installed(
    c("shiny", "mapgl", "bslib", "ggplot2"),
    reason = "to launch the interactive ACS viewer.")

  if (!inherits(.data, "sf")) {
    cli::cli_abort(c(
      "{.arg .data} must be an {.cls sf} object.",
      "i" = "Pass {.code spatial = TRUE} to {.fn compile_acs_data} to get one."))
  }

  if (nrow(.data) == 0) {
    cli::cli_abort("{.arg .data} has zero rows; nothing to map.")
  }

  codebook = attr(.data, "codebook")
  if (is.null(codebook)) {
    cli::cli_abort(c(
      "{.arg .data} is missing its {.code codebook} attribute.",
      "i" = paste("This attribute is set by {.fn compile_acs_data}.",
                  "Use dplyr verbs (which preserve attributes) when subsetting,",
                  "or pass the original output of {.fn compile_acs_data} directly.")))
  }

  choices1 = build_variable_choices(.data, codebook, variables)
  if (length(choices1) == 0) {
    cli::cli_abort(c(
      "No mappable variables were found.",
      "i" = "Check {.arg variables} or the codebook attached to {.arg .data}."))
  }

  geography = if (is.null(geography)) NA_character_ else as.character(geography)
  benchmark_levels = benchmark_levels_for_geography(geography)

  data1 = sf::st_transform(.data, 4326)
  has_multi_year =
    "data_source_year" %in% colnames(data1) &&
    length(unique(data1$data_source_year)) > 1

  target_prep1 = prepare_target_dataset(.data, target_geographies, benchmark_levels)
  target_data1 = target_prep1$data
  target_parent_lookup = target_prep1$parent_lookup
  target_incomplete = target_prep1$incomplete
  target_codebook = if (is.null(target_data1)) NULL else attr(target_data1, "codebook")

  app1 = shiny::shinyApp(
    ui     = .view_acs_ui(choices1, has_multi_year, data1, benchmark_levels),
    server = .view_acs_server(data1, codebook, choices1, has_multi_year,
                              geography, benchmark_levels,
                              target_data1, target_codebook,
                              target_parent_lookup, target_incomplete))

  if (interactive()) {
    shiny::runApp(app1, ...)
  } else {
    invisible(app1)
  }
}

## Eligible variables = codebook rows whose calculated_variable exists in the
## data, whose variable_type is one we know how to map, and which are not MOE
## columns. Returns a named character vector: names are pretty labels, values
## are raw column names.
build_variable_choices = function(.data, codebook, variables) {
  eligible_types = c("Count", "Percent", "Sum", "Median",
                     "Median ($)", "Average", "Index")
  data_cols = colnames(.data)

  candidates1 = codebook %>%
    dplyr::filter(
      .data$calculated_variable %in% data_cols,
      .data$variable_type %in% eligible_types,
      !stringr::str_detect(.data$calculated_variable, "_M$")) %>%
    dplyr::distinct(.data$calculated_variable, .keep_all = TRUE)

  if (!is.null(variables)) {
    pretty_lookup = stats::setNames(
      candidates1$calculated_variable,
      make_pretty_names(candidates1$calculated_variable))
    requested1 = unique(c(
      intersect(variables, candidates1$calculated_variable),
      unname(pretty_lookup[variables])))
    requested1 = requested1[!is.na(requested1)]
    candidates1 = candidates1 %>%
      dplyr::filter(.data$calculated_variable %in% requested1)
  }

  if (nrow(candidates1) == 0) return(stats::setNames(character(0), character(0)))

  raw_names = candidates1$calculated_variable
  pretty1 = make_pretty_names(raw_names)
  ord1 = order(pretty1)
  stats::setNames(raw_names[ord1], pretty1[ord1])
}

## urbnthemes-derived palettes, embedded so urbnthemes is not a runtime dep.
## See urbnthemes::palette_urbn_cyan / _quintile / _diverging.
.urbn_palettes = list(
  cyan      = c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb",
                "#1696d2", "#12719e", "#0a4c6a", "#062635"),
  quintile  = c("#cfe8f3", "#73bfe2", "#1696d2", "#0a4c6a", "#000000"),
  diverging = c("#ca5800", "#fdbf11", "#fdd870", "#fff2cf",
                "#cfe8f3", "#73bfe2", "#1696d2", "#0a4c6a"))

## Grey used for out-of-range polygons (urbnthemes' "gray" main color).
.out_of_range_color = "#d2d2d2"

## A target polygon must be covered by at least this share of source geographies
## to be considered accurately interpolatable. Targets below this threshold
## (including those with no overlap at all) extend beyond the source data extent;
## their interpolated values are set to NA. See build_spatial_crosswalk() and
## interpolate_to_targets().
.target_coverage_threshold = 0.99

## Three-color palette for significance comparisons (matches the
## quantified-survey-error vignette).
.benchmark_colors = c(
  "Larger"          = "#1696d2",  # urbn cyan
  "Smaller"         = "#fdbf11",  # urbn yellow
  "Not significant" = "#d2d2d2")  # urbn gray

## Pick a palette + display formatter from a codebook variable_type.
palette_for_type = function(variable_type) {
  if (is.na(variable_type)) variable_type = "Count"
  switch(variable_type,
    "Percent"     = list(palette = .urbn_palettes$quintile,  fmt = "percent"),
    "Index"       = list(palette = .urbn_palettes$diverging, fmt = "comma"),
    "Median ($)"  = list(palette = .urbn_palettes$cyan,      fmt = "dollar"),
    "Quintile ($)"= list(palette = .urbn_palettes$cyan,      fmt = "dollar"),
                    list(palette = .urbn_palettes$cyan,      fmt = "comma"))
}

format_value = function(x, fmt, accuracy = 0.1) {
  if (length(x) == 0 || all(is.na(x))) return(rep(NA_character_, length(x)))
  if (requireNamespace("scales", quietly = TRUE)) {
    f1 = switch(fmt,
      percent = scales::label_percent(accuracy = accuracy),
      dollar  = scales::label_dollar(accuracy = max(accuracy, 1), big.mark = ","),
                scales::label_comma(accuracy = accuracy))
    return(f1(x))
  }
  digits1 = if (accuracy >= 1) 0 else max(0, -floor(log10(accuracy)))
  switch(fmt,
    percent = paste0(formatC(100 * x, format = "f", digits = digits1), "%"),
    dollar  = paste0("$", formatC(x, format = "f", big.mark = ",", digits = 0)),
              formatC(x, format = "f", big.mark = ",", digits = digits1))
}

## Default brush range for a variable: percent variables default to c(0, 1);
## everything else defaults to the observed finite-value range.
default_range_for_variable = function(values, variable_type) {
  if (!is.na(variable_type) && variable_type == "Percent") {
    return(c(0, 1))
  }
  values1 = values[is.finite(values)]
  if (length(values1) == 0) return(c(0, 1))
  rng1 = range(values1)
  if (rng1[1] == rng1[2]) rng1[2] = rng1[1] + 1e-9
  rng1
}

## Available benchmark levels for a given source geography. Returns a named
## character vector usable directly as `choices` for selectInput, with "None"
## first. Empty (zero-length) when no benchmarking is supported.
benchmark_levels_for_geography = function(geography) {
  if (is.null(geography) || length(geography) == 0 || is.na(geography)) {
    return(character(0))
  }
  opts1 = switch(tolower(geography),
    "tract"  = c("None" = "none", "County"   = "county", "State" = "state"),
    "county" = c("None" = "none", "State"    = "state"),
    "state"  = c("None" = "none", "National" = "national"),
    character(0))
  opts1
}

## Substring length of a parent GEOID at a given target level (relative to
## tract-/county-/state-coded GEOIDs). Returns NA for unsupported pairs.
parent_geoid_length_for_level = function(target_level) {
  switch(tolower(target_level),
    "county"   = 5L,
    "state"    = 2L,
    "national" = NA_integer_,
    NA_integer_)
}

## Build the parent-GEOID column for a benchmark target. For "national",
## every row is assigned the constant "US". When `lookup` is supplied (used
## in target-geography view), it is a tibble with columns GEOID, level, and
## parent_geoid; rows for the given level are joined to `geoids` to derive
## the parent. When `lookup` is NULL, falls back to the GEOID-prefix walk
## (the source-data behavior).
assign_parent_geoids = function(geoids, target_level, lookup = NULL) {
  if (is.null(geoids) || length(geoids) == 0) return(character(0))
  if (!is.null(lookup)) {
    sub1 = lookup[lookup[["level"]] == tolower(target_level), , drop = FALSE]
    return(sub1[["parent_geoid"]][match(as.character(geoids), sub1[["GEOID"]])])
  }
  if (tolower(target_level) == "national") {
    return(rep("US", length(geoids)))
  }
  len1 = parent_geoid_length_for_level(target_level)
  if (is.na(len1)) return(rep(NA_character_, length(geoids)))
  stringr::str_sub(as.character(geoids), 1L, len1)
}

## Build a fractional crosswalk between source and target polygons by area
## overlay. Returns a tibble with columns `source_geoid`, `target_geoid`,
## `intersection_area` (in m^2), and `share` (intersection area divided by
## source-polygon area). Shares sum to <= 1 per source; values < 1 indicate
## the source polygon is partially outside all targets.
##
## Both inputs are reprojected to EPSG:5070 (Albers Equal Area, CONUS) for
## area-accurate computation. The target_geographies object must carry a
## `GEOID` column. The source object's GEOID column is assumed to be `GEOID`.
build_spatial_crosswalk = function(source_sf, target_sf,
                                   source_id = "GEOID",
                                   target_id = "GEOID") {
  if (!inherits(source_sf, "sf")) {
    cli::cli_abort("{.arg source_sf} must be an {.cls sf} object.")
  }
  if (!inherits(target_sf, "sf")) {
    cli::cli_abort("{.arg target_sf} must be an {.cls sf} object.")
  }
  if (!source_id %in% colnames(source_sf)) {
    cli::cli_abort("Column {.var {source_id}} not found in {.arg source_sf}.")
  }
  if (!target_id %in% colnames(target_sf)) {
    cli::cli_abort("Column {.var {target_id}} not found in {.arg target_sf}.")
  }

  ## Rename to internal names up-front so source_id == target_id == "GEOID"
  ## doesn't produce a collision in st_intersection().
  src1 = source_sf %>%
    sf::st_as_sf() %>%
    dplyr::distinct(!!rlang::sym(source_id), .keep_all = TRUE) %>%
    dplyr::transmute(source_geoid = !!rlang::sym(source_id)) %>%
    sf::st_transform(5070)
  tgt1 = target_sf %>%
    sf::st_as_sf() %>%
    dplyr::distinct(!!rlang::sym(target_id), .keep_all = TRUE) %>%
    dplyr::transmute(target_geoid = !!rlang::sym(target_id)) %>%
    sf::st_transform(5070)

  source_areas = tibble::tibble(
    source_geoid = sf::st_drop_geometry(src1)[["source_geoid"]],
    source_area  = as.numeric(sf::st_area(src1)))

  inter1 = suppressWarnings(sf::st_intersection(src1, tgt1))
  if (nrow(inter1) == 0) {
    cli::cli_abort(c(
      "No spatial overlap between source and target geographies.",
      "i" = "Check that the CRS and extent of {.arg target_geographies} matches the source data."))
  }

  inter_areas = as.numeric(sf::st_area(inter1))
  inter_df = sf::st_drop_geometry(inter1) %>%
    dplyr::mutate(intersection_area = inter_areas)

  out1 = inter_df %>%
    dplyr::group_by(.data$source_geoid, .data$target_geoid) %>%
    dplyr::summarise(intersection_area = sum(.data$intersection_area),
                     .groups = "drop") %>%
    dplyr::left_join(source_areas, by = "source_geoid") %>%
    dplyr::mutate(share = .data$intersection_area / .data$source_area) %>%
    dplyr::filter(.data$share > 1e-9) %>%
    dplyr::select("source_geoid", "target_geoid", "intersection_area", "share")

  ## Warn about source polygons that are partially or wholly outside all targets
  src_share = out1 %>%
    dplyr::group_by(.data$source_geoid) %>%
    dplyr::summarise(total_share = sum(.data$share), .groups = "drop")
  missing_src = setdiff(unique(sf::st_drop_geometry(src1)[["source_geoid"]]),
                        src_share$source_geoid)
  partial_src = src_share$source_geoid[src_share$total_share < 1 - 1e-3]
  dropped_n = length(missing_src) + length(partial_src)
  if (dropped_n > 0) {
    cli::cli_warn(c(
      "{dropped_n} source polygon{?s} fall{?s/} partially or wholly outside the target geographies.",
      "i" = "Data from those areas will be partially or wholly excluded from the interpolated result."))
  }

  ## Renormalize shares so each source's allocations sum to 1 — interpolate_acs
  ## warns otherwise. This treats the source-area outside the targets as
  ## non-existent (the warning above already surfaced the data drop).
  out1 = out1 %>%
    dplyr::group_by(.data$source_geoid) %>%
    dplyr::mutate(share = .data$share / sum(.data$share)) %>%
    dplyr::ungroup()

  ## Target-side coverage: a target polygon that extends beyond the union of the
  ## source geographies can't be accurately interpolated (the uncovered area
  ## contributes nothing, silently biasing counts and rates). Flag any target
  ## whose area is not nearly fully covered by source polygons — including
  ## targets with no overlap at all — and attach them so interpolate_to_targets()
  ## can set those rows to NA and warn the user.
  target_areas = tibble::tibble(
    target_geoid = sf::st_drop_geometry(tgt1)[["target_geoid"]],
    target_area  = as.numeric(sf::st_area(tgt1)))

  target_coverage = inter_df %>%
    dplyr::group_by(.data$target_geoid) %>%
    dplyr::summarise(covered_area = sum(.data$intersection_area),
                     .groups = "drop") %>%
    dplyr::right_join(target_areas, by = "target_geoid") %>%
    dplyr::mutate(
      covered_area = dplyr::coalesce(.data$covered_area, 0),
      coverage     = .data$covered_area / .data$target_area)

  incomplete_targets = target_coverage$target_geoid[
    target_coverage$coverage < .target_coverage_threshold]

  attr(out1, "incomplete_targets") = incomplete_targets
  out1
}

## For each benchmark level, pick the parent_geoid that contributes the
## largest intersection area to each target polygon. Returns a tibble keyed
## by (GEOID, level) with a parent_geoid column. Returns NULL when there
## are no benchmark levels to compute.
compute_target_parent_map = function(crosswalk, benchmark_levels) {
  if (length(benchmark_levels) == 0) return(NULL)
  levels1 = unname(benchmark_levels[benchmark_levels != "none"])
  if (length(levels1) == 0) return(NULL)

  purrr::map(levels1, function(lvl) {
    parents1 = assign_parent_geoids(crosswalk$source_geoid, lvl, lookup = NULL)
    tibble::tibble(
      GEOID        = crosswalk$target_geoid,
      parent_geoid = parents1,
      area         = crosswalk$intersection_area) %>%
      dplyr::filter(!is.na(.data$parent_geoid)) %>%
      dplyr::group_by(.data$GEOID, .data$parent_geoid) %>%
      dplyr::summarise(area = sum(.data$area), .groups = "drop_last") %>%
      dplyr::slice_max(.data$area, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(level = lvl) %>%
      dplyr::select("GEOID", "level", "parent_geoid")
  }) %>% purrr::list_rbind()
}

## Interpolate source ACS data onto an arbitrary target `sf` via an
## area-weighted crosswalk. This is the runtime-callable core shared by the
## launch-time uploaded-target path (`prepare_target_dataset()`) and the
## reactive draw-your-own-polygons path in the Shiny server. `target_sf` must
## already be a validated `sf` object carrying a unique `GEOID` column (and,
## optionally, a `NAME` column used in popups). Returns a list with `data`
## (an sf object with the codebook attached as an attribute) and
## `parent_lookup` (the target→parent map for benchmarking).
interpolate_to_targets = function(source_sf, target_sf, benchmark_levels) {
  crosswalk1 = build_spatial_crosswalk(source_sf, target_sf)
  incomplete_targets = attr(crosswalk1, "incomplete_targets")
  if (is.null(incomplete_targets)) incomplete_targets = character(0)

  ## interpolate_acs() takes source_geoid (default "GEOID") and joins to the
  ## crosswalk via that name; the crosswalk's target column must be uniquely
  ## named so it doesn't collide with the source GEOID.
  xwalk_for_interp = crosswalk1 %>%
    dplyr::transmute(
      GEOID        = .data$source_geoid,
      target_GEOID = .data$target_geoid,
      share        = .data$share)

  interp1 = interpolate_acs(
    .data                 = sf::st_drop_geometry(source_sf),
    target_geoid_column   = "target_GEOID",
    weight                = "share",
    crosswalk             = xwalk_for_interp)
  interp_codebook = attr(interp1, "codebook")

  ## Carry through NAME (for popups) when the target provides one.
  keep_cols = c("GEOID", if ("NAME" %in% colnames(target_sf)) "NAME")
  geom1 = target_sf %>%
    dplyr::distinct(.data$GEOID, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(keep_cols)) %>%
    sf::st_transform(4326)

  ## left_join (not inner_join) so targets with no source overlap are retained
  ## as all-NA rows rather than silently dropped — these extend beyond the
  ## source data and must surface as NA. left_join preserves sf-ness on the LHS;
  ## the geometry side goes first so the result remains sf. dplyr's sf join
  ## methods strip non-codebook attrs, so the codebook is re-attached below.
  joined1 = geom1 %>%
    dplyr::left_join(interp1, by = "GEOID")

  ## NA out targets that extend beyond the source extent. Zero-overlap targets
  ## are already NA from the left_join; partially-covered ones still carry
  ## (biased) interpolated values, so blank their measure columns here. Identity
  ## columns (GEOID, NAME, year, geometry) are preserved so the rows still map.
  if (length(incomplete_targets) > 0) {
    geom_col1 = attr(joined1, "sf_column")
    id_cols1  = intersect(c("GEOID", "NAME", "data_source_year", geom_col1),
                          colnames(joined1))
    value_cols1 = setdiff(colnames(joined1), id_cols1)
    na_rows1 = joined1[["GEOID"]] %in% incomplete_targets
    if (any(na_rows1) && length(value_cols1) > 0) {
      joined1[na_rows1, value_cols1] = NA
    }
  }

  attr(joined1, "codebook") = interp_codebook

  parent_lookup = compute_target_parent_map(crosswalk1, benchmark_levels)

  list(data = joined1, parent_lookup = parent_lookup,
       incomplete = incomplete_targets)
}

## Interpolate source ACS data onto user-supplied `target_geographies` via an
## area-weighted crosswalk. Validates the user-facing argument, then delegates
## to [interpolate_to_targets()]. Returns a list with `data` and
## `parent_lookup`; both are NULL when `target_geographies` is NULL.
prepare_target_dataset = function(.data, target_geographies, benchmark_levels) {
  if (is.null(target_geographies)) return(list(data = NULL, parent_lookup = NULL))

  if (!inherits(target_geographies, "sf")) {
    cli::cli_abort("{.arg target_geographies} must be an {.cls sf} object.")
  }
  if (!"GEOID" %in% colnames(target_geographies)) {
    cli::cli_abort(c(
      "{.arg target_geographies} must contain a {.code GEOID} column.",
      "i" = "Add one whose values uniquely identify each target polygon."))
  }
  if (nrow(target_geographies) == 0) {
    cli::cli_abort("{.arg target_geographies} has zero rows.")
  }

  cli::cli_inform("Computing spatial crosswalk and interpolating to target geographies...")

  res1 = interpolate_to_targets(.data, target_geographies, benchmark_levels)

  n_incomplete1 = length(res1$incomplete)
  if (n_incomplete1 > 0) {
    cli::cli_warn(c(
      paste("{n_incomplete1} target geograph{?y/ies} extend{?s/} beyond the",
            "source data extent and {?was/were} set to NA."),
      "i" = "Interpolation is only accurate where targets are fully covered by the source geographies."))
  }
  res1
}

## Run a population-weighted aggregation of `.data` up to the parent level via
## interpolate_acs(). Returns a tibble keyed by `parent_geoid` containing
## aggregated values for every variable, with MOEs preserved. Returns NULL on
## error so the UI can fall back gracefully.
compute_benchmark_data = function(.data, target_level) {
  if (length(target_level) == 0 || tolower(target_level) == "none") return(NULL)
  if (!"GEOID" %in% colnames(.data)) return(NULL)

  .data1 = .data
  .data1[["__parent_geoid__"]] = assign_parent_geoids(.data1[["GEOID"]], target_level)
  if (all(is.na(.data1[["__parent_geoid__"]]))) return(NULL)

  res1 = tryCatch(
    interpolate_acs(
      .data        = .data1,
      target_geoid_column = "__parent_geoid__",
      weight       = NULL),
    error = function(e) NULL)
  if (is.null(res1)) return(NULL)

  ## interpolate_acs renames target_geoid_column → GEOID; surface it as
  ## parent_geoid so it doesn't collide with the source data's GEOID on join.
  res1 = sf::st_drop_geometry(res1) %>%
    dplyr::rename(parent_geoid = "GEOID")
  res1
}

## Three-way classification using tidycensus::significance(). Inputs are
## per-row vectors. Returns a character vector with values in
## {"Larger", "Smaller", "Not significant", NA}.
classify_significance = function(est1, moe1, est2, moe2, clevel = 0.9) {
  n1 = length(est1)
  if (n1 == 0) return(character(0))

  if (length(moe1) == 0 || length(moe2) == 0 || all(is.na(moe1)) || all(is.na(moe2))) {
    return(rep(NA_character_, n1))
  }

  sig1 = tryCatch(
    tidycensus::significance(est1 = est1, est2 = est2,
                             moe1 = moe1, moe2 = moe2,
                             clevel = clevel),
    error = function(e) rep(NA, n1))

  out1 = rep("Not significant", n1)
  out1[!is.na(sig1) & sig1 & est1 > est2] = "Larger"
  out1[!is.na(sig1) & sig1 & est1 < est2] = "Smaller"
  out1[is.na(est1) | is.na(est2) | is.na(moe1) | is.na(moe2) | is.na(sig1)] = NA_character_
  out1
}

## Build a per-row HTML popup string. The top line is the selected variable's
## name, followed by `{geography name}: {value}`. When `benchmark` is non-NULL,
## a `{benchmark name}: {benchmark value}` line is added, plus a final
## color-coded sentence describing the significance of the comparison. When
## `out_of_range_idx` is supplied, those rows get an additional note.
make_popup_html = function(data1, var1, moe_var, fmt, label1,
                           out_of_range_idx = NULL,
                           benchmark = NULL) {
  name1   = if ("NAME" %in% colnames(data1)) data1[["NAME"]] else data1[["GEOID"]]
  name1   = clean_geography_name(name1)
  value_s = format_value(data1[[var1]], fmt)

  ## `{geography name}: {value}` (with margin of error when one is available).
  value_line = if (!is.null(moe_var) && moe_var %in% colnames(data1)) {
    moe_s = format_value(data1[[moe_var]], fmt)
    paste0(htmltools_escape(name1), ": ", value_s, " (± ", moe_s, ")")
  } else {
    paste0(htmltools_escape(name1), ": ", value_s)
  }

  ## Top line: the selected variable's name.
  base1 = paste0(
    "<strong>", htmltools_escape(label1), "</strong><br/>",
    value_line)

  if (!is.null(benchmark)) {
    bench_s = format_value(benchmark$values, fmt)
    base1 = paste0(
      base1, "<br/>",
      htmltools_escape(benchmark$label), ": ", bench_s)

    sig1 = benchmark$category
    sig_phrase = dplyr::case_when(
      sig1 == "Larger"          ~ "Statistically significantly larger",
      sig1 == "Smaller"         ~ "Statistically significantly smaller",
      sig1 == "Not significant" ~ "Not statistically significantly different",
      TRUE                      ~ NA_character_)
    ## Color the sentence to match the map's significance palette; fall back to
    ## a readable grey for "not significant" (the swatch grey is too light here).
    sig_color = dplyr::case_when(
      sig1 == "Larger"  ~ .benchmark_colors[["Larger"]],
      sig1 == "Smaller" ~ .benchmark_colors[["Smaller"]],
      TRUE              ~ "#5c5859")
    sig_sentence = dplyr::if_else(
      is.na(sig_phrase),
      "<em style=\"color:#5c5859;\">No statistical comparison is available.</em>",
      paste0(
        "<span style=\"color:", sig_color, "; font-weight:600;\">",
        sig_phrase, "</span>"))
    base1 = paste0(base1, "<br/>", sig_sentence)
  }

  if (!is.null(out_of_range_idx) && length(out_of_range_idx) > 0) {
    base1[out_of_range_idx] = paste0(
      base1[out_of_range_idx],
      "<br/><em style=\"color:#5c5859;\">Outside selected range</em>")
  }
  base1
}

## Shorten geography names for popups. Tract names from tidycensus look like
## "Census Tract 175, Essex County, New Jersey" (comma- or semicolon-separated);
## for tracts we drop the state and the "Census " prefix so they read
## "Tract 175, Essex County". Non-tract names (counties, states, drawn areas,
## bare GEOIDs) are returned unchanged apart from separator normalization.
clean_geography_name = function(name1) {
  is_tract = stringr::str_detect(name1, stringr::coll("Census Tract")) %in% TRUE
  parts = stringr::str_split(name1, "\\s*[;,]\\s*")
  purrr::map2_chr(parts, is_tract, function(parts1, tract1) {
    if (tract1) {
      parts1 = stringr::str_remove(parts1, "^Census ")     # "Census Tract" -> "Tract"
      parts1 = parts1[seq_len(min(2L, length(parts1)))]    # drop the state component
    }
    stringr::str_c(parts1, collapse = ", ")
  })
}

## Minimal HTML-escape so we don't pull in htmltools just for popups.
htmltools_escape = function(x) {
  x %>%
    stringr::str_replace_all("&",  "&amp;") %>%
    stringr::str_replace_all("<",  "&lt;")  %>%
    stringr::str_replace_all(">",  "&gt;")  %>%
    stringr::str_replace_all('"', "&quot;")
}

## Evenly-spaced color stops across a numeric range; one stop per palette entry.
## `range1` is taken as a 2-element numeric (lower, upper); when degenerate the
## upper bound is nudged so mapgl::interpolate gets a strictly increasing input.
make_color_stops = function(range1, palette) {
  if (length(range1) != 2 || any(!is.finite(range1))) return(NULL)
  if (range1[1] == range1[2]) range1[2] = range1[1] + 1e-9
  list(
    stops  = seq(range1[1], range1[2], length.out = length(palette)),
    colors = palette)
}

## Identify rows whose value for `var1` falls outside `[range1[1], range1[2]]`.
## NA values are treated as in-range (mapgl's na_color handles them anyway).
out_of_range_indices = function(values, range1) {
  if (length(range1) != 2 || any(!is.finite(range1))) return(integer(0))
  out1 = !is.na(values) & (values < range1[1] | values > range1[2])
  which(out1)
}

.view_acs_ui = function(choices1, has_multi_year, data1, benchmark_levels) {
  year_choices = if (has_multi_year) sort(unique(data1$data_source_year)) else NULL

  sidebar1 = bslib::sidebar(
    width = 320,
    ## The Source/Target toggle appears once a target exists — either supplied
    ## at launch via `target_geographies` or drawn in-app. `has_target_flag` is
    ## a server-side reactive that tracks this.
    shiny::conditionalPanel(
      condition = "output.has_target_flag == true",
      shiny::selectInput("geo_view",
        label   = "Geography",
        choices = c("Source", "Target"),
        selected = "Source")),
    shiny::selectInput("variable",
      label   = "Variable",
      choices = choices1,
      selected = unname(choices1[[1]])),
    if (has_multi_year)
      shiny::selectInput("year",
        label   = "Year",
        choices = year_choices,
        selected = max(year_choices))
      else NULL,
    if (length(benchmark_levels) > 0)
      shiny::selectInput("benchmark",
        label   = "Statistical benchmark",
        choices = benchmark_levels,
        selected = "none")
      else NULL,
    if (length(benchmark_levels) > 0)
      shiny::tags$div(
        style = "font-size: 9px; color: #5c5859; margin-top: -0.3em;",
        shiny::textOutput("benchmark_note", inline = FALSE))
      else NULL,
    shiny::tags$hr(style = "margin: 0;"),
    shiny::tags$div(
      style = "font-weight: 700; font-size: 11px; margin-bottom: 0.05em;",
      "Export"),
    shiny::downloadButton("download_data", "Download data",
      class = "btn-outline-secondary btn-sm", style = "width: 100%;"),
    shiny::actionButton("figure_options", "Download figure",
      class = "btn-outline-secondary btn-sm", width = "100%"),
    shiny::tags$hr(style = "margin: 0;"),
    shiny::tags$div(
      shiny::tags$div(
        style = "font-weight: 700; font-size: 11px; margin-bottom: 0.05em;",
        "Custom geographies"),
      shiny::tags$div(
        style = "font-size: 10px; color: #000000;",
        paste("Use the polygon tool at the top-left of the map to draw one or",
              "more areas, then interpolate the ACS data onto them."))),
    shiny::actionButton("use_drawn", "Interpolate to drawn area",
      class = "btn-primary btn-sm", width = "100%"),
    shiny::conditionalPanel(
      condition = "output.has_target_flag == true",
      shiny::actionButton("clear_target", "Clear target",
        class = "btn-outline-secondary btn-sm", width = "100%")),
    shiny::tags$hr(style = "margin: 0;"),
    shiny::tags$div(
      style = "font-size: 10px; color: #000000;",
      "Drag on the histogram to limit the choropleth color range."),
    shiny::plotOutput(
      "hist",
      height = "240px",
      brush  = shiny::brushOpts(
        id         = "hist_brush",
        direction  = "x",
        resetOnNew = TRUE,
        fill       = "#1696d2",
        stroke     = "#0a4c6a",
        opacity    = 0.25)))

  bslib::page_sidebar(
    title    = "ACS Data Viewer",
    sidebar  = sidebar1,
    fillable = TRUE,
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(.legend_css)),
      shiny::tags$style(shiny::HTML(.notification_css)),
      shiny::tags$style(shiny::HTML(.sidebar_css)),
      shiny::tags$script(shiny::HTML(.export_js))),
    mapgl::maplibreOutput("map", height = "100%"))
}

## Let long legend titles wrap and give the legend a touch more breathing room.
## Note: do NOT set background-color on `[class*='legend']` here — that selector
## also matches the color-swatch elements inside categorical legends and would
## paint them white, hiding the colors entirely.
.legend_css = "
.maplibregl-map [class*='legend'],
.mapboxgl-map  [class*='legend'] {
  max-width: 320px;
}
.maplibregl-map [class*='legend'] *,
.mapboxgl-map  [class*='legend'] * {
  white-space: normal;
  overflow: visible;
  text-overflow: clip;
}
"

## Compact sidebar typography: bold-but-small input labels ("Variable",
## "Statistical benchmark", etc.), smaller control/button text, and tighter
## vertical spacing between controls. The smallest size used anywhere is 9px
## (the help/explanatory text set inline in .view_acs_ui).
.sidebar_css = "
.sidebar > .sidebar-content {
  gap: 0.35rem !important;
  padding-left: 0.9rem !important;
  padding-right: 0.9rem !important;
  padding-top: 1.5rem !important;
  padding-bottom: 0.9rem !important;
}
.sidebar .control-label,
.sidebar label.control-label {
  font-weight: 700;
  font-size: 11px;
  margin-bottom: 0.1rem;
}
.sidebar .shiny-input-container,
.sidebar .form-group {
  margin-bottom: 0;
}
.sidebar .form-select,
.sidebar .selectize-input,
.sidebar select {
  font-size: 11px;
  min-height: auto;
  padding-top: 0.2rem;
  padding-bottom: 0.2rem;
}
.sidebar .btn-sm {
  font-size: 11px;
  padding: 0.25rem 0.5rem;
}
"

## Client-side map-image export. mapgl ships no figure export, so this registers
## a Shiny custom-message handler that captures the live MapLibre canvas as a
## PNG and triggers a browser download. The downloaded image is exactly what is
## on screen (basemap, active layer, current zoom/pan, drawn areas).
##
## The on-screen legend is a separate HTML overlay, not part of the WebGL
## canvas, so it is reconstructed onto the export with the Canvas 2D API from a
## legend spec sent by the server (title, colors, value labels). At scale > 1 an
## off-screen copy of the map's current style and camera is rendered at a higher
## pixelRatio for a higher-resolution export; on any failure it falls back to the
## visible canvas (readable because the map uses preserveDrawingBuffer = TRUE).
## A polling wrapper defers registration until Shiny is available.
.export_js = "
(function register() {
  if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) {
    setTimeout(register, 50);
    return;
  }

  function roundRect(ctx, x, y, w, h, r) {
    ctx.beginPath();
    ctx.moveTo(x + r, y);
    ctx.arcTo(x + w, y, x + w, y + h, r);
    ctx.arcTo(x + w, y + h, x, y + h, r);
    ctx.arcTo(x, y + h, x, y, r);
    ctx.arcTo(x, y, x + w, y, r);
    ctx.closePath();
  }
  function wrapText(ctx, text, maxW) {
    var words = String(text).split(' '), lines = [], cur = '';
    for (var i = 0; i < words.length; i++) {
      var test = cur ? cur + ' ' + words[i] : words[i];
      if (ctx.measureText(test).width > maxW && cur) { lines.push(cur); cur = words[i]; }
      else { cur = test; }
    }
    if (cur) lines.push(cur);
    return lines;
  }

  // Draw a legend matching the app's top-right overlay onto the export canvas.
  // S = device pixels per CSS pixel, so the legend is sized like the on-screen one.
  function drawLegend(ctx, lg, W, S) {
    if (!lg || !lg.type) return;
    var pad = 8 * S, fT = 10.5 * S, fL = 11 * S, mg = 10 * S, lineGap = 4 * S;
    var titleLH = fT * 1.25, sw = 12 * S, gap = 6 * S, rowH = 18 * S, barH = 10 * S;
    var labels = lg.labels || [], colors = lg.colors || [];
    ctx.textBaseline = 'top'; ctx.textAlign = 'left';

    // Content width is driven by the text only; the continuous bar then spans it.
    ctx.font = fT + 'px sans-serif';
    var titleLines = wrapText(ctx, lg.title || '', 200 * S);
    var contentW = 0, i;
    for (i = 0; i < titleLines.length; i++) {
      contentW = Math.max(contentW, ctx.measureText(titleLines[i]).width);
    }
    ctx.font = fL + 'px sans-serif';
    if (lg.type === 'categorical') {
      for (i = 0; i < labels.length; i++) {
        contentW = Math.max(contentW, sw + gap + ctx.measureText(labels[i]).width);
      }
    } else {
      var lo = labels[0] || '', hi = labels[labels.length - 1] || '';
      contentW = Math.max(contentW, ctx.measureText(lo).width + 10 * S + ctx.measureText(hi).width);
    }
    var barW = contentW;

    var boxW = contentW + pad * 2;
    var titleH = titleLines.length * titleLH;
    var boxH = (lg.type === 'categorical')
      ? pad * 2 + titleH + lineGap + labels.length * rowH
      : pad * 2 + titleH + lineGap + barH + lineGap + fL;
    var x = W - boxW - mg, y = mg;

    ctx.fillStyle = 'rgba(255, 255, 255, 0.8)';
    roundRect(ctx, x, y, boxW, boxH, 4 * S); ctx.fill();

    ctx.fillStyle = '#000000'; ctx.font = fT + 'px sans-serif';
    var ty = y + pad;
    for (i = 0; i < titleLines.length; i++) { ctx.fillText(titleLines[i], x + pad, ty); ty += titleLH; }

    var cy = y + pad + titleH + lineGap;
    if (lg.type === 'categorical') {
      ctx.font = fL + 'px sans-serif';
      for (i = 0; i < labels.length; i++) {
        ctx.fillStyle = colors[i]; ctx.fillRect(x + pad, cy + (rowH - sw) / 2, sw, sw);
        ctx.fillStyle = '#000000'; ctx.fillText(labels[i], x + pad + sw + gap, cy + (rowH - fL) / 2);
        cy += rowH;
      }
    } else {
      var grad = ctx.createLinearGradient(x + pad, 0, x + pad + barW, 0), n = colors.length;
      for (i = 0; i < n; i++) { grad.addColorStop(n === 1 ? 0 : i / (n - 1), colors[i]); }
      ctx.fillStyle = grad; ctx.fillRect(x + pad, cy, barW, barH);
      cy += barH + lineGap;
      ctx.fillStyle = '#000000'; ctx.font = fL + 'px sans-serif';
      ctx.textAlign = 'left';  ctx.fillText(labels[0] || '', x + pad, cy);
      ctx.textAlign = 'right'; ctx.fillText(labels[labels.length - 1] || '', x + pad + barW, cy);
      ctx.textAlign = 'left';
    }
  }

  Shiny.addCustomMessageHandler('acs_export_map', function(data) {
    var widget = HTMLWidgets.find('#' + data.id);
    if (!widget || typeof widget.getMap !== 'function') return;
    var map = widget.getMap();
    if (!map) return;
    var scale = data.scale || 1;
    var filename = data.filename || 'map.png';
    var legend = data.legend || null;
    var container = map.getContainer();
    var w = container.offsetWidth;
    var h = container.offsetHeight;

    function triggerDownload(url) {
      var a = document.createElement('a');
      a.href = url;
      a.download = filename;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
    }
    // Composite the map canvas + reconstructed legend onto a 2D canvas.
    function composite(srcCanvas) {
      var out = document.createElement('canvas');
      out.width = srcCanvas.width; out.height = srcCanvas.height;
      var ctx = out.getContext('2d');
      ctx.drawImage(srcCanvas, 0, 0);
      drawLegend(ctx, legend, out.width, w ? srcCanvas.width / w : 1);
      return out.toDataURL('image/png');
    }
    function captureVisible() {
      try { triggerDownload(composite(map.getCanvas())); }
      catch (e) { console.error('Map export failed:', e); }
    }

    if (typeof maplibregl === 'undefined' || scale <= 1 || !w || !h) {
      captureVisible();
      return;
    }

    var hidden = document.createElement('div');
    hidden.style.position = 'absolute';
    hidden.style.left = '-10000px';
    hidden.style.top = '0';
    hidden.style.width = w + 'px';
    hidden.style.height = h + 'px';
    document.body.appendChild(hidden);

    var renderMap;
    try {
      renderMap = new maplibregl.Map({
        container: hidden,
        style: map.getStyle(),
        center: map.getCenter(),
        zoom: map.getZoom(),
        bearing: map.getBearing(),
        pitch: map.getPitch(),
        interactive: false,
        preserveDrawingBuffer: true,
        pixelRatio: scale * (window.devicePixelRatio || 1),
        attributionControl: false
      });
    } catch (e) {
      if (hidden.parentNode) hidden.parentNode.removeChild(hidden);
      captureVisible();
      return;
    }

    var done = false;
    function finish() {
      if (done) return;
      done = true;
      try { triggerDownload(composite(renderMap.getCanvas())); }
      catch (e) { console.error('Map export failed:', e); captureVisible(); }
      try { renderMap.remove(); } catch (e) {}
      if (hidden.parentNode) hidden.parentNode.removeChild(hidden);
    }
    renderMap.once('idle', finish);
    setTimeout(finish, 8000);
  });
})();
"

## Compact, neutral notification styling: white background and black text for
## every type (the type is conveyed by a small header instead of a background
## color), 10px text, and a narrower panel. See .acs_notify().
.notification_css = "
#shiny-notification-panel {
  width: 260px;
}
.shiny-notification {
  background-color: #ffffff !important;
  color: #000000 !important;
  font-size: 10px !important;
  line-height: 1.3 !important;
  padding: 6px 22px 6px 10px !important;
  border: 1px solid #d2d2d2 !important;
  border-radius: 3px !important;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.15) !important;
  opacity: 0.98 !important;
}
.shiny-notification-message,
.shiny-notification-warning,
.shiny-notification-error,
.shiny-notification-default {
  background-color: #ffffff !important;
  color: #000000 !important;
  border-left: none !important;
}
.shiny-notification .acs-note-header {
  font-weight: 700;
  font-size: 10px;
  margin-bottom: 2px;
}
.shiny-notification-close {
  font-size: 13px !important;
  top: 3px !important;
}
"

## Show a compact, neutral-styled notification carrying a small type header
## ("Warning", "Error", "Note") rather than a colored background. There is no
## "Success" variant on purpose: changes that render visibly on the map don't
## need a toast to confirm them.
.acs_notify = function(message, type = c("note", "warning", "error"),
                       duration = 8) {
  type = match.arg(type)
  header1 = switch(type, note = "Note", warning = "Warning", error = "Error")
  ## Map to shiny's notification types; background color is overridden to white
  ## by .notification_css regardless, so this only affects the semantic class.
  shiny_type1 = switch(type, note = "default", warning = "warning", error = "error")
  shiny::showNotification(
    ui = shiny::tags$div(
      shiny::tags$div(class = "acs-note-header", header1),
      shiny::tags$div(class = "acs-note-body", message)),
    type     = shiny_type1,
    duration = duration)
}

.view_acs_server = function(data1, codebook, choices1, has_multi_year,
                            geography, benchmark_levels,
                            target_data1 = NULL, target_codebook = NULL,
                            target_parent_lookup = NULL,
                            target_incomplete = NULL) {
  ## The launch-time target (if any) seeds the reactive target state; users can
  ## additionally draw polygons in-app to replace it (see the `use_drawn`
  ## observer below).
  launch_target = if (is.null(target_data1)) {
    NULL
  } else {
    list(data = target_data1, codebook = target_codebook,
         parent_lookup = target_parent_lookup)
  }

  ## Source sf used for runtime interpolation onto drawn polygons. st_transform
  ## may drop the codebook attribute, so re-attach it here: interpolate_acs()
  ## reads the codebook from this object to dispatch per-variable aggregation.
  source_sf_for_interp = data1
  attr(source_sf_for_interp, "codebook") = codebook

  function(input, output, session) {

    ## Surface, once the UI is ready, any launch-time target geographies that
    ## extended beyond the source extent and were set to NA.
    if (length(target_incomplete) > 0) {
      session$onFlushed(function() {
        .acs_notify(
          paste0(length(target_incomplete), " target geograph",
                 if (length(target_incomplete) > 1) "ies" else "y",
                 " extend beyond the source data extent and were set to NA."),
          type = "warning", duration = 10)
      }, once = TRUE)
    }

    ## Current target dataset: a list(data, codebook, parent_lookup), or NULL
    ## when no target exists. Seeded from the launch-time target and replaced
    ## when the user draws polygons.
    target_state = shiny::reactiveVal(launch_target)
    has_target_now = shiny::reactive(!is.null(target_state()))

    output$has_target_flag = shiny::reactive(has_target_now())
    shiny::outputOptions(output, "has_target_flag", suspendWhenHidden = FALSE)

    active_dataset = shiny::reactive({
      ts1   = target_state()
      view1 = if (!is.null(ts1)) (input$geo_view %||% "Source") else "Source"
      if (view1 == "Target" && !is.null(ts1)) {
        list(data = ts1$data, codebook = ts1$codebook, view = "target")
      } else {
        list(data = data1, codebook = codebook, view = "source")
      }
    })

    filter_by_year = function(df) {
      if (has_multi_year && !is.null(input$year) &&
          "data_source_year" %in% colnames(df)) {
        df[df$data_source_year == as.numeric(input$year), , drop = FALSE]
      } else {
        df
      }
    }

    filtered_data = shiny::reactive({
      filter_by_year(active_dataset()$data)
    })

    ## Source-data view always used for benchmark aggregation, regardless of
    ## which view is being rendered on the map.
    source_filtered_data = shiny::reactive({
      filter_by_year(data1)
    })

    active_var_info = shiny::reactive({
      ds   = active_dataset()
      var1 = input$variable
      shiny::req(var1)
      cb1  = ds$codebook
      row1 = cb1[cb1$calculated_variable == var1, , drop = FALSE][1, ]
      vt1  = if (nrow(row1) >= 1) row1$variable_type else "Count"
      pal1 = palette_for_type(vt1)
      moe1 = paste0(var1, "_M")
      list(
        var     = var1,
        label   = make_pretty_names(var1),
        type    = vt1,
        palette = pal1$palette,
        fmt     = pal1$fmt,
        moe     = if (moe1 %in% colnames(ds$data)) moe1 else NULL)
    })

    default_range = shiny::reactive({
      info1 = active_var_info()
      default_range_for_variable(filtered_data()[[info1$var]], info1$type)
    })

    selected_range = shiny::reactive({
      brush1 = input$hist_brush
      def1   = default_range()
      if (is.null(brush1) || !is.finite(brush1$xmin) || !is.finite(brush1$xmax)) {
        return(def1)
      }
      lo1 = max(def1[1], brush1$xmin)
      hi1 = min(def1[2], brush1$xmax)
      if (lo1 >= hi1) def1 else c(lo1, hi1)
    })

    ## Whether a benchmark is being requested *and* possible for this variable.
    ## Counts/Sums are excluded: the parent benchmark is computed by aggregating
    ## these same rows up to the parent geography (sum-of-tracts = the county
    ## value), so a tract is by construction a component of its parent, not an
    ## independent estimate, and a significance test isn't meaningful.
    ## Variables without an MOE are likewise excluded since the test needs one.
    benchmark_state = shiny::reactive({
      if (length(benchmark_levels) == 0) {
        return(list(active = FALSE, level = "none", label = NULL, reason = NULL))
      }
      level1 = input$benchmark
      if (is.null(level1) || tolower(level1) == "none") {
        return(list(active = FALSE, level = "none", label = NULL, reason = NULL))
      }
      info1 = active_var_info()
      if (info1$type %in% c("Count", "Sum")) {
        return(list(active = FALSE, level = level1, label = NULL,
                    reason = "Benchmarking is only relevant and supported for standardized variables such as percentages, medians, or averages."))
      }
      if (is.null(info1$moe)) {
        return(list(active = FALSE, level = level1, label = NULL,
                    reason = "Benchmarking requires a margin of error; this variable doesn't have one."))
      }
      list(
        active = TRUE,
        level  = level1,
        label  = stringr::str_to_title(level1),
        reason = NULL)
    })

    ## Benchmark aggregation result, cached per (level, year). Always derived
    ## from the source dataset, even when the map is showing target view —
    ## the user's choice was to keep using the source-data benchmark.
    ## Returns a tibble keyed by parent_geoid.
    benchmark_table = shiny::reactive({
      st1 = benchmark_state()
      if (!st1$active) return(NULL)
      compute_benchmark_data(source_filtered_data(), st1$level)
    })

    ## Per-row benchmark info for the active variable: a list of `values`,
    ## `moes`, and `category` aligned to filtered_data() row order. NULL when
    ## benchmarking is inactive.
    benchmark_for_active_var = shiny::reactive({
      st1 = benchmark_state()
      if (!st1$active) return(NULL)
      info1 = active_var_info()
      bt1   = benchmark_table()
      if (is.null(bt1) || !info1$var %in% colnames(bt1)) return(NULL)

      df1 = filtered_data()
      ds  = active_dataset()
      lookup1 = if (ds$view == "target") target_state()$parent_lookup else NULL
      parent_ids = assign_parent_geoids(df1[["GEOID"]], st1$level,
                                        lookup = lookup1)
      lookup_idx = match(parent_ids, bt1[["parent_geoid"]])

      bench_vals = bt1[[info1$var]][lookup_idx]
      bench_moes_col = paste0(info1$var, "_M")
      bench_moes = if (bench_moes_col %in% colnames(bt1)) {
        bt1[[bench_moes_col]][lookup_idx]
      } else {
        rep(NA_real_, length(parent_ids))
      }

      cats1 = classify_significance(
        est1 = df1[[info1$var]],
        moe1 = df1[[info1$moe]],
        est2 = bench_vals,
        moe2 = bench_moes,
        clevel = 0.9)

      list(values = bench_vals, moes = bench_moes,
           category = cats1, label = st1$label)
    })

    output$benchmark_note = shiny::renderText({
      st1 = benchmark_state()
      if (is.null(st1$reason)) "" else st1$reason
    })
    shiny::outputOptions(output, "benchmark_note", suspendWhenHidden = FALSE)

    ## ----- Downloads -----
    ## The currently-visualized data (the active dataset filtered to the
    ## selected year). Per-variable MOEs are already present as `<var>_M`
    ## columns; when benchmarking is active, the benchmark value, its MOE, and
    ## the significance category for the selected variable are appended.
    export_data = shiny::reactive({
      df1   = filtered_data()
      base1 = sf::st_drop_geometry(df1)
      bm1   = benchmark_for_active_var()
      if (!is.null(bm1)) {
        info1 = active_var_info()
        base1[[paste0(info1$var, "_benchmark")]]    = bm1$values
        base1[[paste0(info1$var, "_benchmark_M")]]  = bm1$moes
        base1[[paste0(info1$var, "_significance")]] = bm1$category
      }
      base1
    })

    output$download_data = shiny::downloadHandler(
      filename = function() paste0("acs_data_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(export_data(), file, row.names = FALSE, na = "")
      })

    ## Spec for the legend reconstructed onto the exported figure (the on-screen
    ## legend is an HTML overlay outside the WebGL canvas). Mirrors the legend
    ## drawn by .add_choropleth_layer(); NULL when the live map has no legend.
    legend_spec = shiny::reactive({
      info1 = active_var_info()
      bm1   = benchmark_for_active_var()
      if (!is.null(bm1)) {
        return(list(
          type   = "categorical",
          title  = "Statistically significant differences",
          colors = unname(c(.benchmark_colors[["Larger"]],
                            .benchmark_colors[["Smaller"]],
                            .benchmark_colors[["Not significant"]])),
          labels = c("Larger", "Smaller", "Not significant")))
      }
      stops1 = make_color_stops(selected_range(), info1$palette)
      if (is.null(stops1)) return(NULL)
      list(
        type   = "continuous",
        title  = info1$label,
        colors = stops1$colors,
        labels = format_value(range(stops1$stops), info1$fmt))
    })

    ## "Download figure" opens a modal for the export resolution. The capture
    ## itself happens client-side (see .export_js), so the PNG is the live
    ## MapLibre map exactly as shown — basemap, the active layer, current
    ## zoom/pan, and any drawn areas.
    shiny::observeEvent(input$figure_options, {
      shiny::showModal(shiny::modalDialog(
        title     = "Download figure",
        size      = "s",
        easyClose = TRUE,
        shiny::tags$p(
          style = "font-size: 13px;",
          paste("Save the current map view as a PNG, including the basemap,",
                "the active layer, your zoom and pan, and any drawn areas.")),
        shiny::radioButtons("fig_scale", "Resolution",
          choices  = c("1× (screen)" = "1", "2×" = "2",
                       "3×" = "3", "4×" = "4"),
          selected = "2", inline = TRUE),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("do_figure_download", "Download .png",
            class = "btn-primary"))))
    })

    ## Ask the browser to capture the map at the chosen scale, then dismiss the
    ## modal. The download is triggered entirely client-side.
    shiny::observeEvent(input$do_figure_download, {
      session$sendCustomMessage("acs_export_map", list(
        id       = "map",
        scale    = as.numeric(input$fig_scale %||% "2"),
        filename = paste0("acs_map_", Sys.Date(), ".png"),
        legend   = legend_spec()))
      shiny::removeModal()
    })

    ## ----- Histogram (sidebar) -----
    output$hist = shiny::renderPlot({
      info1 = active_var_info()
      vals1 = filtered_data()[[info1$var]]
      vals1 = vals1[is.finite(vals1)]
      def1  = default_range()

      df_h = data.frame(value = vals1)
      x_labeller = function(x) format_value(x, info1$fmt, accuracy = 1)

      n_bins = 30
      binwidth1 = (def1[2] - def1[1]) / n_bins

      ggplot2::ggplot(df_h, ggplot2::aes(x = .data$value)) +
        ggplot2::geom_histogram(
          binwidth = binwidth1,
          boundary = def1[1],
          fill     = "#1696d2",
          color    = "#ffffff",
          na.rm    = TRUE) +
        ggplot2::coord_cartesian(xlim = def1) +
        ggplot2::scale_x_continuous(
          labels = x_labeller,
          expand = ggplot2::expansion(mult = 0.02)) +
        ggplot2::labs(
          subtitle = "Distribution of data values",
          x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          axis.text.y        = ggplot2::element_blank(),
          plot.subtitle      = ggplot2::element_text(size = 9, color = "#5c5859"),
          plot.margin        = ggplot2::margin(4, 6, 2, 2))
    }, res = 96)

    ## ----- Initial map render (one-shot via isolate; see notes above) -----
    output$map = mapgl::renderMaplibre({
      info1 = shiny::isolate(active_var_info())
      df1   = shiny::isolate(filtered_data())
      sel1  = shiny::isolate(selected_range())
      bm1   = shiny::isolate(benchmark_for_active_var())

      stops1 = make_color_stops(sel1, info1$palette)
      ## preserveDrawingBuffer keeps the WebGL canvas readable after compositing
      ## so the figure-export path (see .export_js) can call toDataURL().
      m1 = mapgl::maplibre(
        style                = mapgl::carto_style("positron"),
        bounds               = sf::st_bbox(df1),
        preserveDrawingBuffer = TRUE) %>%
        mapgl::add_draw_control(position = "top-left")
      .add_choropleth_layer(m1, df1, info1, stops1, sel1, bm1)
    })

    ## ----- Updates: redraw the layer on any of these changing -----
    shiny::observeEvent(
      list(input$variable, input$year, input$hist_brush, input$benchmark,
           input$geo_view, target_state()),
      ignoreInit = TRUE,
      {
        info1 = active_var_info()
        df1   = filtered_data()
        sel1  = selected_range()
        bm1   = benchmark_for_active_var()
        stops1 = make_color_stops(sel1, info1$palette)

        proxy1 = mapgl::maplibre_proxy("map")
        mapgl::clear_layer(proxy1, "acs")
        mapgl::clear_legend(proxy1)
        .add_choropleth_layer(proxy1, df1, info1, stops1, sel1, bm1)
      })

    ## When the active dataset changes (view toggle or a freshly drawn target),
    ## refit the bounds so the user sees the new dataset's full extent.
    shiny::observeEvent(
      list(input$geo_view, target_state()),
      ignoreInit = TRUE,
      {
        df1 = filtered_data()
        bbox1 = sf::st_bbox(df1)
        if (all(is.finite(bbox1))) {
          mapgl::fit_bounds(mapgl::maplibre_proxy("map"), bbox1)
        }
      })

    ## ----- Draw-your-own target geographies -----
    ## Pull the polygons the user drew on the map, interpolate the source ACS
    ## data onto them, and switch the map into Target view.
    shiny::observeEvent(input$use_drawn, {
      proxy1 = mapgl::maplibre_proxy("map")
      drawn1 = tryCatch(mapgl::get_drawn_features(proxy1),
                        error = function(e) NULL)

      if (is.null(drawn1) || nrow(drawn1) == 0) {
        .acs_notify(
          "Draw at least one polygon on the map first (polygon tool, top-left).",
          type = "warning")
        return()
      }

      geom_types1 = as.character(sf::st_geometry_type(drawn1))
      drawn_poly1 = drawn1[geom_types1 %in% c("POLYGON", "MULTIPOLYGON"), ]
      if (nrow(drawn_poly1) == 0) {
        .acs_notify(
          "No polygons found. Use the polygon tool to enclose an area.",
          type = "warning")
        return()
      }

      ## Give each drawn polygon a unique GEOID and a friendly NAME for popups;
      ## interpolate_to_targets() requires GEOID and uses NAME when present.
      drawn_poly1 = drawn_poly1 %>%
        dplyr::mutate(
          GEOID = paste0("drawn_", dplyr::row_number()),
          NAME  = paste0("Drawn area ", dplyr::row_number())) %>%
        dplyr::select("GEOID", "NAME")

      res1 = tryCatch(
        interpolate_to_targets(source_sf_for_interp, drawn_poly1, benchmark_levels),
        error = function(e) {
          .acs_notify(
            paste0("Could not interpolate to the drawn area: ", conditionMessage(e)),
            type = "error", duration = 8)
          NULL
        })
      if (is.null(res1)) return()

      ## Drawn outlines are now redundant with the target choropleth — clear them.
      mapgl::clear_drawn_features(proxy1)

      target_state(list(
        data          = res1$data,
        codebook      = attr(res1$data, "codebook"),
        parent_lookup = res1$parent_lookup))
      shiny::updateSelectInput(session, "geo_view", selected = "Target")
      ## No success toast — the map switching to the interpolated Target view is
      ## its own confirmation.

      ## Drawn areas that extend beyond the source data can't be accurately
      ## interpolated and were set to NA — flag them so the grey polygons read
      ## as "no data" rather than a value.
      n_incomplete1 = length(res1$incomplete)
      if (n_incomplete1 > 0) {
        .acs_notify(
          paste0(n_incomplete1, " drawn area",
                 if (n_incomplete1 > 1) "s" else "",
                 " extend beyond the source data extent and ",
                 if (n_incomplete1 > 1) "were" else "was",
                 " set to NA. Draw within the source geographies for accurate interpolation."),
          type = "warning", duration = 10)
      }
    })

    ## Discard the drawn target (reverting to any launch-time target, else the
    ## source view) and clear the polygons from the map.
    shiny::observeEvent(input$clear_target, {
      mapgl::clear_drawn_features(mapgl::maplibre_proxy("map"))
      target_state(launch_target)
      if (is.null(launch_target)) {
        shiny::updateSelectInput(session, "geo_view", selected = "Source")
      }
    })
  }
}

## Shared layer add for the initial render and proxy updates.
##
## When `benchmark` is NULL, polygons are filled by the continuous palette
## (existing behavior). When `benchmark` is supplied, polygons are filled
## categorically (Larger / Smaller / Not significant) with the three-color
## urbn palette. Out-of-range polygons (per the histogram brush) always
## override to grey.
.add_choropleth_layer = function(target, df1, info1, stops1, sel1,
                                 benchmark = NULL) {
  out_idx = out_of_range_indices(df1[[info1$var]], sel1)

  popup_text = make_popup_html(
    df1, info1$var, info1$moe, info1$fmt, info1$label,
    out_of_range_idx = out_idx,
    benchmark        = benchmark)
  df1[["__popup__"]] = popup_text

  if (!is.null(benchmark)) {
    cats1 = benchmark$category
    cats1[out_idx]  = "Out of range"
    cats1[is.na(cats1)] = "Out of range"
    df1[["__sig_category__"]] = cats1

    title1 = "Statistically significant differences"

    out1 = target %>%
      mapgl::add_fill_layer(
        id                 = "acs",
        source             = df1,
        fill_color         = mapgl::match_expr(
          column  = "__sig_category__",
          values  = c("Larger", "Smaller", "Not significant"),
          stops   = c(.benchmark_colors[["Larger"]],
                      .benchmark_colors[["Smaller"]],
                      .benchmark_colors[["Not significant"]]),
          default = .out_of_range_color),
        fill_opacity       = 0.7,
        fill_outline_color = "#ffffff",
        popup              = "__popup__") %>%
      mapgl::add_legend(
        legend_title = title1,
        values       = c("Larger", "Smaller", "Not significant"),
        colors       = c(.benchmark_colors[["Larger"]],
                         .benchmark_colors[["Smaller"]],
                         .benchmark_colors[["Not significant"]]),
        type         = "categorical",
        ## top-left is occupied by the draw toolbar; keep the legend clear of it.
        position     = "top-right",
        style        = list(background_opacity = 0.95))
    return(out1)
  }

  display_col = "__display_value__"
  df1[[display_col]] = df1[[info1$var]]
  if (length(out_idx) > 0) df1[[display_col]][out_idx] = NA_real_

  if (is.null(stops1)) {
    return(mapgl::add_fill_layer(
      target,
      id           = "acs",
      source       = df1,
      fill_color   = .out_of_range_color,
      fill_opacity = 0.7,
      popup        = "__popup__"))
  }

  out1 = target %>%
    mapgl::add_fill_layer(
      id                 = "acs",
      source             = df1,
      fill_color         = mapgl::interpolate(
        column   = display_col,
        values   = stops1$stops,
        stops    = stops1$colors,
        na_color = .out_of_range_color),
      fill_opacity       = 0.7,
      fill_outline_color = "#ffffff",
      popup              = "__popup__") %>%
    mapgl::add_legend(
      legend_title = info1$label,
      values       = format_value(range(stops1$stops), info1$fmt),
      colors       = stops1$colors,
      type         = "continuous",
      ## top-left is occupied by the draw toolbar; keep the legend clear of it.
      position     = "top-right",
      style        = list(background_opacity = 0.95))

  out1
}

utils::globalVariables(c("calculated_variable", "variable_type"))
