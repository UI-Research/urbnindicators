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
#'   is used in popups when present. When supplied, a "Geography" toggle
#'   appears in the sidebar that switches the map between the source and the
#'   target dataset. Benchmark values remain computed from the source data;
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
  target_codebook = if (is.null(target_data1)) NULL else attr(target_data1, "codebook")
  has_target = !is.null(target_data1)

  app1 = shiny::shinyApp(
    ui     = .view_acs_ui(choices1, has_multi_year, data1, benchmark_levels,
                          has_target),
    server = .view_acs_server(data1, codebook, choices1, has_multi_year,
                              geography, benchmark_levels,
                              target_data1, target_codebook,
                              target_parent_lookup))

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

## Interpolate source ACS data onto target_geographies via an area-weighted
## crosswalk. Returns a list with `data` (an sf object with the codebook
## attached as an attribute) and `parent_lookup` (the target→parent map for
## benchmarking). Both are NULL when target_geographies is NULL.
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

  crosswalk1 = build_spatial_crosswalk(.data, target_geographies)

  ## interpolate_acs() takes source_geoid (default "GEOID") and joins to the
  ## crosswalk via that name; the crosswalk's target column must be uniquely
  ## named so it doesn't collide with the source GEOID.
  xwalk_for_interp = crosswalk1 %>%
    dplyr::transmute(
      GEOID        = .data$source_geoid,
      target_GEOID = .data$target_geoid,
      share        = .data$share)

  interp1 = interpolate_acs(
    .data                 = sf::st_drop_geometry(.data),
    target_geoid_column   = "target_GEOID",
    weight                = "share",
    crosswalk             = xwalk_for_interp)
  interp_codebook = attr(interp1, "codebook")

  geom1 = target_geographies %>%
    dplyr::distinct(.data$GEOID, .keep_all = TRUE) %>%
    sf::st_transform(4326)

  ## inner_join preserves sf-ness on the LHS; do the geometry side first so
  ## the result remains sf. dplyr::inner_join.sf strips non-codebook attrs.
  joined1 = geom1 %>%
    dplyr::inner_join(interp1, by = "GEOID")
  attr(joined1, "codebook") = interp_codebook

  parent_lookup = compute_target_parent_map(crosswalk1, benchmark_levels)

  list(data = joined1, parent_lookup = parent_lookup)
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

## Build a per-row HTML popup string. When `out_of_range_idx` is supplied,
## those rows get an additional note in their popup. When `benchmark` is
## non-NULL, popups gain a line showing the benchmark value and significance
## category.
make_popup_html = function(data1, var1, moe_var, fmt, label1,
                           out_of_range_idx = NULL,
                           benchmark = NULL) {
  name1   = if ("NAME" %in% colnames(data1)) data1[["NAME"]] else data1[["GEOID"]]
  value1  = data1[[var1]]
  value_s = format_value(value1, fmt)

  base1 = if (!is.null(moe_var) && moe_var %in% colnames(data1)) {
    moe_s = format_value(data1[[moe_var]], fmt)
    paste0(
      "<strong>", htmltools_escape(name1), "</strong><br/>",
      htmltools_escape(label1), ": ", value_s,
      " (± ", moe_s, ")")
  } else {
    paste0(
      "<strong>", htmltools_escape(name1), "</strong><br/>",
      htmltools_escape(label1), ": ", value_s)
  }

  if (!is.null(benchmark)) {
    bench_label = paste0("vs. ", benchmark$label)
    bench_s = format_value(benchmark$values, fmt)
    sig1 = benchmark$category
    sig_note = dplyr::case_when(
      is.na(sig1)                ~ "no comparison possible",
      sig1 == "Larger"           ~ "significantly larger",
      sig1 == "Smaller"          ~ "significantly smaller",
      sig1 == "Not significant"  ~ "not significantly different",
      TRUE                       ~ NA_character_)
    base1 = paste0(
      base1, "<br/>",
      htmltools_escape(bench_label), ": ", bench_s,
      " <em style=\"color:#5c5859;\">(", sig_note, ")</em>")
  }

  if (!is.null(out_of_range_idx) && length(out_of_range_idx) > 0) {
    base1[out_of_range_idx] = paste0(
      base1[out_of_range_idx],
      "<br/><em style=\"color:#5c5859;\">Outside selected range</em>")
  }
  base1
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

.view_acs_ui = function(choices1, has_multi_year, data1, benchmark_levels,
                        has_target = FALSE) {
  year_choices = if (has_multi_year) sort(unique(data1$data_source_year)) else NULL

  sidebar1 = bslib::sidebar(
    width = 320,
    if (has_target)
      shiny::selectInput("geo_view",
        label   = "Geography",
        choices = c("Source", "Target"),
        selected = "Source")
      else NULL,
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
        style = "font-size: 0.8em; color: #5c5859; margin-top: -0.5em;",
        shiny::textOutput("benchmark_note", inline = FALSE))
      else NULL,
    shiny::tags$div(
      style = "margin-top: 0.5em; font-size: 0.85em; color: #000000;",
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
    shiny::tags$head(shiny::tags$style(shiny::HTML(.legend_css))),
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

.view_acs_server = function(data1, codebook, choices1, has_multi_year,
                            geography, benchmark_levels,
                            target_data1 = NULL, target_codebook = NULL,
                            target_parent_lookup = NULL) {
  has_target = !is.null(target_data1)

  function(input, output, session) {

    active_dataset = shiny::reactive({
      view1 = if (has_target) (input$geo_view %||% "Source") else "Source"
      if (view1 == "Target") {
        list(data = target_data1, codebook = target_codebook, view = "target")
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
      lookup1 = if (ds$view == "target") target_parent_lookup else NULL
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
      m1 = mapgl::maplibre(
        style  = mapgl::carto_style("positron"),
        bounds = sf::st_bbox(df1))
      .add_choropleth_layer(m1, df1, info1, stops1, sel1, bm1)
    })

    ## ----- Updates: redraw the layer on any of these changing -----
    shiny::observeEvent(
      list(input$variable, input$year, input$hist_brush, input$benchmark,
           input$geo_view),
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

    ## When the geo_view toggle changes, also refit the bounds so the user
    ## sees the new dataset's full extent.
    if (has_target) {
      shiny::observeEvent(input$geo_view, ignoreInit = TRUE, {
        df1 = filtered_data()
        bbox1 = sf::st_bbox(df1)
        if (all(is.finite(bbox1))) {
          mapgl::fit_bounds(mapgl::maplibre_proxy("map"), bbox1)
        }
      })
    }
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

    title1 = paste0(info1$label, " vs. ", benchmark$label, " (90% CI)")

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
      style        = list(background_opacity = 0.95))

  out1
}

utils::globalVariables(c("calculated_variable", "variable_type"))
