# A top-level `width`/`height` (`payload$spec$width`/`$height` --
# spec$attrs from vg_attributes(), or an already-complete string spec's own
# top-level keys; see as_spec_payload()) becomes vg_widget()'s own default
# container size only when it's a single plain number. mosaic-spec also
# allows a ParamRef there (a reactive `param()`-driven value, deserialized
# as a list, e.g. `list(param = "myWidth")`) -- not a fixed size to copy,
# so that (and a missing key, NULL) both fall through to NULL here, leaving
# htmlwidgets' own default sizing in charge exactly as before this existed.
spec_top_level_size <- function(x) {
  if (is.numeric(x) && length(x) == 1) x else NULL
}

#' Build a vgspec into a live mosaic vgplot widget
#'
#' Returns a live, interactive `htmlwidget` -- useful directly
#' when you know the result will be viewed somewhere JavaScript can
#' run (the console, RStudio's viewer, an HTML vignette, a Quarto HTML
#' page). For a document that might *also* be knitted to an output
#' that can't run JavaScript (plain markdown, PDF, Word, ...),
#' see [vg_render()], which picks automatically between this function, [vg_snapshot()],
#' and [vg_iframe()].
#'
#' Renders the spec in the browser using a client-side DuckDB (via
#' duckdb-wasm). Most of the JS runtime this needs
#' (mosaic-spec/mosaic-core) is vendored with the package
#' (`inst/htmlwidgets/lib/`), so it works offline out of the box.
#' The one exception is duckdb-wasm's actual database engine --
#' a ~35 MB compiled WebAssembly binary, too large to
#' ship with the package -- which is fetched from a CDN each time a graphic
#' is *viewed*, unless you've called [vg_cache_duckdb()] to cache it locally
#' (in which case that cached copy is used instead, and no CDN is involved
#' at all -- except on a page opened directly from disk (`file://`), which
#' browsers don't allow to load a local engine, so it uses the CDN anyway).
#' If the cache doesn't exist, this function will offer to set it
#' up for you the first time you call it in an interactive session; see
#' [vg_cache_duckdb()] for details, including how to silence that offer.
#'
#' Mosaic-spec's own `meta` (title/description/credit -- see [vg_meta()])
#' isn't rendered by mosaic's JS runtime at all; it's inert, spec-level
#' metadata (`@uwdata/mosaic-spec`'s own
#' `astToDOM()` never reads it). Since a title is nonetheless the
#' most commonly expected use of it, `vg_widget()` renders `meta$title`
#' itself -- as a caption prepended above the widget -- as a vgplotr-level
#' convenience layered on top of what mosaic itself does; `description`/
#' `credit` remain inert for now.
#'
#' @param spec A `vgspec` with a layout of plots/vconcat()/hconcat(), or a
#'   single JSON or YAML string holding an already-complete mosaic spec --
#'   e.g., copied from mosaic's own example gallery, or the output of
#'   [to_json()]/[to_yaml()] -- to render it directly without building it up
#'   through `vg_*()` calls first. Format is auto-detected (JSON if the
#'   trimmed text starts with an opening brace or bracket, YAML otherwise).
#'   Inline data -- a `data:` entry holding an array of row objects, the
#'   form [to_json()]/[to_yaml()] write a data frame as -- is loaded
#'   automatically. Any other data reference in such a spec is left exactly
#'   as given -- unlike a `vgspec`'s own local `file =` sources, nothing
#'   is read or embedded, so a local file path needs to actually be
#'   fetchable (an http(s) URL, or a path relative to wherever the
#'   rendered page is ultimately opened from) for duckdb-wasm to load it
#'   in the browser. (An inline array that also has `where`/`select`
#'   options is one of these: it's handed to mosaic as-is.)
#' @param width,height Widget sizing, in CSS units (e.g., `"100%"`) or pixels.
#'   Left `NULL` (the default), this falls back to the spec's own top-level
#'   `width`/`height` -- set via [vg_attributes()], or already present as
#'   top-level keys in a JSON/YAML string spec -- when that's a plain
#'   number, so the widget's own size matches the graphic's by default
#'   instead of the two silently disagreeing. A `param()`-driven `width`/
#'   `height` (reactive, no fixed number) is left alone, the same as when
#'   the spec sets neither at all -- pass an explicit value here to size
#'   the widget in either case.
#' @param elementId Optional DOM element ID for the widget.
#' @param use_cache Whether this graphic should use the local duckdb-wasm
#'   engine cache (see [vg_cache_duckdb()]) if one exists. Defaults to
#'   `getOption("vgplotr.use_cache")` if that option is set, or otherwise
#'   whatever [vg_duckdb_cache_status()] currently reports, so it tracks the
#'   cache automatically; set to `FALSE` to force this one graphic to fetch
#'   the engine from the CDN even when a cache is present, or `TRUE` to
#'   request the cache explicitly (harmless, and equivalent to the default,
#'   when no cache exists -- it just falls back to the CDN). Ignored when
#'   `connector` isn't the default (there's no duckdb-wasm engine to cache).
#'
#'   The `vgplotr.use_cache` option is meant for a whole document rather
#'   than one call -- e.g., a vignette's setup chunk setting
#'   `options(vgplotr.use_cache = FALSE)` so its *built* output
#'   always references the CDN, regardless of whether the machine that
#'   happens to build it has a local cache. That matters because
#'   self-contained HTML embeds a cache directly as base64, increasing
#'   the size of file substantially.
#' @param connector Which database this graphic's SQL actually runs against:
#'   [vg_wasm_connector()] (the default -- DuckDB-Wasm in the browser, fully
#'   self-contained) or [vg_duckdb_connector()] (a real, native DuckDB,
#'   reached over a local server started automatically -- see its docs for
#'   when you'd want this and its sharing/security caveats). Defaults to
#'   the session's default connector ([vg_set_default_connector()]), so you
#'   don't need to repeat this on every call once you've set one.
#' @param coordinator Options for mosaic's client-side query coordinator (its
#'   caching, query consolidation, pre-aggregation and console logging), made
#'   with [vg_coordinator()]. Defaults to `getOption("vgplotr.coordinator")`,
#'   so `options(vgplotr.coordinator = vg_coordinator(logging = "errors"))`
#'   sets it for a whole document; `NULL` leaves mosaic's own defaults. The
#'   first widget on a page (using a given `connector`) to render creates the
#'   coordinator, and its options are the ones used (see [vg_coordinator()]).
#' @param link Optional name of a *link group*, a single string. Live widgets
#'   on the same web page that give the same name share their params and
#'   selections, so a brush made in one plot filters the plots in the others
#'   -- separate `vg_render()` calls, e.g. one per Quarto chunk, behaving like
#'   one [vg_vconcat()]/[vg_hconcat()]. See the "Linking widgets" section of
#'   `vignette("getting-started")`. The default `NULL` keeps a widget on its
#'   own, as before.
#'
#'   How it works, and what it needs from you: a param/selection is shared by
#'   *name*. Declare it (see [vg_params()]) the same way in every widget that
#'   declares it; if two widgets in a group declare one name differently, the
#'   first declaration wins and the browser console shows a warning. A widget
#'   that only uses `param(brush)` (e.g., in `filter_by`) picks up whatever
#'   the group declares, whichever renders first. Each widget must still
#'   include the data its own plots use ([vg_data()]), under the same name
#'   and with the same contents as the others -- linking shares selections,
#'   not tables. The widgets must use the same `connector`. Only live widgets
#'   in the same page can link: separate documents (see [vg_iframe()]) and
#'   static [vg_snapshot()] images cannot.
#' @param ... Not used by `vg_widget()` itself. Accepted (and silently
#'   ignored) so that [vg_render()] can forward its own `...` uniformly to
#'   whichever of [vg_widget()]/[vg_snapshot()]/[vg_iframe()] ends up
#'   handling a request, without erroring on arguments meant for one of
#'   the other two (e.g., `delay =`, only meaningful for [vg_snapshot()]).
#' @family rendering functions
#' @seealso [vg_cache_duckdb()], [vg_duckdb_cache_status()]
#' @export
vg_widget <- function(
  spec,
  width = NULL,
  height = NULL,
  elementId = NULL,
  use_cache = getOption("vgplotr.use_cache", vg_duckdb_cache_status()$cached),
  connector = vg_default_connector(),
  coordinator = getOption("vgplotr.coordinator"),
  link = NULL,
  ...
) {
  if (!is_vg_connector(connector)) {
    stop("`connector` must be vg_wasm_connector() or vg_duckdb_connector().", call. = FALSE)
  }
  if (!is.null(coordinator) && !is_vg_coordinator(coordinator)) {
    stop("`coordinator` must be NULL or the result of vg_coordinator().", call. = FALSE)
  }
  if (!is.null(link) && !(is.character(link) && length(link) == 1 && !is.na(link) && nzchar(link))) {
    stop("`link` must be NULL or a single, non-empty string naming a link group.", call. = FALSE)
  }

  payload <- as_spec_payload(spec)

  if (is.null(width)) width <- spec_top_level_size(payload$spec$width)
  if (is.null(height)) height <- spec_top_level_size(payload$spec$height)

  if (inherits(connector, "vg_duckdb_connector")) {
    server <- ensure_vg_duckdb_server(connector)
    if (is_vgspec(spec)) {
      register_native_data_sources(spec, server$con)
    } else {
      register_native_inline_tables(payload$tables, server$con)
    }
    x <- list(spec = payload$spec, tables = list(), files = list(),
              connector = list(type = "rest", uri = server$uri))
    dep <- NULL
  } else {
    maybe_offer_duckdb_cache()
    x <- list(spec = payload$spec, tables = payload$tables, files = payload$files)
    # `use_cache`'s default is a promise that isn't forced until here, i.e.,
    # *after* maybe_offer_duckdb_cache() above may have just created the
    # cache -- so a first-ever call that accepts the offer still uses it
    # immediately, in the same render, rather than only from the next call on.
    dep <- if (isTRUE(use_cache)) vg_duckdb_cache_dependency() else NULL
  }
  # Neither is part of the mosaic spec: inst/htmlwidgets/vgplotr.js reads them.
  x$link <- link
  x$coordinator <- if (!is.null(coordinator)) coordinator_payload(coordinator)
  # Data frames in `tables` need to become arrays of row objects in JSON
  # (what the JS side expects), not htmlwidgets' columnar default. NULL
  # needs to become JSON `null` (jsonlite's default turns it into `{}`),
  # which matters for zero-argument transforms like vg_count()/vg_rank().
  attr(x, "TOJSON_ARGS") <- list(dataframe = "rows", null = "null")

  widget <- htmlwidgets::createWidget(
    name = "vgplotr",
    x = x,
    width = width,
    height = height,
    elementId = elementId,
    package = "vgplotr",
    dependencies = if (!is.null(dep)) list(dep),
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.suppress = TRUE,
      browser.fill = FALSE,
      knitr.figure = FALSE
    )
  )

  if (!is.null(payload$spec$meta$title)) {
    widget <- htmlwidgets::prependContent(
      widget,
      htmltools::div(
        payload$spec$meta$title,
        style = "font-weight: 600; font-size: 1.1em; margin-bottom: 0.4em;"
      )
    )
  }

  widget
}
