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
#' ship with the package -- which is fetched from a CDN each time a plot is
#' *viewed*, unless you've called [vg_cache_duckdb()] to cache it locally
#' (in which case that cached copy is used instead, and no CDN is involved
#' at all). If the cache doesn't exist, this function will offer to set it
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
#'   e.g. copied from mosaic's own example gallery, or the output of
#'   [to_json()]/[to_yaml()] -- to render it directly without building it up
#'   through `vg_*()` calls first. Format is auto-detected (JSON if the
#'   trimmed text starts with an opening brace or bracket, YAML otherwise).
#'   Data references in
#'   such a spec are left exactly as given -- unlike a `vgspec`'s own local
#'   `file =`/inline-data-frame sources, nothing here is read or embedded,
#'   so a local file path needs to actually be fetchable (an http(s) URL,
#'   or a path relative to wherever the rendered page is ultimately opened
#'   from) for duckdb-wasm to load it in the browser.
#' @param width,height Widget sizing, in CSS units (e.g. `"100%"`) or pixels.
#' @param elementId Optional DOM element ID for the widget.
#' @param use_cache Whether this plot should use the local duckdb-wasm engine
#'   cache (see [vg_cache_duckdb()]) if one exists. Defaults to whatever
#'   [vg_duckdb_cache_status()] currently reports, so it tracks the cache
#'   automatically; set to `FALSE` to force this one plot to fetch the engine
#'   from the CDN even when a cache is present, or `TRUE` to request the
#'   cache explicitly (harmless, and equivalent to the default, when no cache
#'   exists -- it just falls back to the CDN). Ignored when `connector` isn't
#'   the default (there's no duckdb-wasm engine to cache).
#' @param connector Which database this plot's SQL actually runs against:
#'   [vg_wasm_connector()] (the default -- DuckDB-Wasm in the browser, fully
#'   self-contained) or [vg_duckdb_connector()] (a real, native DuckDB,
#'   reached over a local server started automatically -- see its docs for
#'   when you'd want this and its sharing/security caveats).
#' @param ... Not used by `vg_widget()` itself. Accepted (and silently
#'   ignored) so that [vg_render()] can forward its own `...` uniformly to
#'   whichever of [vg_widget()]/[vg_snapshot()]/[vg_iframe()] ends up
#'   handling a request, without erroring on arguments meant for one of
#'   the other two (e.g. `delay =`, only meaningful for [vg_snapshot()]).
#' @family rendering functions
#' @seealso [vg_cache_duckdb()], [vg_duckdb_cache_status()]
#' @export
vg_widget <- function(
  spec,
  width = NULL,
  height = NULL,
  elementId = NULL,
  use_cache = vg_duckdb_cache_status()$cached,
  connector = vg_wasm_connector(),
  ...
) {
  if (!is_vg_connector(connector)) {
    stop("`connector` must be vg_wasm_connector() or vg_duckdb_connector().", call. = FALSE)
  }

  payload <- as_spec_payload(spec)

  if (inherits(connector, "vg_duckdb_connector")) {
    server <- ensure_vg_duckdb_server(connector)
    if (is_vgspec(spec)) register_native_data_sources(spec, server$con)
    x <- list(spec = payload$spec, tables = list(), files = list(),
              connector = list(type = "rest", uri = server$uri))
    dep <- NULL
  } else {
    maybe_offer_duckdb_cache()
    x <- list(spec = payload$spec, tables = payload$tables, files = payload$files)
    # `use_cache`'s default is a promise that isn't forced until here, i.e.
    # *after* maybe_offer_duckdb_cache() above may have just created the
    # cache -- so a first-ever call that accepts the offer still uses it
    # immediately, in the same render, rather than only from the next call on.
    dep <- if (isTRUE(use_cache)) vg_duckdb_cache_dependency() else NULL
  }
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
