#' Render a vgspec as a live mosaic vgplot
#'
#' Renders the spec in the browser using a client-side DuckDB (via
#' duckdb-wasm). Most of the JS runtime this needs (mosaic-spec/mosaic-core)
#' is vendored with the package (`inst/htmlwidgets/lib/`), so it works
#' offline out of the box. The one exception is duckdb-wasm's actual
#' database engine -- a ~35 MB compiled WebAssembly binary, too large to
#' ship with the package -- which is fetched from a CDN each time a plot is
#' *viewed*, unless you've called [vg_cache_duckdb()] to cache it locally
#' (in which case that cached copy is used instead, and no CDN is involved
#' at all). If the cache doesn't exist, this function will offer to set it
#' up for you the first time you call it in an interactive session; see
#' [vg_cache_duckdb()] for details, including how to silence that offer.
#'
#' The relationship between this function, `print()`, and mosaic's own
#' `publish()` naming (see `design/api-brainstorming.qmd`) is still an open
#' design question; `vg_render()` is a placeholder name.
#'
#' @param spec A `vgspec` with a layout of plots/vconcat()/hconcat().
#' @param width,height Widget sizing, in CSS units (e.g. `"100%"`) or pixels.
#' @param elementId Optional DOM element ID for the widget.
#' @param use_cache Whether this plot should use the local duckdb-wasm engine
#'   cache (see [vg_cache_duckdb()]) if one exists. Defaults to whatever
#'   [vg_duckdb_cache_status()] currently reports, so it tracks the cache
#'   automatically; set to `FALSE` to force this one plot to fetch the engine
#'   from the CDN even when a cache is present, or `TRUE` to request the
#'   cache explicitly (harmless, and equivalent to the default, when no cache
#'   exists -- it just falls back to the CDN).
#' @family duckdb caching functions
#' @export
vg_render <- function(
  spec,
  width = NULL,
  height = NULL,
  elementId = NULL,
  use_cache = vg_duckdb_cache_status()$cached
) {
  maybe_offer_duckdb_cache()

  payload <- as_spec_payload(spec)
  x <- list(spec = payload$spec, tables = payload$tables, files = payload$files)
  # Data frames in `tables` need to become arrays of row objects in JSON
  # (what the JS side expects), not htmlwidgets' columnar default. NULL
  # needs to become JSON `null` (jsonlite's default turns it into `{}`),
  # which matters for zero-argument transforms like vg_count()/vg_rank().
  attr(x, "TOJSON_ARGS") <- list(dataframe = "rows", null = "null")

  # `use_cache`'s default is a promise that isn't forced until here, i.e.
  # *after* maybe_offer_duckdb_cache() above may have just created the
  # cache -- so a first-ever call that accepts the offer still uses it
  # immediately, in the same render, rather than only from the next call on.
  dep <- if (isTRUE(use_cache)) vg_duckdb_cache_dependency() else NULL

  htmlwidgets::createWidget(
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
}
