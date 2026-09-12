#' Render a vgspec as a live mosaic vgplot
#'
#' Loads the mosaic JS runtime and a client-side DuckDB (via duckdb-wasm)
#' from a CDN, then parses and renders the spec in the browser. This is a
#' first working version, not the final architecture: it fetches the JS
#' runtime from esm.sh at view time (see `inst/htmlwidgets/vgplotr.js`)
#' rather than from locally vendored assets, so viewing the result requires
#' an internet connection. Vendoring the runtime for offline/reproducible
#' use is planned but not done yet.
#'
#' The relationship between this function, `print()`, and mosaic's own
#' `publish()` naming (see `design/api-brainstorming.qmd`) is still an open
#' design question; `vg_render()` is a placeholder name.
#'
#' @param spec A `vgspec` with a layout of plots/vconcat()/hconcat().
#' @param width,height Widget sizing, in CSS units (e.g. `"100%"`) or pixels.
#' @param elementId Optional DOM element ID for the widget.
#' @export
vg_render <- function(spec, width = NULL, height = NULL, elementId = NULL) {
  payload <- as_spec_payload(spec)
  x <- list(spec = payload$spec, tables = payload$tables, files = payload$files)
  # Data frames in `tables` need to become arrays of row objects in JSON
  # (what the JS side expects), not htmlwidgets' columnar default. NULL
  # needs to become JSON `null` (jsonlite's default turns it into `{}`),
  # which matters for zero-argument transforms like vg_count()/vg_rank().
  attr(x, "TOJSON_ARGS") <- list(dataframe = "rows", null = "null")

  htmlwidgets::createWidget(
    name = "vgplotr",
    x = x,
    width = width,
    height = height,
    elementId = elementId,
    package = "vgplotr",
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.suppress = TRUE,
      browser.fill = FALSE,
      knitr.figure = FALSE
    )
  )
}
