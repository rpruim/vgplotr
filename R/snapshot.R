#' Render a vgspec as a static screenshot
#'
#' Renders `spec` as a static PNG.  Useful when
#' [vg_widget()]'s live widget doesn't work because
#' the output format can't run JavaScript
#' (e.g., plain markdown, PDF, Word, ...).
#' For a document that should render *live* when possible and
#' fall back to a screenshot only when it has to,
#' see [vg_render()].
#'
#' @details
#' knitr/rmarkdown's own built-in mechanism for turning a widget into a
#' static screenshot (used e.g. for `output: github_document`) doesn't
#' work for vgplotr: it captures the widget by opening it as a local file
#' (`file://`), and Chrome refuses to load an ES module script (vgplotr's
#' JS runtime is one) -- or run `fetch()` at all -- under `file://`, for
#' security reasons. The result is a blank screenshot no matter how long
#' you wait, on both the legacy `webshot` (PhantomJS) and modern
#' `webshot2` (real headless Chrome) backends.
#'
#' `vg_snapshot()` works around this by serving the widget over a local
#' HTTP server (via the `httpuv` package) before screenshotting it (via
#' the `webshot2` package), which lets the widget's JS actually run.
#'
#' @param spec A `vgspec`, a JSON/YAML spec string (see [vg_widget()]),
#'   or an already-built widget (i.e. the result of calling
#'   [vg_widget()] yourself).
#' @param file If given, always save to this exact path and return it
#'   invisibly -- for one-off or scripted use outside a document. Left
#'   `NULL` (the default), the screenshot is saved under the usual
#'   `fig.path` convention (e.g. `man/figures/README-*.png`) and the
#'   result is a [knitr::include_graphics()] value meant to be the
#'   value of a knitted chunk.
#' @param delay Seconds to wait after the widget loads before taking the
#'   screenshot, to give duckdb-wasm and mosaic time to fetch data and
#'   draw. 3 seconds is enough for a small/local data source; a large or
#'   remote one may need more.
#' @param vwidth,vheight Browser viewport size (pixels) for the screenshot.
#' @param ... Passed on to [vg_widget()] when `spec` isn't already a
#'   built widget (e.g. `width =`, `height =`). Also silently absorbs
#'   arguments meant for [vg_iframe()] instead (e.g. `iframe`-only
#'   options), so [vg_render()] can forward its own `...` uniformly.
#' @family rendering functions
#' @export
vg_snapshot <- function(
  spec,
  file = NULL,
  delay = 3,
  vwidth = 992,
  vheight = 744,
  ...
) {
  widget <- if (inherits(spec, "htmlwidget")) spec else vg_widget(spec, ...)

  explicit_file <- !is.null(file)
  out <- if (explicit_file) file else default_snapshot_path(".png")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  vg_snapshot_capture(
    widget,
    out,
    delay = delay,
    vwidth = vwidth,
    vheight = vheight
  )

  if (explicit_file) {
    return(invisible(out))
  }
  knitr::include_graphics(out)
}

# Where to save a snapshot/iframe file when the caller didn't give an
# explicit `file`: knitr's own fig.path/label/counter convention when
# there's an active knit (so output lands in the usual place, e.g.
# man/figures/README-*), otherwise a plain tempfile.
default_snapshot_path <- function(ext) {
  if (isTRUE(getOption("knitr.in.progress", FALSE)) && requireNamespace("knitr", quietly = TRUE)) {
    knitr::fig_path(ext)
  } else {
    tempfile(fileext = ext)
  }
}

# Serves `widget` over a local HTTP server and screenshots it with
# webshot2 -- the actual file:// workaround described in vg_snapshot()'s
# docs above.
vg_snapshot_capture <- function(widget, out, delay, vwidth, vheight) {
  if (!requireNamespace("httpuv", quietly = TRUE) || !requireNamespace("webshot2", quietly = TRUE)) {
    stop(
      "vg_snapshot() needs the httpuv and webshot2 packages to render a ",
      "static screenshot (install.packages(c(\"httpuv\", \"webshot2\"))).",
      call. = FALSE
    )
  }

  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  html_path <- file.path(dir, "widget.html")
  htmlwidgets::saveWidget(widget, html_path, selfcontained = FALSE)

  port <- httpuv::randomPort()
  server <- httpuv::startServer(
    "127.0.0.1", port,
    list(staticPaths = list("/" = httpuv::staticPath(dir)))
  )
  on.exit(httpuv::stopServer(server), add = TRUE)

  url <- paste0("http://127.0.0.1:", port, "/widget.html")
  webshot2::webshot(url, out, delay = delay, vwidth = vwidth, vheight = vheight, quiet = TRUE)
  invisible(out)
}
