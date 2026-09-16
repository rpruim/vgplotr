#' Render a vgspec live or as a static screenshot, whichever the current
#' output format can actually display
#'
#' [vg_render()]'s widget can't survive being embedded as plain
#' markdown/PDF/Word text -- those formats can't run JavaScript, so a
#' static image is needed instead there. Unfortunately, knitr/rmarkdown's
#' own built-in mechanism for turning a widget into a static screenshot
#' (used e.g. for `output: github_document`) doesn't work for vgplotr
#' specifically: it captures the widget by opening it as a local file
#' (`file://`), and Chrome refuses to load an ES module script (vgplotr's
#' JS runtime is one) -- or run `fetch()` at all -- under `file://`, for
#' security reasons. Confirmed directly: this produces a blank screenshot
#' no matter how long you wait, on both the legacy `webshot` (PhantomJS)
#' and modern `webshot2` (real headless Chrome) backends.
#'
#' `vg_snapshot()` works around this the way a real deployment would:
#' it serves the widget over a local HTTP server (via the `httpuv`
#' package) before screenshotting it (via the `webshot2` package), which
#' lets the widget's JS actually run. Use `vg_snapshot(spec)` anywhere
#' you'd otherwise write `vg_render(spec)` in a document meant to be
#' knitted to more than one kind of output -- it renders live when the
#' current output format supports embedded JavaScript (an HTML
#' vignette, a Quarto HTML page, or no active knit at all, e.g. typed at
#' the console -- so it behaves exactly like [vg_render()] there), and
#' falls back to a static screenshot otherwise (`github_document`, PDF,
#' Word, ...). This is what lets a single `README.Rmd` serve as both the
#' GitHub README (`github_document`, screenshotted) and an HTML page
#' (live widgets) from the same source.
#'
#' `iframe = TRUE` forces a third option: the widget is saved as a real,
#' non-self-contained HTML file (under the same `fig.path` convention
#' used for the screenshot PNGs, e.g. `man/figures/README-*.html`) and
#' embedded as a live `<iframe>` -- raw HTML, which markdown/gfm passes
#' through untouched. This is specifically for a pkgdown home page built
#' from `pkgdown/index.md` rather than `README.md`: pkgdown always
#' copies `man/figures/` into `docs/reference/figures/` (confirmed
#' directly, for any file type, not just images), and the deployed page
#' is served over real HTTPS, so the `file://` restriction that makes
#' screenshotting necessary in the first place doesn't apply -- the
#' iframe's widget genuinely runs live once deployed. It won't run
#' correctly if you preview the raw `.md`/`.html` output locally via
#' `file://`, only once actually served over HTTP(S) (e.g. via
#' `pkgdown::build_site()` + serving `docs/`, or the real deployed site).
#'
#' @param spec A `vgspec`, a JSON/YAML spec string (see [vg_render()]),
#'   or an already-built widget (i.e. the result of calling
#'   [vg_render()] yourself).
#' @param file If given, always save to this exact path (a PNG, unless
#'   `iframe = TRUE`, in which case an HTML file) instead of the usual
#'   `fig.path` convention, and return the path invisibly instead of an
#'   embeddable result -- for one-off or scripted use outside a
#'   document. Ignored (with a live widget returned instead) when the
#'   current output format can embed one and `iframe` is `FALSE`.
#' @param delay Seconds to wait after the widget loads before taking the
#'   screenshot, to give duckdb-wasm and mosaic time to fetch data and
#'   draw. 3 seconds is enough for a small/local data source; a large or
#'   remote one may need more. Not used when `iframe = TRUE` (nothing is
#'   screenshotted).
#' @param vwidth,vheight Browser viewport size (pixels) for the
#'   screenshot, or the `<iframe>`'s own width/height when
#'   `iframe = TRUE`.
#' @param iframe If `TRUE`, always embed a live widget via an `<iframe>`
#'   rather than deciding live-vs-static from the output format -- see
#'   Details.
#' @param ... Passed on to [vg_render()] when `spec` isn't already a
#'   built widget (e.g. `width =`, `height =`).
#' @family duckdb caching functions
#' @export
vg_snapshot <- function(spec, file = NULL, delay = 3, vwidth = 992, vheight = 744, iframe = FALSE, ...) {
  dots <- list(...)
  # iframe mode saves the widget to a *committed* file (man/figures/), so
  # unlike a scratch screenshot, the local duckdb-wasm cache -- if this
  # machine happens to have one -- must not get embedded: htmlwidgets
  # bundles a cached local dependency as a real copy of the file, and
  # duckdb-wasm's engine is a ~35 MB binary. Default to fetching it from
  # the CDN at view time instead (like every other example in this
  # package), unless the caller explicitly asked for caching anyway.
  if (iframe && !inherits(spec, "htmlwidget") && is.null(dots$use_cache)) {
    dots$use_cache <- FALSE
  }
  widget <- if (inherits(spec, "htmlwidget")) spec else do.call(vg_render, c(list(spec), dots))

  if (iframe) {
    return(vg_snapshot_iframe(widget, file, vwidth = vwidth, vheight = vheight))
  }

  explicit_file <- !is.null(file)
  if (!explicit_file && can_embed_live_widget()) {
    return(widget)
  }

  out <- if (explicit_file) file else default_snapshot_path(".png")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  vg_snapshot_capture(widget, out, delay = delay, vwidth = vwidth, vheight = vheight)

  if (explicit_file) {
    return(invisible(out))
  }
  knitr::include_graphics(out)
}

# Whether the current rendering context (if any) can actually execute
# embedded JavaScript. Deliberately narrower than knitr::is_html_output()
# -- that function also counts "markdown"/"gfm" as HTML output (they do
# support *passing through* raw HTML blocks), but GitHub sanitizes
# <script> tags out of a rendered README, so a live widget still wouldn't
# actually run there. No active knit at all (e.g. called at the console)
# defaults to TRUE, matching vg_render()'s normal interactive behavior.
can_embed_live_widget <- function() {
  if (!isTRUE(getOption("knitr.in.progress", FALSE))) return(TRUE)
  if (!requireNamespace("knitr", quietly = TRUE)) return(TRUE)
  fmt <- knitr::pandoc_to()
  if (length(fmt) == 0) return(TRUE)
  fmt %in% c("html", "html4", "html5", "revealjs", "s5", "slideous", "slidy")
}

# Where to save a snapshot when the caller didn't give an explicit `file`:
# knitr's own fig.path/label/counter convention when there's an active
# knit (so output lands in the usual place, e.g. man/figures/README-*),
# otherwise a plain tempfile.
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

# Saves `widget` as a standalone (non-self-contained) HTML file under the
# man/figures/ convention and returns a raw <iframe> HTML block pointing
# at it -- see vg_snapshot()'s `iframe` argument. The src is deliberately
# `man/figures/<x>` rewritten to `reference/figures/<x>`, matching exactly
# where pkgdown::build_reference() copies man/figures/ contents to
# (confirmed directly) -- this only makes sense embedded in a pkgdown
# page, not a generic document.
vg_snapshot_iframe <- function(widget, file, vwidth, vheight) {
  out <- if (!is.null(file)) file else default_snapshot_path(".html")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(widget, out, selfcontained = FALSE)

  src <- sub("^man/figures/", "reference/figures/", out)
  knitr::asis_output(sprintf(
    '<iframe src="%s" width="%s" height="%s" style="border: none;" loading="lazy"></iframe>',
    src, vwidth, vheight
  ))
}
