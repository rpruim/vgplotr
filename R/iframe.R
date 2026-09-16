#' Embed a vgspec as a live widget via an iframe
#'
#' Saves `spec` as a non-self-contained HTML file and
#' returns a raw `<iframe>` HTML block pointing at it.
#' Since formats like markdown and gfm
#' pass raw HTML through untouched, this embeds a genuinely live
#' widget even inside a plain-markdown document, as long as the
#' *page it ends up on* is eventually served over http(s) rather
#' than opened as a local file.
#' (See [vg_snapshot()]'s Details for why `file://` doesn't
#' work for a vgplotr widget).
#'
#' This can be used to include a live interactive graphic
#' on a pkgdown home page built from `pkgdown/index.md` (rather
#' than from `README.md`). pkgdown copies
#' `man/figures/` into `docs/reference/figures/`
#' (for any file type, not just images), so a widget saved under the
#' `fig.path` convention there -- the default when `file` isn't given --
#' ends up served alongside everything else on the deployed site, and
#' `src` defaults accordingly (see `src` below).
#'
#' Note: [vg_render()] never picks this mode automatically
#' (only [vg_widget()] or [vg_snapshot()]), so you must ask for it
#' explicitly, e.g. via `vg_render(spec, mode = "iframe")`. This is
#' because [vg_render()]'s auto-detection works by asking
#' "Can this pandoc target run embedded JavaScript?" — that's a real,
#' fixed property of the output format itself
#' (HTML can, gfm/markdown/PDF/Word can't), so it's reliable.
#' But whether an `<iframe>` is useful depends on
#' whether the rendered file ends up served over http(s),
#' with its sibling reference/figures/*.html file reachable at a
#' predictable relative path. That's not a property of the render at all
#' — it's a fact about what happens to the rendered file afterward,
#' entirely outside the knitting process.
#'
#' @section Using this outside a pkgdown home page:
#' The default `src` handling only makes sense for the pkgdown scenario
#' above: it assumes the widget was saved somewhere under `man/figures/`
#' and will be found at the matching `reference/figures/` path once
#' deployed, because that's specifically what
#' `pkgdown::build_reference()` does. Any other deployment -- a plain
#' Quarto/R Markdown site, a custom build script, a different pkgdown
#' convention -- almost certainly copies or serves files differently, so
#' that guess won't hold.
#'
#' For those cases, set `file` and `src` independently: `file` is where
#' `vg_iframe()` saves the widget *now*, at render time (a path relative
#' to your current working directory, or absolute); `src` is the URL the
#' `<iframe>` should use to find it *later*, from wherever the finished
#' page that embeds it will actually live once deployed -- which is
#' often a different relative path than `file`, if your build process
#' moves, renames, or copies things along the way (as pkgdown does).
#' For example, a Quarto project that copies a `widgets/` resource
#' directory verbatim into `_site/` might do:
#'
#' ```r
#' vg_iframe(spec, file = "widgets/plot1.html", src = "widgets/plot1.html")
#' ```
#'
#' -- here `file` and `src` happen to match because that pipeline
#' preserves relative paths, but they don't have to: whatever your own
#' build does to get a file from its saved location to its served one,
#' `src` should describe the *result* of that, not the `file` path
#' itself.
#'
#' @inheritParams vg_snapshot
#' @param file If given, save to this exact HTML path instead of the
#'   usual `fig.path` convention (e.g. `man/figures/README-*.html`).
#' @param src The `<iframe>`'s `src` attribute -- where the *deployed*
#'   page should look for the saved widget, which is not always the
#'   same as `file` (see the section above). Left `NULL` (the default),
#'   `file` (or the `fig.path` default) gets `man/figures/` rewritten to
#'   `reference/figures/` if present (the pkgdown convention) and is
#'   used as-is otherwise. Ignored -- with the plain save path returned
#'   invisibly instead of an `<iframe>` block -- if neither `src` nor
#'   the default `file` is given (i.e. one-off/scripted use with only
#'   `file` set).
#' @param vwidth,vheight The `<iframe>`'s own width/height (pixels).
#' @param ... Passed on to [vg_widget()] when `spec` isn't already a
#'   built widget (e.g. `width =`, `height =`). Also silently absorbs
#'   arguments meant for [vg_snapshot()] instead (e.g. `delay =`), so
#'   [vg_render()] can forward its own `...` uniformly. `use_cache`
#'   defaults to `FALSE` here specifically, even if this machine has a
#'   local duckdb-wasm cache set up (see [vg_cache_duckdb()]):
#'   htmlwidgets bundles a cached dependency as a real copy of the file,
#'   and duckdb-wasm's engine is a ~35 MB binary -- embedding that into
#'   what's typically a *committed* file would be a serious bloat
#'   (confirmed directly: this happened once, ~35 MB per widget). Pass
#'   `use_cache = TRUE` explicitly to override.
#' @family rendering functions
#' @export
vg_iframe <- function(
  spec,
  file = NULL,
  src = NULL,
  vwidth = 992,
  vheight = 744,
  ...
) {
  dots <- list(...)
  if (!inherits(spec, "htmlwidget") && is.null(dots$use_cache)) {
    dots$use_cache <- FALSE
  }
  widget <- if (inherits(spec, "htmlwidget")) {
    spec
  } else {
    do.call(vg_widget, c(list(spec), dots))
  }

  out <- if (!is.null(file)) file else default_snapshot_path(".html")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(widget, out, selfcontained = FALSE)

  if (!is.null(file) && is.null(src)) {
    return(invisible(out))
  }

  # The default src rewrite (man/figures/<x> -> reference/figures/<x>)
  # matches exactly where pkgdown::build_reference() copies man/figures/
  # contents to (confirmed directly) -- see the "Using this outside a
  # pkgdown home page" section above for anything else.
  final_src <- if (!is.null(src)) {
    src
  } else {
    sub("^man/figures/", "reference/figures/", out)
  }
  knitr::asis_output(sprintf(
    '<iframe src="%s" width="%s" height="%s" style="border: none;" loading="lazy"></iframe>',
    final_src,
    vwidth,
    vheight
  ))
}
