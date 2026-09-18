#' Render a vgspec, picking the right way to display it
#'
#' The multitool of the rendering-functions family: renders `spec` using
#' whichever of [vg_widget()], [vg_snapshot()], or [vg_iframe()] makes
#' sense for the current rendering context, or whichever `mode` asks for
#' explicitly.
#'
#' Left at the default `mode = "auto"`, `vg_render()` inspects whether the
#' current output can actually run JavaScript. An HTML vignette or Quarto
#' HTML page -- or no active knit at all, e.g., typed directly at the
#' console -- gets [vg_widget()]'s live widget; a plain-markdown or other
#' non-HTML output (`github_document`, PDF, Word, ...), which can't run
#' embedded JavaScript, gets [vg_snapshot()]'s static screenshot instead.
#' This check is deliberately narrower than `knitr::is_html_output()`:
#' that function also counts "markdown"/"gfm" as HTML-capable (they do
#' support *passing through* raw HTML blocks), but GitHub strips
#' `<script>` tags out of a rendered README regardless, so a live widget
#' still wouldn't actually run there.
#'
#' `mode = "iframe"` is never chosen automatically -- ask for it
#' explicitly (e.g., for a pkgdown home page built from
#' `pkgdown/index.md`; see [vg_iframe()] for why that one needs to be
#' deliberate).
#'
#' Use `vg_render(spec)` anywhere you'd otherwise write a bare
#' [vg_widget()] call, in a document meant to be knitted to more than one
#' kind of output. It can also be used anywhere [vg_widget()],
#' [vg_iframe()] or [vg_snapshot()] are used by
#' setting `mode` appropriately.

#'
#' @param spec A `vgspec`, a JSON/YAML spec string (see [vg_widget()]),
#'   or an already-built widget (i.e., the result of calling
#'   [vg_widget()] yourself).
#' @param ... Passed on to whichever of [vg_widget()], [vg_snapshot()],
#'   or [vg_iframe()] ends up handling the request (e.g., `width =`,
#'   `use_cache =`, or `connector =` for [vg_widget()], `delay =` for
#'   [vg_snapshot()]).
#'   Each of those three accepts and silently ignores arguments meant for
#'   one of the other two, so it's fine to pass along options for a mode
#'   that doesn't end up being used.
#' @param mode `"auto"` (the default, see Details), or force one of
#'   `"widget"`, `"snapshot"`, `"iframe"` regardless of context.
#' @family rendering functions
#' @export
vg_render <- function(
  spec,
  ...,
  mode = c("auto", "widget", "snapshot", "iframe")
) {
  mode <- match.arg(mode)
  if (mode == "auto") {
    mode <- if (can_embed_live_widget()) "widget" else "snapshot"
  }
  switch(
    mode,
    widget = vg_widget(spec, ...),
    snapshot = vg_snapshot(spec, ...),
    iframe = vg_iframe(spec, ...)
  )
}

# Whether the current rendering context (if any) can actually execute
# embedded JavaScript -- see vg_render()'s "auto" mode above. Deliberately
# narrower than knitr::is_html_output() -- that function also counts
# "markdown"/"gfm" as HTML output (they do support *passing through* raw
# HTML blocks), but GitHub sanitizes <script> tags out of a rendered
# README regardless, so a live widget still wouldn't actually run there.
# No active knit at all (e.g., called at the console) defaults to TRUE,
# matching vg_widget()'s normal interactive behavior.
can_embed_live_widget <- function() {
  if (!isTRUE(getOption("knitr.in.progress", FALSE))) {
    return(TRUE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(TRUE)
  }
  fmt <- knitr::pandoc_to()
  if (length(fmt) == 0) {
    return(TRUE)
  }
  fmt %in% c("html", "html4", "html5", "revealjs", "s5", "slideous", "slidy")
}
