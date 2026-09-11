#' Add a legend
#'
#' A legend can be embedded in a plot (added alongside its marks, picking up
#' a matching encoding automatically) or, given `for_plot`, live as a
#' standalone layout item referencing a plot by name (its `name =` attribute,
#' see [vg_plot()]) -- mirroring mosaic-spec's own two legend forms.
#'
#' @param spec A plot fragment or `vgspec` to embed this legend in, or `NULL`
#'   to start a new plot with just this legend. Not used (and not allowed)
#'   for a standalone legend with `for_plot` set -- combine that with plots
#'   using [vg_vconcat()]/[vg_hconcat()] instead.
#' @param type The legend type: `"color"`, `"opacity"`, or `"symbol"`.
#' @param ... Legend options, e.g. `as = param(brush)`, `label = "Species"`,
#'   `field =`, `tickSize =`, `columns =`, or margin/width/height settings.
#' @param for_plot For a standalone legend: the `name` of the plot it
#'   decorates.
#' @export
vg_legend <- function(spec = NULL, type, ..., for_plot = NULL) {
  legend_obj <- structure(
    list(type = type, for_plot = for_plot, options = list(...)),
    class = "vg_legend"
  )

  if (!is.null(for_plot)) {
    if (!is.null(spec)) {
      stop(
        "A legend with `for_plot` is a standalone layout item; it doesn't ",
        "take a spec/plot to extend. Combine it with plots using ",
        "vg_vconcat()/vg_hconcat() instead.",
        call. = FALSE
      )
    }
    legend_obj
  } else {
    fragment <- as_vg_plot_fragment(spec)
    fragment$items <- c(fragment$items, list(legend_obj))
    update_layout(spec, fragment)
  }
}

#' @rdname vg_legend
#' @export
vg_legend_color <- function(spec = NULL, ..., for_plot = NULL) vg_legend(spec, "color", ..., for_plot = for_plot)

#' @rdname vg_legend
#' @export
vg_legend_opacity <- function(spec = NULL, ..., for_plot = NULL) vg_legend(spec, "opacity", ..., for_plot = for_plot)

#' @rdname vg_legend
#' @export
vg_legend_symbol <- function(spec = NULL, ..., for_plot = NULL) vg_legend(spec, "symbol", ..., for_plot = for_plot)

#' @export
print.vg_legend <- function(x, ...) {
  cat("<vg_legend:", x$type, ">")
  if (!is.null(x$for_plot)) cat(" for", x$for_plot)
  cat("\n")
  if (length(x$options)) {
    str_opt <- vapply(x$options, deparse_short, character(1))
    cat(paste0("  ", names(x$options), " = ", str_opt, collapse = "\n"), "\n")
  }
  invisible(x)
}
