# Names that are attributes of a *plot* (the mosaic-spec container that
# groups one or more marks/interactors on shared axes), as opposed to
# encodings/options that belong to a single mark or interactor.
#
# This list is hand-maintained for now. Once the function-factory /
# JSON-schema generation described in design/api-brainstorming.qmd exists,
# it should be derived from the mosaic-spec schema instead.
vg_plot_level_args <- function() {
  c(
    "name", "style", "width", "height",
    "margin", "margin_left", "margin_right", "margin_top", "margin_bottom", "margins",
    "align", "aspect_ratio", "inset", "axis", "grid", "label", "padding", "round",
    "xDomain", "yDomain", "xyDomain", "zDomain",
    "xLabel", "yLabel", "xAxis", "yAxis", "xLabelAnchor", "yLabelAnchor",
    "xTickFormat", "yTickFormat", "colorScheme", "colorScale"
  )
}

#' Split `...` arguments into plot-level attributes and local (mark/interactor)
#' arguments, based on `vg_plot_level_args()`.
#' @noRd
split_plot_args <- function(args) {
  plot_names <- intersect(names(args), vg_plot_level_args())
  list(
    plot_attrs = args[plot_names],
    local_args = args[setdiff(names(args), plot_names)]
  )
}

#' Merge a named list of new plot-level attributes into an existing set.
#'
#' A key that hasn't been set yet is added silently. A key that is set again
#' with an identical value is a silent no-op. A key that is set again with a
#' *different* value keeps the new value but emits a warning, since the two
#' specifications are incompatible.
#' @noRd
merge_attrs <- function(old, new, context = NULL) {
  for (nm in names(new)) {
    if (nm %in% names(old) && !identical(old[[nm]], new[[nm]])) {
      where <- if (is.null(context)) "" else paste0(" in ", context)
      warning(
        sprintf(
          "Conflicting value for `%s`%s: replacing %s with %s.",
          nm, where, deparse_short(old[[nm]]), deparse_short(new[[nm]])
        ),
        call. = FALSE
      )
    }
    old[[nm]] <- new[[nm]]
  }
  old
}

deparse_short <- function(x) {
  paste(deparse(x, width.cutoff = 30L), collapse = " ")
}
