#' Create a new, empty plot fragment.
#' @noRd
new_vg_plot_fragment <- function(items = list(), attrs = list()) {
  structure(list(items = items, attrs = attrs), class = "vg_plot_fragment")
}

is_vg_plot_fragment <- function(x) inherits(x, "vg_plot_fragment")

#' Coerce `spec` (as passed in to a mark/interactor constructor) into a plot
#' fragment that new items can be appended to.
#'
#' `NULL` becomes a fresh, empty fragment. An existing fragment is returned
#' as-is. A `vgspec` delegates to its current layout (see `vg_create()`).
#' @noRd
as_vg_plot_fragment <- function(spec) {
  if (is.null(spec)) {
    new_vg_plot_fragment()
  } else if (is_vg_plot_fragment(spec)) {
    spec
  } else if (is_vgspec(spec)) {
    if (is.null(spec$layout)) {
      new_vg_plot_fragment()
    } else if (is_vg_plot_fragment(spec$layout)) {
      spec$layout
    } else {
      stop(
        "This vgspec already has a non-plot layout (e.g. from vg_vconcat()/vg_hconcat()); ",
        "wrap new marks in vg_plot() and add them explicitly instead of piping directly.",
        call. = FALSE
      )
    }
  } else {
    stop("Don't know how to use an object of class ", paste(class(spec), collapse = "/"),
         " as a plot.", call. = FALSE)
  }
}

#' Put an updated plot fragment back into whatever `spec` was.
#' @noRd
update_layout <- function(spec, fragment) {
  if (is_vgspec(spec)) {
    spec$layout <- fragment
    spec
  } else {
    fragment
  }
}

#' Create or extend a mosaic plot (a coordinate space that groups one or more
#' marks and interactors)
#'
#' Plot-level attributes such as `width`, `height`, `name`, and `margins`
#' belong to the plot as a whole, not to any individual mark. `vg_plot()` can
#' either wrap marks/interactors explicitly, or be piped onto a chain of
#' marks built with functions like [vg_mark()] / `vg_dot()` / `vg_line_y()`.
#' The two styles are equivalent:
#'
#' ```r
#' vg_plot(vg_dot(x = ~a, y = ~b), vg_line_y(x = ~a, y = ~c), width = 680)
#'
#' vg_dot(x = ~a, y = ~b) |>
#'   vg_line_y(x = ~a, y = ~c) |>
#'   vg_plot(width = 680)
#' ```
#'
#' When two sibling marks/interactors set the same plot-level attribute to
#' different values, the later one wins and a warning is emitted; identical
#' values accumulate silently. A named argument that isn't one of mosaic's
#' own plot attributes also triggers a warning, since it won't do anything
#' to the rendered plot -- see [vg_attributes()].
#'
#' @param spec A plot fragment or `vgspec` to extend, or `NULL` to start a new plot.
#' @param ... Additional plot fragments (marks/interactors) to include, and/or
#'   named plot-level attributes (`width =`, `height =`, `name =`, ...).
#' @export
vg_plot <- function(spec = NULL, ...) {
  args <- list(...)
  children <- Filter(is_vg_plot_fragment, args)
  attrs <- args[!vapply(args, is_vg_plot_fragment, logical(1))]
  warn_unknown_attrs(names(attrs), "vg_plot()")

  fragment <- as_vg_plot_fragment(spec)
  for (child in children) {
    fragment$items <- c(fragment$items, child$items)
    fragment$attrs <- merge_attrs(fragment$attrs, child$attrs, context = "vg_plot()")
  }
  fragment$attrs <- merge_attrs(fragment$attrs, attrs, context = "vg_plot()")

  update_layout(spec, fragment)
}

#' @export
print.vg_plot_fragment <- function(x, ...) {
  cat("<vg_plot_fragment>", length(x$items), "item(s)\n")
  for (item in x$items) {
    kind <- if (inherits(item, "vg_mark")) "mark" else "interactor"
    type <- if (is.null(item$mark)) item$type else item$mark
    cat("  -", kind, ":", type, "\n")
  }
  if (length(x$attrs)) {
    cat("  attrs:\n")
    str_attrs <- vapply(x$attrs, deparse_short, character(1))
    cat(paste0("    ", names(x$attrs), " = ", str_attrs, collapse = "\n"), "\n")
  }
  invisible(x)
}
