#' Add a mark (a single visual layer, e.g. points or a line) to a plot
#'
#' This is the generic, low-level constructor that the `vg_dot()`,
#' `vg_line_y()`, etc. convenience functions are built on top of (eventually
#' generated from the mosaic-spec mark schema; see
#' `design/api-brainstorming.qmd`).
#'
#' @param spec A plot fragment or `vgspec` to add this mark to, or `NULL` to
#'   start a new plot with just this mark.
#' @param mark The mosaic mark type, e.g. `"dot"`, `"lineY"`.
#' @param ... Encodings (e.g. `x = ~var1`), mark options, and/or plot-level
#'   attributes (`width =`, `name =`, ...). See [vg_plot()] for how
#'   plot-level attributes from multiple marks are combined.
#' @export
vg_mark <- function(spec = NULL, mark, ...) {
  split <- split_plot_args(list(...))

  mark_obj <- structure(
    list(mark = mark, encodings = split$local_args),
    class = "vg_mark"
  )

  fragment <- as_vg_plot_fragment(spec)
  fragment$items <- c(fragment$items, list(mark_obj))
  fragment$attrs <- merge_attrs(fragment$attrs, split$plot_attrs, context = paste0("mark `", mark, "`"))

  update_layout(spec, fragment)
}

#' @rdname vg_mark
#' @export
vg_dot <- function(spec = NULL, ...) vg_mark(spec, "dot", ...)

#' @rdname vg_mark
#' @export
vg_line_y <- function(spec = NULL, ...) vg_mark(spec, "lineY", ...)

#' @rdname vg_mark
#' @export
vg_area_y <- function(spec = NULL, ...) vg_mark(spec, "areaY", ...)

#' @rdname vg_mark
#' @export
vg_rect_y <- function(spec = NULL, ...) vg_mark(spec, "rectY", ...)

#' @export
print.vg_mark <- function(x, ...) {
  cat("<vg_mark:", x$mark, ">\n")
  if (length(x$encodings)) {
    str_enc <- vapply(x$encodings, deparse_short, character(1))
    cat(paste0("  ", names(x$encodings), " = ", str_enc, collapse = "\n"), "\n")
  }
  invisible(x)
}
