new_vg_concat <- function(direction, children = list()) {
  structure(list(direction = direction, children = children), class = "vg_concat")
}

is_vg_concat <- function(x) inherits(x, "vg_concat")

new_vg_space <- function(direction, amount) {
  structure(list(direction = direction, amount = amount), class = "vg_space")
}

is_vg_space <- function(x) inherits(x, "vg_space")

# If `existing` is already a concat of the same direction, its children are
# extended in place; otherwise `existing` (a plot fragment, a concat of the
# *other* direction, or NULL) becomes the first child alongside `new_children`.
# This is what lets vg_vconcat()/vg_hconcat() nest naturally (an hconcat
# passed into a vconcat becomes one row) without the caller having to think
# about it.
combine_concat <- function(direction, existing, new_children) {
  if (is_vg_concat(existing) && existing$direction == direction) {
    existing$children <- c(existing$children, new_children)
    existing
  } else if (is.null(existing)) {
    new_vg_concat(direction, new_children)
  } else {
    new_vg_concat(direction, c(list(existing), new_children))
  }
}

vg_concat_ <- function(direction, spec, ...) {
  children <- list(...)
  if (is_vgspec(spec)) {
    spec$layout <- combine_concat(direction, spec$layout, children)
    spec
  } else {
    combine_concat(direction, spec, children)
  }
}

#' Stack plots (and other layout items) vertically or horizontally
#'
#' Unlike piping marks into one another -- which adds marks to the *same*
#' plot (see [vg_plot()]) -- each argument passed to `vg_vconcat()`/
#' `vg_hconcat()` becomes its *own* plot, laid out as a column (`vconcat`) or
#' row (`hconcat`). Children can be bare mark/interactor chains (each
#' promoted to its own plot), `vg_plot()`-wrapped plots (to attach
#' plot-level attributes), spacers (`vg_hspace()`/`vg_vspace()`), or nested
#' `vg_vconcat()`/`vg_hconcat()` results.
#'
#' @param spec A `vgspec`, an existing layout item to prepend, or `NULL` to
#'   start a new layout.
#' @param ... Additional layout items (plots, spacers, nested concats).
#' @family layout functions
#' @export
vg_vconcat <- function(spec = NULL, ...) vg_concat_("vconcat", spec, ...)

#' @rdname vg_vconcat
#' @export
vg_hconcat <- function(spec = NULL, ...) vg_concat_("hconcat", spec, ...)

#' Fixed-size spacers for use inside `vg_vconcat()`/`vg_hconcat()`
#' @param amount Spacer size in pixels.
#' @family layout functions
#' @export
vg_hspace <- function(amount) new_vg_space("hspace", amount)

#' @rdname vg_hspace
#' @export
vg_vspace <- function(amount) new_vg_space("vspace", amount)

#' @export
print.vg_concat <- function(x, ...) {
  cat("<vg_concat:", x$direction, ">", length(x$children), "item(s)\n")
  for (child in x$children) {
    cat(" -")
    if (is_vg_plot_fragment(child)) cat(" plot with", length(child$items), "item(s)\n")
    else if (is_vg_concat(child)) cat(" nested", child$direction, "with", length(child$children), "item(s)\n")
    else if (is_vg_space(child)) cat(" ", child$direction, "(", child$amount, "px)\n")
    else cat(" <", paste(class(child), collapse = "/"), ">\n")
  }
  invisible(x)
}
