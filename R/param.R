#' Reference a mosaic Param or Selection by name
#'
#' `param(brush)` is how a `$brush`-style reference (to a Param or Selection
#' declared elsewhere in the spec, e.g., via `vg_params()` or as the `as =`
#' target of an interactor) is written in valid R. `$brush` alone is not
#' parseable R, so `param()` captures the bare name you give it and returns an
#' object that stands for `$brush`.
#'
#' `param()` is meant for use in the arguments of marks, interactors and
#' inputs, in formulas, and inside `sql()` expressions. Printing it just
#' shows the `$name` form.
#'
#' @param name The name of the param/selection, unquoted.
#' @family spec functions
#' @export
#' @examples
#' param(brush)
param <- function(name) {
  nm <- rlang::as_name(rlang::ensym(name))
  new_vg_param(nm)
}

new_vg_param <- function(name) {
  structure(list(name = name), class = "vg_param")
}

is_vg_param <- function(x) inherits(x, "vg_param")

#' @export
format.vg_param <- function(x, ...) paste0("$", x$name)

#' @export
print.vg_param <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}
