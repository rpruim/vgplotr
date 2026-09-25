#' Write a raw JavaScript value
#'
#' Wraps a piece of JavaScript source so it reaches the browser as a real
#' JavaScript value -- most usefully a function -- instead of as a string:
#' `opacity = js("() => 0.7")` becomes `opacity: () => 0.7` in the rendered
#' plot. Use it where mosaic-spec's own JSON can only name a column or hold
#' a literal, but the underlying vgplot/Observable Plot API also accepts a
#' function (or other JavaScript value).
#'
#' The code is evaluated once, in the browser, when the plot is rendered by
#' [vg_render()] (or an rmarkdown/Quarto chunk, or a saved widget). It runs
#' with the page's own privileges, so -- like any JavaScript you would put
#' in a page -- only use code you wrote or trust.
#'
#' This is a vgplotr extension to mosaic-spec: [to_json()]/[to_yaml()] write
#' it as `{js: "<code>"}`, which vgplotr's renderer understands but mosaic's
#' own `parseSpec()` does not.
#'
#' A function is called by Observable Plot as `(d, i, data)`, but mosaic
#' holds a table as columns, not row objects: `d` is `undefined`, and only
#' the row index `i` is usable. So a function suits a constant
#' (`() => 0.7`) or a value computed from the row's position
#' (`(d, i) => i % 2 ? 1 : 0.3`); to draw from a column's values, map the
#' column itself (`opacity = ~col`) or use [sql()].
#'
#' Unlike [sql()], `js()` is not translated from R and takes no [param()]
#' pieces, and it works as an argument value (`opacity = js(...)`) but not
#' inside a mapping formula.
#'
#' @param code A single string of JavaScript source that evaluates to the
#'   value wanted, e.g., `"() => 0.7"` or `"(d, i) => i % 2 ? 1 : 0.3"`.
#' @family transform functions
#' @export
#' @examples
#' js("() => 0.7")
js <- function(code) {
  if (!is.character(code) || length(code) != 1 || is.na(code)) {
    stop("`code` must be a single string of JavaScript source.", call. = FALSE)
  }
  structure(list(code = code), class = "vg_js")
}

is_vg_js <- function(x) inherits(x, "vg_js")

serialize_js <- function(x) list(js = x$code)

#' @export
format.vg_js <- function(x, ...) paste0("js(", encodeString(x$code, quote = '"'), ")")

#' @export
print.vg_js <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}
