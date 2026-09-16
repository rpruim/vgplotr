# Shared by sql()/agg(): mosaic-spec's SQLExpression/AggregateExpression
# are both { <key>: "<sql text>", label: <optional> } -- the same shape,
# just a different key depending on whether the expression contains an
# aggregate function.
new_vg_sql_expr <- function(key, parts, label) {
  text <- paste0(
    vapply(parts, function(x) if (is_vg_param(x)) format(x) else as.character(x), character(1)),
    collapse = ""
  )
  structure(list(key = key, text = text, label = label), class = "vg_sql_expr")
}

is_vg_sql_expr <- function(x) inherits(x, "vg_sql_expr")

#' Write a raw SQL expression
#'
#' Wraps a raw SQL expression for use as a mark encoding or other spec
#' value, matching mosaic-spec's own `SQLExpression`/`AggregateExpression`.
#' Unlike [vg_bin()]/[vg_count()]/etc., this is not translated from R syntax
#' -- the pieces are just pasted together as SQL text, exactly as DuckDB
#' will see it. Use `sql()` for ordinary expressions and `agg()` when the
#' expression contains an aggregate function (e.g. `SUM(...)`); mosaic
#' needs to know which, since aggregates are handled differently in a
#' query.
#'
#' A `param()` reference can be embedded either by writing mosaic's own
#' `$name` syntax directly in the SQL text, or by passing the `param()`
#' object as one of the pieces -- `sql("v + $point")` and
#' `sql("v + ", param(point))` are equivalent. Both work directly as a
#' value (`y = sql(...)`) or inside a mapping formula (`y = ~ sql(...)`).
#'
#' @param ... One or more pieces of the SQL expression; character strings
#'   and [param()] values are pasted together with no separator in between.
#' @param label An optional label for this expression, e.g. for a plot axis.
#' @family transform functions
#' @export
sql <- function(..., label = NULL) new_vg_sql_expr("sql", list(...), label)

#' @rdname sql
#' @export
agg <- function(..., label = NULL) new_vg_sql_expr("agg", list(...), label)

#' @export
format.vg_sql_expr <- function(x, ...) paste0(x$key, '("', x$text, '")')

#' @export
print.vg_sql_expr <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}
