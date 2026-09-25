#' Date/time intervals for window frames
#'
#' `vg_years()`, `vg_months()`, `vg_days()`, `vg_hours()`, `vg_minutes()`,
#' `vg_seconds()`, `vg_milliseconds()` and `vg_microseconds()` write a span of
#' time -- mosaic-spec's interval transforms (`{days: 7}`) -- where a window
#' frame wants an offset in time units instead of a row count. The frame
#' options `rows`, `range` and `groups` of a window or aggregate transform
#' (see [vg_transforms]) each take a pair of offsets, measured from the
#' current row: how far the frame reaches *before* it, then how far *after*
#' it. So `range = list(vg_days(6), vg_days(0))` is a trailing 7-day window:
#' the current day and the 6 before it.
#'
#' Unlike the transforms in [vg_transforms], the argument is a plain value,
#' not a column, so `vg_days(6)` is evaluated as ordinary R. An offset is a
#' distance, so it can't be negative. A `range` frame also needs `orderby`
#' (a column name, as a string) to say which column the distances are
#' measured along.
#'
#' As of mosaic-spec 0.31.0 a frame is only rendered correctly if *both*
#' offsets are intervals, as above: mosaic fails on a plain number or `NULL`
#' in a frame (`rows = c(6, 0)`), so write `vg_days(0)` rather than `0`.
#'
#' @param n A single non-negative number of units, or a [param()].
#' @return A `vg_interval` object, serialized as `{<unit>: n}`.
#' @family transform functions
#' @name vg_intervals
#' @examples
#' # a 7-day trailing average
#' y <- ~ vg_avg(close, orderby = "date", range = list(vg_days(6), vg_days(0)))
NULL

new_vg_interval_fn <- function(unit) {
  force(unit)
  function(n) {
    if (!is_vg_param(n) && !(is.numeric(n) && length(n) == 1 && is.finite(n) && n >= 0)) {
      stop(
        "`n` must be a single non-negative number of ", unit, " (or a param()): ",
        "a frame offset is a distance from the current row, so it can't be negative.",
        call. = FALSE
      )
    }
    structure(list(key = unit, n = n), class = "vg_interval")
  }
}

#' @rdname vg_intervals
#' @export
vg_years <- new_vg_interval_fn("years")
#' @rdname vg_intervals
#' @export
vg_months <- new_vg_interval_fn("months")
#' @rdname vg_intervals
#' @export
vg_days <- new_vg_interval_fn("days")
#' @rdname vg_intervals
#' @export
vg_hours <- new_vg_interval_fn("hours")
#' @rdname vg_intervals
#' @export
vg_minutes <- new_vg_interval_fn("minutes")
#' @rdname vg_intervals
#' @export
vg_seconds <- new_vg_interval_fn("seconds")
#' @rdname vg_intervals
#' @export
vg_milliseconds <- new_vg_interval_fn("milliseconds")
#' @rdname vg_intervals
#' @export
vg_microseconds <- new_vg_interval_fn("microseconds")

is_vg_interval <- function(x) inherits(x, "vg_interval")

serialize_interval <- function(x) named_list(x$key, serialize_value(x$n))

#' @export
format.vg_interval <- function(x, ...) paste0("vg_", x$key, "(", format(x$n), ")")

#' @export
print.vg_interval <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}
