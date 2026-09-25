#' Define a Selection, or a date-valued Param, for `vg_params()`
#'
#' `vg_selection()` writes mosaic-spec's `Selection` definition,
#' `vg_selection("crossfilter", include = param(other))`, where
#' `vg_params(brush = list(select = "crossfilter"))` would otherwise be typed
#' out by hand. It checks the pieces and spells the `include` references for
#' you. `vg_param_date()` writes a date-valued param: a plain `Date` in
#' `vg_params()` reaches mosaic as an ordinary string, not a date.
#'
#' A selection collects the clauses that interactors and inputs publish to it
#' (`as = param(brush)`), and filters whatever is bound to it (a mark's
#' `filter_by`). Its `select` type decides how the clauses combine.
#'
#' @param select How clauses combine: `"crossfilter"` (a cross-filtered
#'   intersection: a selection made in a plot filters the others but not
#'   itself), `"intersect"` (logical "and"), `"union"` (logical "or"), or
#'   `"single"` (only the most recent clause is kept).
#' @param cross Whether selections made in a plot filter other plots but not
#'   itself (default `FALSE`, except that a `"crossfilter"` selection always
#'   does). Not allowed with `select = "crossfilter"`, which ignores it.
#' @param empty Whether a selection with no clauses selects no records (`TRUE`)
#'   or all of them (`FALSE`, the default).
#' @param include Upstream selections whose clauses are relayed into this one:
#'   a [param()], a list of them, or the selections' names as strings.
#' @param date A `Date`, a `POSIXct` date-time, or an ISO date/time string,
#'   e.g. `"2020-01-01"`: the param's initial value.
#' @return A `vg_selection` or `vg_param_date` object, for use as a named
#'   argument to [vg_params()].
#' @family spec functions
#' @export
#' @examples
#' vg_create() |>
#'   vg_params(
#'     brush = vg_selection("crossfilter"),
#'     either = vg_selection("union", include = c("a", "b")),
#'     since = vg_param_date(as.Date("2020-01-01"))
#'   )
vg_selection <- function(select = c("crossfilter", "intersect", "single", "union"),
                         cross = NULL, empty = NULL, include = NULL) {
  select <- rlang::arg_match(select)
  check_selection_flag(cross, "cross")
  check_selection_flag(empty, "empty")
  if (!is.null(cross) && select == "crossfilter") {
    warning(
      "`cross` has no effect on a \"crossfilter\" selection, which always ",
      "cross-filters; use select = \"intersect\" (or another type) to set it.",
      call. = FALSE
    )
  }
  fields <- list(select = select, cross = cross, empty = empty, include = as_param_refs(include))
  structure(fields[!vapply(fields, is.null, logical(1))], class = "vg_selection")
}

check_selection_flag <- function(x, name) {
  if (!is.null(x) && !(is.logical(x) && length(x) == 1 && !is.na(x))) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

# NULL, or a list of vg_param: accepts one param(), a list of them, or names
# (with or without mosaic's leading `$`).
as_param_refs <- function(x) {
  if (is.null(x)) return(NULL)
  if (is_vg_param(x)) x <- list(x) else if (is.character(x)) x <- as.list(x)
  ok <- is.list(x) && length(x) > 0 && all(vapply(x, function(e) {
    is_vg_param(e) || (is.character(e) && length(e) == 1 && !is.na(e) && nzchar(e))
  }, logical(1)))
  if (!ok) {
    stop("`include` must be a param(), a list of them, or the names of selections.", call. = FALSE)
  }
  lapply(x, function(e) if (is_vg_param(e)) e else new_vg_param(sub("^\\$", "", e)))
}

#' @rdname vg_selection
#' @export
vg_param_date <- function(date) {
  iso <- if (inherits(date, "POSIXt")) {
    format(as.POSIXct(date), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else if (inherits(date, "Date")) {
    format(date, "%Y-%m-%d")
  } else {
    date
  }
  if (!is.character(iso) || length(iso) != 1 || is.na(iso) || !nzchar(iso)) {
    stop("`date` must be a single Date, date-time, or ISO date/time string.", call. = FALSE)
  }
  structure(list(date = iso), class = "vg_param_date")
}

#' @export
print.vg_selection <- function(x, ...) {
  cat("<vg_selection: ", x$select, ">\n", sep = "")
  fields <- x[setdiff(names(x), c("select", "include"))]
  print_fields(fields)
  if (!is.null(x$include)) {
    cat("  include = ", paste(vapply(x$include, format, character(1)), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' @export
print.vg_param_date <- function(x, ...) {
  cat("<vg_param_date: ", x$date, ">\n", sep = "")
  invisible(x)
}
