# Transform functions (vg_bin(), vg_count(), ...) for use inside mapping
# formulas, e.g., `x = ~vg_bin(delay, step = 10)`, or called directly (each
# returns a "vg_transform" object). Transcribed from mosaic-spec's
# Transform.ts (uwdata/mosaic packages/vgplot/spec/src/spec/Transform.ts),
# which is the source of truth for names/arguments if this ever needs
# updating -- a schema-driven generator (per design/api-brainstorming.qmd)
# would be a better long-term source, but no such schema was available here.
#
# Each function's "field" arguments (the column/expression(s) it operates
# on) are captured unevaluated via match.call() -- this is what lets the
# exact same function calls be recognized syntactically when they appear,
# still unevaluated, inside a formula's RHS (see serialize_expr() in
# serialize.R). "Option" arguments (interval, step, distinct, order_by, ...)
# are evaluated normally.

# AggregateOptions + WindowOptions from Transform.ts, shared by every
# aggregate transform; window transforms take only the WindowOptions.
.vg_window_options <- c("order_by", "partition_by", "rows", "range", "groups", "exclude")

# Options whose R name differs from Mosaic's key (which run words together):
# R name -> Mosaic key. Mosaic's spelling is also accepted, through `...`.
.vg_transform_option_keys <- c(order_by = "orderby", partition_by = "partitionby", min_step = "minstep")
.vg_aggregate_options <- c("distinct", .vg_window_options)

new_vg_transform_fn <- function(key, field_names, option_names) {
  spec <- list(key = key, field_names = field_names, option_names = option_names)
  arg_names <- c(field_names, option_names, "...")
  args <- stats::setNames(rep(list(quote(expr = )), length(arg_names)), arg_names)
  rlang::new_function(
    args,
    quote(build_vg_transform(spec, match.call(), parent.frame())),
    env = environment()
  )
}

build_vg_transform <- function(spec, mc, env) {
  supplied <- normalize_transform_args(spec, as.list(mc)[-1])

  field_names_supplied <- spec$field_names[spec$field_names %in% names(supplied)]
  field <- unname(supplied[field_names_supplied])

  option_names_supplied <- spec$option_names[spec$option_names %in% names(supplied)]
  options <- lapply(supplied[option_names_supplied], function(e) serialize_value(eval(e, envir = env)))
  renamed <- names(options) %in% names(.vg_transform_option_keys)
  names(options)[renamed] <- .vg_transform_option_keys[names(options)[renamed]]

  structure(list(key = spec$key, field = field, env = env, options = options), class = "vg_transform")
}

is_vg_transform <- function(x) inherits(x, "vg_transform")

# A transform's arguments (a matched call's, as a named list), with Mosaic's
# own spellings (`orderby =`) renamed to vgplotr's (`order_by =`). Anything
# else that ended up in `...` is an error: an unrecognized name (with a
# suggestion) or a stray positional argument.
normalize_transform_args <- function(spec, supplied) {
  nms <- names(supplied) %||% rep("", length(supplied))
  valid <- c(spec$field_names, spec$option_names)
  aliases <- stats::setNames(names(.vg_transform_option_keys), .vg_transform_option_keys)
  is_alias <- nms %in% names(aliases) & aliases[nms] %in% valid
  nms[is_alias] <- aliases[nms[is_alias]]
  names(supplied) <- nms
  fn_name <- transform_fn_name(spec)
  if (any(!nzchar(nms))) {
    stop(sprintf("In `%s()`: too many unnamed arguments -- its arguments are %s.",
                 fn_name, paste0("`", valid, "`", collapse = ", ")), call. = FALSE)
  }
  unknown <- nms[!nms %in% valid]
  if (length(unknown)) stop(unknown_transform_args_message(fn_name, valid, setdiff(nms, unknown), unknown), call. = FALSE)
  supplied
}

transform_fn_name <- function(spec) {
  names(vg_transform_specs)[vapply(vg_transform_specs, function(s) identical(s$key, spec$key), logical(1))][1]
}

# "In `vg_bin()`: `stp` is not an argument of this transform. Did you
# perhaps mean `step`?"
unknown_transform_args_message <- function(fn_name, valid, present, unknown) {
  names_str <- paste0("`", unknown, "`", collapse = ", ")
  subject <- if (length(unknown) == 1) {
    paste0(names_str, " is not an argument of this transform.")
  } else {
    paste0(names_str, " are not arguments of this transform.")
  }
  suggestions <- unlist(lapply(unknown, function(nm) {
    format_suggestion(
      suggest_names(nm, valid, present = present, table = synonym_section("transform_options")),
      arg = if (length(unknown) > 1) nm
    )
  }))
  if (length(suggestions) == 0) {
    suggestions <- paste0("Its arguments are ", paste0("`", valid, "`", collapse = ", "), ".")
  }
  paste(c(sprintf("In `%s()`: %s", fn_name, subject), suggestions), collapse = " ")
}

# A transform written inside a mapping formula (`~vg_bin(delay, step = 10)`)
# is never *called* -- serialize_expr() (R/serialize.R) reads it
# syntactically with match.call() -- and that happens at render/export time,
# well after the mark was built. A misspelled option there fails with R's own
# bare "unused argument (stp = 10)", which doesn't even say which transform it
# was about. So this is match.call() plus, when it fails on an unrecognized
# *name*, an error that names the transform and suggests the closest valid
# argument (or lists them, if none is close). Anything else that goes wrong
# (a stray positional argument, an ambiguous partial name) re-raises R's own
# error untouched. `expr` is the unevaluated call, `fn` the transform.
match_transform_call <- function(fn_name, fn, expr) {
  tryCatch(
    match.call(definition = fn, call = expr),
    error = function(e) {
      valid <- names(formals(fn))
      supplied <- names(expr)[-1]
      supplied <- supplied[!is.na(supplied) & nzchar(supplied)]
      # match.call() accepts an unambiguous partial name (`inter =` for
      # `interval`), so that's not "unrecognized" -- only a name that
      # matches nothing is.
      is_partial <- vapply(supplied, function(s) sum(startsWith(valid, s)) == 1, logical(1))
      unknown <- supplied[!(supplied %in% valid) & !is_partial]
      if (length(unknown) == 0) stop(e)
      stop(unknown_transform_args_message(fn_name, valid, setdiff(supplied, unknown), unknown), call. = FALSE)
    }
  )
}

# " Did you perhaps mean `vg_bin()`?" (leading space, ready to append to an
# error message) for a call to an unrecognized function inside a mapping
# formula, or "" if nothing known is close. Draws on every transform plus
# the sql()/agg()/param() calls a formula also accepts.
#
# The distance is measured on the full names, so a typo in the `vg_` prefix
# itself (`vh_bin`) still counts as an edit -- but how many edits are
# tolerated is worked out from the length of the name *without* that prefix.
# The shared prefix pads every name's length, which used to make unrelated
# names look close: `vg_hist` matched `vg_first` and `vg_last`, which are two
# edits away from it, too far for a 4-letter name once the prefix is ignored.
#
# A name written without the prefix at all (`~bin(delay)`, `~count()`) is
# a likely slip of its own -- and two whole characters from every match, so
# no edit-distance rule would find it -- so if putting `vg_` in front of it
# gives exactly a known name, that's the suggestion, and it beats a fuzzy
# match (`avg` is one edit from `agg`, but `vg_avg` is what was meant). Only
# an exact match counts there: a fuzzy one on top would be guessing twice,
# and would tell a base-R `log(x)` to use `vg_lag()`.
#
# Next comes the synonym table's `transform_names` section -- a different word
# for a transform (`mean` for `vg_avg`, `n` for `vg_count`), looked up without
# the `vg_` prefix on either side -- and only then the closest spelling.
transform_name_suggestion <- function(fn_name) {
  known <- c(names(vg_transform_specs), "sql", "agg", "param")
  prefixed <- paste0("vg_", fn_name)
  hits <- if (!startsWith(fn_name, "vg_") && prefixed %in% known) {
    prefixed
  } else {
    bare <- sub("^vg_", "", fn_name)
    bare_length <- nchar(gsub("[_.]", "", bare))
    suggest_with_synonyms(
      bare, known, synonym_section("transform_names"),
      function() similar_names(fn_name, known, limit = max(1L, bare_length %/% 3L))
    )
  }
  if (length(hits) == 0) return("")
  paste0(" ", format_suggestion(paste0(hits, "()")))
}

#' @export
print.vg_transform <- function(x, ...) {
  field_str <- vapply(x$field, deparse_short, character(1))
  cat("<vg_transform: ", x$key, "(", paste(field_str, collapse = ", "), ")>\n", sep = "")
  print_fields(x$options)
  invisible(x)
}

# name (the R function name, and the lookup key used when recognizing an
# unevaluated call inside a formula) -> list(key, field_names, option_names)
vg_transform_specs <- list(
  vg_bin = list(key = "bin", field_names = "field", option_names = c("interval", "step", "steps", "min_step", "nice", "offset")),
  vg_column = list(key = "column", field_names = "field", option_names = character()),
  vg_date_month = list(key = "dateMonth", field_names = "field", option_names = character()),
  vg_date_month_day = list(key = "dateMonthDay", field_names = "field", option_names = character()),
  vg_date_day = list(key = "dateDay", field_names = "field", option_names = character()),
  vg_centroid = list(key = "centroid", field_names = "field", option_names = character()),
  vg_centroid_x = list(key = "centroidX", field_names = "field", option_names = character()),
  vg_centroid_y = list(key = "centroidY", field_names = "field", option_names = character()),
  vg_geojson = list(key = "geojson", field_names = "field", option_names = character()),

  vg_argmax = list(key = "argmax", field_names = c("x", "y"), option_names = .vg_aggregate_options),
  vg_argmin = list(key = "argmin", field_names = c("x", "y"), option_names = .vg_aggregate_options),
  vg_avg = list(key = "avg", field_names = "field", option_names = .vg_aggregate_options),
  vg_count = list(key = "count", field_names = "field", option_names = .vg_aggregate_options),
  vg_covariance = list(key = "covariance", field_names = c("x", "y"), option_names = .vg_aggregate_options),
  vg_covar_pop = list(key = "covarPop", field_names = c("x", "y"), option_names = .vg_aggregate_options),
  vg_first = list(key = "first", field_names = "field", option_names = .vg_aggregate_options),
  vg_geomean = list(key = "geomean", field_names = "field", option_names = .vg_aggregate_options),
  vg_last = list(key = "last", field_names = "field", option_names = .vg_aggregate_options),
  vg_max = list(key = "max", field_names = "field", option_names = .vg_aggregate_options),
  vg_min = list(key = "min", field_names = "field", option_names = .vg_aggregate_options),
  vg_median = list(key = "median", field_names = "field", option_names = .vg_aggregate_options),
  vg_mode = list(key = "mode", field_names = "field", option_names = .vg_aggregate_options),
  vg_product = list(key = "product", field_names = "field", option_names = .vg_aggregate_options),
  vg_quantile = list(key = "quantile", field_names = c("field", "probability"), option_names = .vg_aggregate_options),
  vg_stddev = list(key = "stddev", field_names = "field", option_names = .vg_aggregate_options),
  vg_stddev_pop = list(key = "stddevPop", field_names = "field", option_names = .vg_aggregate_options),
  vg_sum = list(key = "sum", field_names = "field", option_names = .vg_aggregate_options),
  vg_variance = list(key = "variance", field_names = "field", option_names = .vg_aggregate_options),
  vg_var_pop = list(key = "varPop", field_names = "field", option_names = .vg_aggregate_options),

  vg_row_number = list(key = "row_number", field_names = character(), option_names = .vg_window_options),
  vg_rank = list(key = "rank", field_names = character(), option_names = .vg_window_options),
  vg_dense_rank = list(key = "dense_rank", field_names = character(), option_names = .vg_window_options),
  vg_percent_rank = list(key = "percent_rank", field_names = character(), option_names = .vg_window_options),
  vg_cume_dist = list(key = "cume_dist", field_names = character(), option_names = .vg_window_options),
  vg_ntile = list(key = "ntile", field_names = "num_buckets", option_names = .vg_window_options),
  vg_lag = list(key = "lag", field_names = c("field", "offset", "default"), option_names = .vg_window_options),
  vg_lead = list(key = "lead", field_names = c("field", "offset", "default"), option_names = .vg_window_options),
  vg_first_value = list(key = "first_value", field_names = "field", option_names = .vg_window_options),
  vg_last_value = list(key = "last_value", field_names = "field", option_names = .vg_window_options),
  vg_nth_value = list(key = "nth_value", field_names = c("field", "n"), option_names = .vg_window_options)
)

for (.name in names(vg_transform_specs)) {
  .spec <- vg_transform_specs[[.name]]
  assign(.name, new_vg_transform_fn(.spec$key, .spec$field_names, .spec$option_names))
}
rm(.name, .spec)

# The \usage lines for ?vg_transforms (via @eval below): the functions are
# created in a loop above, so roxygen can't see their definitions. Built
# from the same table, so it always matches their real arguments; wrapped
# to keep each line under 80 characters.
transform_usage_doc <- function() {
  one <- function(name) {
    args <- names(formals(get(name)))
    lines <- character()
    cur <- paste0(name, "(")
    for (i in seq_along(args)) {
      piece <- paste0(args[[i]], if (i < length(args)) ", " else "")
      if (nchar(cur) + nchar(piece) > 78 && !grepl("\\($", cur)) {
        lines <- c(lines, sub(" $", "", cur))
        cur <- "  "
      }
      cur <- paste0(cur, piece)
    }
    c(lines, paste0(cur, ")"))
  }
  usage <- unlist(lapply(names(vg_transform_specs), one))
  c(paste("@usage", usage[[1]]), usage[-1])
}

#' Transform functions for use inside mapping formulas
#'
#' Transform and aggregate functions computed in the database (binning,
#' aggregates like `vg_avg()`/`vg_count()`/`vg_sum()`, and window functions
#' like `vg_rank()`/`vg_lag()`), for use as (or inside) a mark's mapping
#' formula, e.g., `x = ~vg_bin(delay, step = 10)` or `y = ~vg_count()`.
#'
#' Each function's first argument(s) are the column(s) it operates on,
#' written bare (unquoted); the remaining arguments configure it. Which
#' arguments a transform takes is shown in the Usage section; see
#' [Mosaic's documentation](https://idl.uw.edu/mosaic/) for more about each
#' one. All can also be called directly (outside a formula, no `~` needed) to
#' see what they produce.
#'
#' See [vg_value_types] for what the `<...>` notation in the Arguments
#' section (`column`, `param()`, ...) means.
#'
#' @eval transform_usage_doc()
#' @param field `<column | param()>` The column the transform operates on.
#' @param x,y `<column | param()>` The two columns of a two-column aggregate
#'   ([vg_argmax()], [vg_argmin()], [vg_covariance()], [vg_covar_pop()]).
#' @param probability `<number | param()>` For [vg_quantile()]: the quantile
#'   to compute, between 0 and 1 (e.g., `0.5` for the median).
#' @param num_buckets `<number | param()>` For [vg_ntile()]: how many
#'   groups to divide the rows into.
#' @param offset `<number | param()>` For [vg_lag()]/[vg_lead()]: how many
#'   rows back or ahead to look (default 1). For [vg_bin()]: a shift of the
#'   bin boundaries.
#' @param default `<constant | param()>` For [vg_lag()]/[vg_lead()]: the value
#'   to use when there is no row that far back or ahead.
#' @param n `<number | param()>` For [vg_nth_value()]: which row of the
#'   window frame to take (1 for the first).
#' @param interval `<"date" | "number" | "millisecond" | "second" | "minute" |
#'   "hour" | "day" | "month" | "year">` For [vg_bin()]: the kind of bins,
#'   numeric or a unit of time.
#' @param step,steps,min_step `<number>` For [vg_bin()]: the exact bin width
#'   (`step`), or else the approximate number of bins (`steps`), with a
#'   minimum bin width (`min_step`).
#' @param nice `<boolean>` For [vg_bin()]: whether to line the bins up on
#'   round values.
#' @param distinct `<boolean>` For aggregate transforms: compute over
#'   distinct values only.
#' @param order_by,partition_by `<string | character vector | param()>` Window
#'   options, for aggregate and window transforms: the column(s), as strings,
#'   that order the rows (`order_by`) and split them into separate windows
#'   (`partition_by`).
#' @param rows,range,groups `<list | numeric vector | param()>` Window
#'   options: the window frame, as a pair of offsets measured from the
#'   current row -- how far the frame reaches before it, then after it
#'   (numbers, `NULL` for unbounded, or date/time intervals such as
#'   [vg_days()]; see [vg_intervals]). `rows` counts rows, `range` measures
#'   along the `order_by` column, and `groups` counts groups of tied rows.
#' @param exclude `<"CURRENT ROW" | "GROUP" | "TIES" | "NO OTHERS">` Window
#'   option: rows to leave out of the frame.
#' @param ... Mosaic's own spellings of `order_by`, `partition_by` and
#'   `min_step` -- `orderby`, `partitionby` and `minstep` -- are also
#'   accepted.
#' @family transform functions
#' @name vg_transforms
#' @aliases vg_bin vg_column vg_date_month vg_date_month_day vg_date_day vg_centroid vg_centroid_x vg_centroid_y vg_geojson vg_argmax vg_argmin vg_avg vg_count vg_covariance vg_covar_pop vg_first vg_geomean vg_last vg_max vg_min vg_median vg_mode vg_product vg_quantile vg_stddev vg_stddev_pop vg_sum vg_variance vg_var_pop vg_row_number vg_rank vg_dense_rank vg_percent_rank vg_cume_dist vg_ntile vg_lag vg_lead vg_first_value vg_last_value vg_nth_value
#' @export vg_bin
#' @export vg_column
#' @export vg_date_month
#' @export vg_date_month_day
#' @export vg_date_day
#' @export vg_centroid
#' @export vg_centroid_x
#' @export vg_centroid_y
#' @export vg_geojson
#' @export vg_argmax
#' @export vg_argmin
#' @export vg_avg
#' @export vg_count
#' @export vg_covariance
#' @export vg_covar_pop
#' @export vg_first
#' @export vg_geomean
#' @export vg_last
#' @export vg_max
#' @export vg_min
#' @export vg_median
#' @export vg_mode
#' @export vg_product
#' @export vg_quantile
#' @export vg_stddev
#' @export vg_stddev_pop
#' @export vg_sum
#' @export vg_variance
#' @export vg_var_pop
#' @export vg_row_number
#' @export vg_rank
#' @export vg_dense_rank
#' @export vg_percent_rank
#' @export vg_cume_dist
#' @export vg_ntile
#' @export vg_lag
#' @export vg_lead
#' @export vg_first_value
#' @export vg_last_value
#' @export vg_nth_value
NULL

# The message for a call inside a mapping formula that is neither
# sql()/agg()/param() nor a known transform. Also names the closest known
# function (transform_name_suggestion(), above).
unknown_formula_call_message <- function(fn_name) {
  paste0(
    "Only simple column references (e.g., ~Date), sql()/agg(), or known ",
    "transform functions (", paste(names(vg_transform_specs), collapse = "(), "), "()) ",
    "can be used inside a mapping formula. Got a call to `", fn_name, "()`.",
    transform_name_suggestion(fn_name)
  )
}

# Decides, syntactically and WITHOUT evaluating anything, what a call found
# inside a mapping formula is: `kind = "eval"` for sql()/agg()/param() (which
# serialize_expr() evaluates, and whose arguments are self-contained), or
# `kind = "transform"` for a known transform, with its arguments matched
# (`mc`, via match_transform_call()) and its `spec`. Errors for anything else,
# exactly as serialization always did -- this is the one place that decides,
# used both by serialize_expr() at render/export time and by
# check_formula_expr() when the mark is built, so the early check accepts
# precisely what serialization accepts.
resolve_formula_call <- function(expr) {
  head <- expr[[1]]
  # `pkg::fn(x)` and the like have a call, not a name, in head position
  if (!is.symbol(head)) stop(unknown_formula_call_message(paste(deparse(head), collapse = "")), call. = FALSE)
  fn_name <- as.character(head)
  if (fn_name %in% c("sql", "agg", "param")) return(list(kind = "eval"))

  spec <- vg_transform_specs[[fn_name]]
  if (is.null(spec)) stop(unknown_formula_call_message(fn_name), call. = FALSE)
  fn <- get(fn_name, mode = "function")
  list(kind = "transform", spec = spec, mc = match_transform_call(fn_name, fn, expr))
}

# Checks the *shape* of one mapping-formula expression, and of the transforms
# nested inside it: known function names, valid argument names -- everything
# that would otherwise only fail at render/export time. Deliberately never
# evaluates anything (a symbol might name a variable, or a param, that is only
# defined later; sql()/agg()/param() arguments may too), so it can only reject
# what serialization is certain to reject. A transform's *option* arguments
# (step = 10, ...) are evaluated normally there, not read as formulas, so only
# its field arguments -- `delay` in `vg_bin(delay)`, or a nested transform --
# are followed, exactly as serialize_transform() does.
check_formula_expr <- function(expr) {
  if (!is.null(formula_literal(expr))) {
    # a constant, e.g. `~5` or `~-2.5` (serialize_channel_value(), R/channels.R)
  } else if (is.call(expr)) {
    resolved <- resolve_formula_call(expr)
    if (resolved$kind == "transform") {
      supplied <- normalize_transform_args(resolved$spec, as.list(resolved$mc)[-1])
      fields <- supplied[resolved$spec$field_names[resolved$spec$field_names %in% names(supplied)]]
      for (field in fields) check_formula_expr(field)
    }
  } else if (!(is.symbol(expr) || is.numeric(expr) || is.character(expr) || is.logical(expr) || is.null(expr))) {
    stop("Can't use this inside a mapping formula: ", paste(deparse(expr), collapse = ""), call. = FALSE)
  }
  invisible(NULL)
}

# Runs check_formula_expr() over every mapping formula (`x = ~vg_bin(delay,
# step = 10)`) and every already-built transform object among a
# mark/interactor/input/legend's arguments, so a misspelled transform option
# or function name fails where it was written instead of at export/render
# time (formulas aren't read until then). Such a spec could never have been
# serialized, so this is an error, not a warning; the message is the one
# serialization would have produced, plus which argument it was in. `where`
# says what was being built, e.g. "mark `dot`".
check_transform_calls <- function(args, where, style = "camel") {
  nms <- names(args)
  if (is.null(nms)) return(invisible(NULL))
  for (nm in nms[nzchar(nms)]) {
    value <- args[[nm]]
    exprs <- if (inherits(value, "formula")) {
      list(value[[2]])
    } else if (is_vg_transform(value)) {
      value$field
    }
    for (expr in exprs) {
      tryCatch(
        check_formula_expr(expr),
        error = function(e) {
          stop(paste0(conditionMessage(e), " (in the `", spell_names(nm, style), "` argument of ", where, ")"), call. = FALSE)
        }
      )
    }
  }
  invisible(NULL)
}
