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
# serialize.R). "Option" arguments (interval, step, distinct, orderby, ...)
# are evaluated normally.

# AggregateOptions + WindowOptions from Transform.ts, shared by every
# aggregate transform; window transforms take only the WindowOptions.
.vg_window_options <- c("orderby", "partitionby", "rows", "range", "groups", "exclude")
.vg_aggregate_options <- c("distinct", .vg_window_options)

new_vg_transform_fn <- function(key, field_names, option_names) {
  spec <- list(key = key, field_names = field_names, option_names = option_names)
  arg_names <- c(field_names, option_names)
  args <- stats::setNames(rep(list(quote(expr = )), length(arg_names)), arg_names)
  rlang::new_function(
    args,
    quote(build_vg_transform(spec, match.call(), parent.frame())),
    env = environment()
  )
}

build_vg_transform <- function(spec, mc, env) {
  supplied <- as.list(mc)[-1]

  field_names_supplied <- spec$field_names[spec$field_names %in% names(supplied)]
  field <- unname(supplied[field_names_supplied])

  option_names_supplied <- spec$option_names[spec$option_names %in% names(supplied)]
  options <- lapply(supplied[option_names_supplied], function(e) serialize_value(eval(e, envir = env)))

  structure(list(key = spec$key, field = field, env = env, options = options), class = "vg_transform")
}

is_vg_transform <- function(x) inherits(x, "vg_transform")

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

      names_str <- paste0("`", unknown, "`", collapse = ", ")
      subject <- if (length(unknown) == 1) {
        paste0(names_str, " is not an argument of this transform.")
      } else {
        paste0(names_str, " are not arguments of this transform.")
      }
      suggestions <- lapply(unknown, function(nm) {
        format_suggestion(
          suggest_names(nm, valid, present = supplied),
          arg = if (length(unknown) > 1) nm
        )
      })
      suggestions <- unlist(suggestions)
      if (length(suggestions) == 0) {
        suggestions <- paste0("Its arguments are ", paste0("`", valid, "`", collapse = ", "), ".")
      }
      stop(paste(c(sprintf("In `%s()`: %s", fn_name, subject), suggestions), collapse = " "), call. = FALSE)
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
transform_name_suggestion <- function(fn_name) {
  known <- c(names(vg_transform_specs), "sql", "agg", "param")
  prefixed <- paste0("vg_", fn_name)
  hits <- if (!startsWith(fn_name, "vg_") && prefixed %in% known) {
    prefixed
  } else {
    bare_length <- nchar(gsub("[_.]", "", sub("^vg_", "", fn_name)))
    similar_names(fn_name, known, limit = max(1L, bare_length %/% 3L))
  }
  if (length(hits) == 0) return("")
  paste0(" ", format_suggestion(paste0(hits, "()")))
}

#' @export
print.vg_transform <- function(x, ...) {
  field_str <- vapply(x$field, deparse_short, character(1))
  cat("<vg_transform: ", x$key, "(", paste(field_str, collapse = ", "), ")>\n", sep = "")
  if (length(x$options)) {
    str_opt <- vapply(x$options, deparse_short, character(1))
    cat(paste0("  ", names(x$options), " = ", str_opt, collapse = "\n"), "\n")
  }
  invisible(x)
}

# name (the R function name, and the lookup key used when recognizing an
# unevaluated call inside a formula) -> list(key, field_names, option_names)
vg_transform_specs <- list(
  vg_bin = list(key = "bin", field_names = "field", option_names = c("interval", "step", "steps", "minstep", "nice", "offset")),
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

#' Transform functions for use inside mapping formulas
#'
#' Transform and aggregate functions from mosaic's SQL layer (binning,
#' aggregates like `vg_avg()`/`vg_count()`/`vg_sum()`, and window functions
#' like `vg_rank()`/`vg_lag()`), for use as (or inside) a mark's mapping
#' formula, e.g., `x = ~vg_bin(delay, step = 10)` or `y = ~vg_count()`.
#'
#' Each function's first argument(s) are the field(s) (column names or
#' nested expressions) it operates on; remaining named arguments configure
#' it (see mosaic's own documentation for what each transform accepts --
#' these mirror `@uwdata/mosaic-spec`'s `Transform.ts` exactly). All can also
#' be called directly (outside a formula, no `~` needed) to inspect the
#' resulting `vg_transform` object.
#'
#' @param field,x,y,probability,num_buckets,default,n The field(s) a given
#'   transform operates on; which of these (if any) a specific transform
#'   accepts is shown in its own Usage line above.
#' @param interval,step,steps,minstep,nice,offset Binning options, for
#'   `vg_bin()` only.
#' @param distinct Only for aggregate transforms: compute over distinct
#'   values only.
#' @param orderby,partitionby,rows,range,groups,exclude Window options,
#'   shared by aggregate and window transforms: control the ordering,
#'   partitioning, and frame of the window the transform is computed over.
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
