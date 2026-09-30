#' Params computed from other params and data
#'
#' `vg_computed_param()` defines a param whose value is computed from other
#' params -- and optionally from a data source -- and kept up to date: when a
#' param it uses changes (a slider moves, a menu changes, a brush is dragged),
#' it is recomputed, and anything that uses it (a mark, a label, another
#' computed param) updates too. It is the equivalent of attaching event
#' listeners to the params it uses, without writing any JavaScript.
#'
#' Each value is an R expression, written as a one-sided formula, that is
#' translated to SQL and evaluated by DuckDB in the browser:
#'
#' * A bare name is a param if one of that name is declared with
#'   [vg_params()] (or is itself a computed param) -- declare params before
#'   using them here. Any other bare name is a column of the `data_from` data
#'   source.
#' * `param(name)` is always a param, and `.data$name` always a column, for
#'   when a param and a column share a name.
#' * Numbers, strings, `TRUE`/`FALSE` and `NA` are constants.
#' * Arithmetic (`+ - * / ^ %% %/%`), comparisons (`== != < > <= >=`) and
#'   logic (`& | !`) work as in R. So do `ifelse()`/`if_else()`, `is.na()`,
#'   `mean()` (SQL `avg()`), `n()` (`count(*)`), `pmin()`/`pmax()`
#'   (`least()`/`greatest()`), `nchar()`, `toupper()`/`tolower()` and
#'   `paste0()` (`concat()`).
#' * Any other function is passed to DuckDB under the same name -- e.g.
#'   `max()`, `sum()`, `round()`, `abs()`, `sqrt()`, `exp()`, `ln()`,
#'   `median()`, `quantile_cont()`.
#' * [sql()] inserts raw SQL, with params written as `$name`.
#'
#' With `data_from`, the expression is evaluated over that data source, so it can
#' use aggregates of its columns -- e.g. `~ scale * max(price)`. With
#' `filter_by`, only the rows the given selection currently includes are
#' used, and the param is also recomputed whenever the selection changes --
#' e.g. the number of rows inside a brush, `~ n()`.
#'
#' A computed param is recomputed asynchronously (it's a small database
#' query), so it lags the params it uses by a moment.
#'
#' Computed params and [vg_on_change()] handlers are vgplotr additions, not
#' part of Mosaic's spec format: [to_json()]/[to_yaml()] write them under a
#' top-level `"vgplotr"` key (unless `vgplotr_keys = FALSE`), which Mosaic's
#' own tools don't understand.
#'
#' @param spec A `vgspec`.
#' @param ... Named one-sided formulas (or [sql()] expressions), one per
#'   computed param, e.g. `area = ~ width * height`.
#' @param data_from Optional data source (see [vg_data()]) whose columns the
#'   expressions can use: its name, or its position as an integer (`1L` for
#'   the first; note the `L`).
#' @param filter_by Optional selection (a [param()]) restricting `data_from` to
#'   the rows it includes.
#' @return The updated `vgspec`.
#' @seealso [vg_on_change()] to update params when another one changes.
#' @family spec functions
#' @export
#' @examples
#' vg_create() |>
#'   vg_params(width = 10, height = 5) |>
#'   vg_computed_param(area = ~ width * height)
#'
#' # a param times the largest value in a column
#' vg_create() |>
#'   vg_data(name = "sales", data = data.frame(price = c(3, 8, 5))) |>
#'   vg_params(scale = 2) |>
#'   vg_computed_param(top = ~ scale * max(price), data_from = "sales")
vg_computed_param <- function(spec, ..., data_from = NULL, filter_by = NULL) {
  check_is_vgspec(spec)
  defs <- list(...)
  if (length(defs) == 0 || is.null(names(defs)) || any(!nzchar(names(defs)))) {
    stop("Give each computed param a name, e.g. `area = ~ width * height`.", call. = FALSE)
  }
  from <- check_from(spec, data_from)
  filter_name <- selection_name(filter_by, "filter_by")
  for (name in names(defs)) {
    if (name %in% names(spec$params) || name %in% names(spec$computed_params)) {
      stop("A param named `", name, "` already exists.", call. = FALSE)
    }
    known <- c(names(spec$params), names(spec$computed_params))
    translated <- translate_derived(defs[[name]], known, from, paste0("computed param `", name, "`"))
    spec$computed_params[[name]] <- c(translated, list(from = from, filter_by = filter_name))
  }
  spec
}

#' Update params when another param or selection changes
#'
#' `vg_on_change()` attaches a handler to a param or selection: whenever its
#' value changes, each action runs, setting a param to a new value -- e.g.
#' resetting a zoom level when a different data set is chosen from a menu.
#' It is the equivalent of an event listener, without writing any
#' JavaScript. To keep one param in step with others, [vg_computed_param()]
#' is usually simpler.
#'
#' @param spec A `vgspec`.
#' @param trigger The param or selection to watch, e.g. `param(dataset)`.
#' @param ... One or more actions, each made with `vg_set_param()`.
#' @return The updated `vgspec`.
#' @seealso [vg_computed_param()], which describes the expressions a value
#'   can use.
#' @family spec functions
#' @export
#' @examples
#' vg_create() |>
#'   vg_params(dataset = "a", zoom = 1, clicks = 0) |>
#'   vg_on_change(
#'     param(dataset),
#'     vg_set_param(param(zoom), 1),
#'     vg_set_param(param(clicks), ~ clicks + 1)
#'   )
vg_on_change <- function(spec, trigger, ...) {
  check_is_vgspec(spec)
  trigger_name <- selection_name(trigger, "trigger")
  if (is.null(trigger_name)) stop("`trigger` must be a param, e.g. `param(dataset)`.", call. = FALSE)
  known <- c(names(spec$params), names(spec$computed_params))
  if (!trigger_name %in% known) {
    stop("`", trigger_name, "` isn't a declared param; declare it with vg_params() first.", call. = FALSE)
  }
  actions <- list(...)
  if (length(actions) == 0 || !all(vapply(actions, inherits, logical(1), "vg_set_param"))) {
    stop("Give vg_on_change() one or more actions made with vg_set_param().", call. = FALSE)
  }
  resolved <- lapply(actions, function(a) {
    if (identical(a$target, trigger_name)) {
      stop("A handler can't set the param it watches (`", trigger_name, "`).", call. = FALSE)
    }
    if (a$target %in% names(spec$computed_params)) {
      stop("`", a$target, "` is a computed param, so it can't also be set by vg_set_param().", call. = FALSE)
    }
    if (!a$target %in% names(spec$params)) {
      stop("`", a$target, "` isn't a declared param; declare it with vg_params() first.", call. = FALSE)
    }
    a$from <- check_from(spec, a$from)
    out <- list(target = a$target)
    if (is_derived_expr(a$value)) {
      out <- c(out, translate_derived(a$value, known, a$from, paste0("vg_set_param(", a$target, ")")),
               list(from = a$from, filter_by = a$filter_by))
    } else {
      out$value <- a$value
    }
    out
  })
  spec$on_change <- c(spec$on_change, list(list(trigger = trigger_name, actions = resolved)))
  check_on_change_cycles(spec)
  spec
}

#' @rdname vg_on_change
#' @param param The param to set, e.g. `param(zoom)`.
#' @param value Its new value: a constant (`1`, `"all"`, `TRUE`), or an
#'   expression computed like [vg_computed_param()]'s, e.g. `~ clicks + 1`.
#' @param data_from,filter_by As for [vg_computed_param()], for a `value`
#'   computed from a data source.
#' @export
vg_set_param <- function(param, value, data_from = NULL, filter_by = NULL) {
  target <- selection_name(param, "param")
  if (is.null(target)) stop("`param` must be a param, e.g. `param(zoom)`.", call. = FALSE)
  if (!is_derived_expr(value) && !(is.atomic(value) && length(value) == 1)) {
    stop("`value` must be a single constant or a formula, e.g. `~ clicks + 1`.", call. = FALSE)
  }
  structure(
    list(target = target, value = value, from = data_from, filter_by = selection_name(filter_by, "filter_by")),
    class = "vg_set_param"
  )
}

#' @export
print.vg_set_param <- function(x, ...) {
  value <- if (inherits(x$value, "formula")) deparse(x$value) else format(x$value)
  cat("<vg_set_param:", x$target, "<-", value, ">\n")
  invisible(x)
}

# --- helpers ---------------------------------------------------------------

check_is_vgspec <- function(spec) {
  if (!is_vgspec(spec)) stop("`spec` must be a vgspec, e.g., from vg_create().", call. = FALSE)
}

# `data_from` as a data source name (an integer index resolved), or NULL.
check_from <- function(spec, from) {
  if (is.null(from)) return(NULL)
  if (is.integer(from) && length(from) == 1) return(resolve_data_from_index(from, names(spec$data)))
  if (!is.character(from) || length(from) != 1) {
    stop("`data_from` must be a data source's name or position, e.g. `data_from = \"sales\"`.", call. = FALSE)
  }
  if (!from %in% names(spec$data)) {
    stop("There's no data source named `", from, "`; add it with vg_data() first.", call. = FALSE)
  }
  from
}

# The name of a param()/selection argument, a string naming one, or NULL.
selection_name <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (is_vg_param(x)) return(sub("^\\$", "", format(x)))
  if (is.character(x) && length(x) == 1) return(x)
  stop("`", arg, "` must be a param, e.g. `param(name)`.", call. = FALSE)
}

is_derived_expr <- function(x) {
  (inherits(x, "formula") && length(x) == 2) || is_vg_sql_expr(x)
}

# Translates a computed-param expression to SQL, returning the SQL text and
# the names of the params it uses (so the browser knows what to listen to).
translate_derived <- function(x, known, from, where) {
  if (is_vg_sql_expr(x)) {
    text <- x$text
  } else if (inherits(x, "formula") && length(x) == 2) {
    ctx <- new.env()
    ctx$known <- known
    ctx$env <- environment(x)
    ctx$columns <- character()
    text <- tryCatch(
      derived_sql(x[[2]], ctx),
      error = function(e) stop("In ", where, ": ", conditionMessage(e), call. = FALSE)
    )
    if (length(ctx$columns) && is.null(from)) {
      stop(
        "In ", where, ": ", paste0("`", unique(ctx$columns), "`", collapse = ", "),
        " isn't a declared param. If it's a column, give the data source with ",
        "`data_from = `; if it's a param, declare it with vg_params() first.",
        call. = FALSE
      )
    }
  } else {
    stop("In ", where, ": use a one-sided formula, e.g. `~ width * height`, or sql().", call. = FALSE)
  }
  params <- unique(regmatches(text, gregexpr("\\$[A-Za-z_][A-Za-z0-9_]*", text))[[1]])
  params <- sub("^\\$", "", params)
  unknown <- setdiff(params, known)
  if (length(unknown)) {
    stop("In ", where, ": ", paste0("`", unknown, "`", collapse = ", "),
         " isn't a declared param; declare it with vg_params() first.", call. = FALSE)
  }
  list(sql = text, params = params)
}

sql_identifier <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')

.derived_binary <- c(
  "+" = "+", "-" = "-", "*" = "*", "/" = "/", "%%" = "%", "%/%" = "//",
  "==" = "=", "!=" = "<>", "<" = "<", ">" = ">", "<=" = "<=", ">=" = ">=",
  "&" = "AND", "&&" = "AND", "|" = "OR", "||" = "OR"
)
.derived_renamed <- c(
  mean = "avg", pmin = "least", pmax = "greatest", nchar = "length",
  toupper = "upper", tolower = "lower", paste0 = "concat"
)

# One R expression as SQL text. `ctx$known` are the declared param names;
# column names used are collected in `ctx$columns`.
derived_sql <- function(expr, ctx) {
  if (is.null(expr)) return("NULL")
  if (is.symbol(expr)) {
    name <- as.character(expr)
    if (name %in% ctx$known) return(paste0("$", name))
    val <- tryCatch(eval(expr, ctx$env), error = function(e) NULL)
    if (is_vg_param(val)) return(format(val))
    ctx$columns <- c(ctx$columns, name)
    return(sql_identifier(name))
  }
  if (is.atomic(expr) && length(expr) == 1) {
    if (is.na(expr)) return("NULL")
    return(sql_literal(expr))
  }
  if (!is.call(expr)) stop("can't translate `", deparse(expr), "` to SQL.", call. = FALSE)

  fn <- if (is.symbol(expr[[1]])) as.character(expr[[1]]) else ""
  args <- as.list(expr)[-1]
  sub_sql <- function(e) derived_sql(e, ctx)

  if (fn == "(") return(paste0("(", sub_sql(args[[1]]), ")"))
  if (fn == "param") return(paste0("$", as.character(args[[1]])))
  if (fn == "$" && identical(args[[1]], quote(.data))) {
    return(sql_identifier(as.character(args[[2]])))
  }
  if (fn == "[[" && identical(args[[1]], quote(.data))) {
    return(sql_identifier(eval(args[[2]], ctx$env)))
  }
  if (fn == "sql") return(paste0("(", eval(expr, ctx$env)$text, ")"))
  if (fn %in% c("-", "+") && length(args) == 1) return(paste0(fn, "(", sub_sql(args[[1]]), ")"))
  if (fn == "!") return(paste0("(NOT ", sub_sql(args[[1]]), ")"))
  if (fn == "^") return(paste0("power(", sub_sql(args[[1]]), ", ", sub_sql(args[[2]]), ")"))
  if (fn %in% names(.derived_binary) && length(args) == 2) {
    return(paste0("(", sub_sql(args[[1]]), " ", .derived_binary[[fn]], " ", sub_sql(args[[2]]), ")"))
  }
  if (fn %in% c("ifelse", "if_else")) {
    return(paste0("(CASE WHEN ", sub_sql(args[[1]]), " THEN ", sub_sql(args[[2]]),
                  " ELSE ", sub_sql(args[[3]]), " END)"))
  }
  if (fn == "is.na") return(paste0("(", sub_sql(args[[1]]), " IS NULL)"))
  if (fn == "n" && length(args) == 0) return("count(*)")
  if (!nzchar(fn) || !grepl("^[A-Za-z_][A-Za-z0-9_.]*$", fn) || grepl(".", fn, fixed = TRUE)) {
    stop("can't translate `", paste(deparse(expr), collapse = ""), "` to SQL.", call. = FALSE)
  }
  if (!is.null(names(args)) && any(nzchar(names(args)))) {
    stop("named arguments aren't supported in `", paste(deparse(expr), collapse = ""), "`.", call. = FALSE)
  }
  sql_fn <- if (fn %in% names(.derived_renamed)) .derived_renamed[[fn]] else fn
  paste0(sql_fn, "(", paste(vapply(args, sub_sql, character(1)), collapse = ", "), ")")
}

# vg_on_change() handlers must not set params in a loop: A's change sets B,
# whose change sets A (directly or through computed params). An edge runs
# from each param to every param recomputed or set when it changes.
check_on_change_cycles <- function(spec) {
  edges <- list()
  add <- function(from, to) edges[[from]] <<- unique(c(edges[[from]], to))
  for (nm in names(spec$computed_params)) {
    deps <- c(spec$computed_params[[nm]]$params, spec$computed_params[[nm]]$filter_by)
    for (d in deps) add(d, nm)
  }
  for (h in spec$on_change) for (a in h$actions) add(h$trigger, a$target)
  visiting <- character()
  done <- character()
  visit <- function(node, path) {
    if (node %in% path) {
      loop <- c(path[match(node, path):length(path)], node)
      stop("These vg_on_change() handlers would update params in a loop: ",
           paste(loop, collapse = " -> "), ".", call. = FALSE)
    }
    if (node %in% done) return()
    for (nxt in edges[[node]]) visit(nxt, c(path, node))
    done <<- c(done, node)
  }
  for (node in names(edges)) visit(node, character())
  invisible()
}

# What vgplotr.js reads (`x.derived`), and what to_json()/to_yaml() write
# under the "vgplotr" key: computed params in dependency order, then the
# change handlers. NULL if the spec has neither.
derived_payload <- function(spec) {
  if (!is_vgspec(spec) || (!length(spec$computed_params) && !length(spec$on_change))) return(NULL)
  computed <- lapply(names(spec$computed_params), function(nm) {
    d <- spec$computed_params[[nm]]
    drop_nulls(list(name = nm, sql = d$sql, params = as.list(d$params), from = d$from, filterBy = d$filter_by))
  })
  on_change <- lapply(spec$on_change, function(h) {
    list(trigger = h$trigger, actions = lapply(h$actions, function(a) {
      drop_nulls(list(target = a$target, value = a$value, sql = a$sql, params = if (!is.null(a$sql)) as.list(a$params),
                      from = a$from, filterBy = a$filter_by))
    }))
  })
  drop_nulls(list(computedParams = if (length(computed)) computed, onChange = if (length(on_change)) on_change))
}

drop_nulls <- function(x) x[!vapply(x, is.null, logical(1))]

# Computed params must exist as Mosaic params (so marks and inputs can use
# them), starting out as null until first computed.
params_with_computed <- function(spec) {
  params <- spec$params
  for (nm in setdiff(names(spec$computed_params), names(params))) params[[nm]] <- NA
  params
}
