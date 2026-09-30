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
#' A param can be kept in an R variable, e.g. `xp <- param(x)`, and used by
#' that name, including inside a formula (`~ vg_column(xp)`). So avoid giving
#' such a variable the name of a column: after `sport <- param(sport)`,
#' `stroke = ~sport` refers to the param, not the column. A param that is
#' used but never given a value -- by [vg_params()], or by an interactor or
#' input's `as =` -- is warned about when the graphic is rendered.
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

# Params a graphic uses for their value -- in a mark's channels or options,
# or a plot attribute -- but that nothing ever gives a value: not declared
# with vg_params(), not a computed param, and not set by any interactor,
# input or legend (`as =`, or a pan/zoom interactor's `x =`/`y =`). Such a
# param is empty when the graphic is drawn, so whatever uses it silently
# gets nothing (e.g., `stroke = ~sport` drawing every line in the default
# color). The usual cause: an R variable holding a param() that has the
# same name as a column, e.g. `sport <- param(sport)`, since a formula
# (`~sport`) uses such a variable as the param. References in `filter_by`
# aren't checked -- an empty selection there just means "no filter".
warn_unset_params <- function(spec) {
  # A check that goes wrong must never stop the graphic from being made; any
  # real problem with the spec is reported by serialization itself.
  tryCatch(check_unset_params(spec), error = function(e) invisible())
}

check_unset_params <- function(spec) {
  if (!is_vgspec(spec) || is.null(spec$layout)) return(invisible())
  provided <- c(names(spec$params), names(spec$computed_params))
  uses <- list()
  # `serialized` is the value as it's actually sent (a mark's options go
  # through serialize_channel_value(), which accepts more than
  # serialize_value(), e.g. `~-2.5`).
  use <- function(serialized, arg, owner, hint = NULL) {
    for (p in param_refs(serialized)) {
      uses[[length(uses) + 1]] <<- list(param = p, arg = arg, owner = owner, symbol = identical(hint, p))
    }
  }
  attr_uses <- function(attrs) {
    for (nm in names(attrs)) use(serialize_value(attrs[[nm]]), camel_to_snake(nm), "plot attribute")
  }
  for (item in layout_items(spec$layout)) {
    if (inherits(item, "vg_mark")) {
      enc <- item$encodings
      enc$data_from <- NULL
      enc$filter_by <- NULL
      enc$data_optimize <- NULL
      for (nm in names(enc)) {
        use(serialize_channel_value(nm, enc[[nm]]), camel_to_snake(nm), sprintf("of mark `%s`", item$mark),
            formula_param_symbol(enc[[nm]]))
      }
    } else if (inherits(item, c("vg_interactor", "vg_input", "vg_legend"))) {
      setters <- c(list(item$options$as), if (startsWith(item$type %||% "", "pan")) list(item$options$x, item$options$y))
      provided <- c(provided, unlist(lapply(setters, function(v) if (is_vg_param(v)) v$name)))
    } else if (is.list(item) && identical(item$kind, "attrs")) {
      attr_uses(item$attrs)
    }
  }
  attr_uses(spec$attrs)
  attr_uses(spec$plot_defaults)

  unset <- Filter(function(u) !u$param %in% provided, uses)
  for (p in unique(vapply(unset, `[[`, "", "param"))) {
    mine <- Filter(function(u) u$param == p, unset)
    owners <- unique(vapply(mine, `[[`, "", "owner"))
    wheres <- vapply(owners, function(o) {
      args <- unique(vapply(Filter(function(u) u$owner == o, mine), `[[`, "", "arg"))
      if (o == "plot attribute") {
        paste0(if (length(args) > 1) "plot attributes " else "plot attribute ", english_and(paste0("`", args, "`")))
      } else {
        paste(english_and(paste0("`", args, "`")), o)
      }
    }, character(1))
    hint <- if (any(vapply(mine, `[[`, logical(1), "symbol"))) {
      sprintf(
        paste0(" If you meant the column `%s`: an R variable named `%s` holds `param(%s)`, and",
               " a formula uses such a variable as the param -- rename the variable",
               " (e.g., `%s_p <- param(%s)`) or remove it."),
        p, p, p, p, p
      )
    } else ""
    warning(
      sprintf("Param `%s` is used by %s, but nothing gives it a value: it isn't declared with vg_params(), and no interactor or input sets it (`as =`).%s",
              p, english_and(wheres), hint),
      call. = FALSE
    )
  }
  invisible()
}

# Every mark, interactor, input and legend in a layout, plus each plot's
# attributes (as list(kind = "attrs", attrs = ...)).
layout_items <- function(layout) {
  if (is_vg_plot_fragment(layout)) {
    c(layout$items, list(list(kind = "attrs", attrs = layout$attrs)))
  } else if (is_vg_concat(layout)) {
    unlist(lapply(layout$children, layout_items), recursive = FALSE)
  } else if (inherits(layout, c("vg_input", "vg_legend", "vg_mark", "vg_interactor"))) {
    list(layout)
  } else {
    list()
  }
}

# Param names referred to in a serialized value: "$name" strings, and `$name`
# inside sql()/agg() text.
param_refs <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x)) && any(names(x) %in% c("sql", "agg"))) {
      text <- unlist(x[names(x) %in% c("sql", "agg")])
      return(unique(sub("^\\$", "", unlist(regmatches(text, gregexpr("\\$[A-Za-z_][A-Za-z0-9_]*", text))))))
    }
    return(unique(unlist(lapply(x, param_refs))))
  }
  if (is.character(x) && length(x) == 1 && grepl("^\\$[A-Za-z_][A-Za-z0-9_]*$", x)) return(sub("^\\$", "", x))
  character()
}

# If `x` is a formula whose right-hand side is a bare name for an R variable
# holding a param(), that param's name.
formula_param_symbol <- function(x) {
  if (!inherits(x, "formula") || length(x) != 2 || !is.symbol(x[[2]])) return(NULL)
  val <- tryCatch(eval(x[[2]], envir = environment(x)), error = function(e) NULL)
  if (is_vg_param(val)) val$name
}

english_and <- function(x) {
  n <- length(x)
  if (n <= 2) paste(x, collapse = " and ") else paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
}
