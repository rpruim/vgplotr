# Parses vg_mark()'s `formula` shorthand (design/vg_formula.qmd) into a
# named list of one-sided mapping formulas for whichever position channels
# (x, y, fx, fy, x1, x2, y1, y2) it implies -- e.g.
# `Sepal.Length ~ Sepal.Width | ~ Species` becomes
# `list(y = ~Sepal.Length, x = ~Sepal.Width, fx = ~Species)`. Each result
# value is an ordinary one-sided formula, so it plugs straight into the same
# `serialize_formula()`/`serialize_expr()` path (R/serialize.R) that a
# user's own `y = ~Sepal.Length` already goes through -- this file never
# needs to know anything about mosaic-spec serialization.

# `~` never evaluates its operands, so `f[[2]]`/`f[[3]]` already hold the
# exact unevaluated sub-expressions the user wrote (whether `f` was reached
# by direct evaluation or by ordinary argument passing) -- no
# `substitute()`/NSE capture is needed anywhere in this file.
parse_vg_formula <- function(f, mark) {
  if (!inherits(f, "formula")) {
    stop(
      "`formula` must be a two-sided formula like `y ~ x`, optionally ",
      "followed by `| facet` (see ?vg_mark).",
      call. = FALSE
    )
  }
  env <- environment(f)
  parts <- split_position_facet(f)

  channels <- list()
  channels <- assign_terms(channels, lhs_of(parts$position), c("y", "y1", "y2"), env)
  channels <- assign_terms(channels, rhs_of(parts$position), c("x", "x1", "x2"), env)

  if (!is.null(parts$facet)) {
    if (is_call(parts$facet, "~")) {
      channels <- assign_terms(channels, lhs_of(parts$facet), "fy", env)
      channels <- assign_terms(channels, rhs_of(parts$facet), "fx", env)
    } else {
      # A bare symbol/expr after `|` with no `~` at all (e.g. `| facet`).
      # Mosaic has no real facet_wrap-style auto layout (only the explicit
      # fx/fy position channels, confirmed against mosaic-plot's own
      # PlotAttributes), so this is exactly `| ~ facet`, i.e. fx only.
      channels <- assign_terms(channels, parts$facet, "fx", env)
    }
  }

  unsupported <- setdiff(names(channels), .vg_mark_own_props[[mark]])
  if (length(unsupported)) {
    stop(
      "`formula` implies the position channel(s) ", paste(unsupported, collapse = ", "),
      ", which the \"", mark, "\" mark doesn't support.",
      call. = FALSE
    )
  }

  channels
}

# Splits `f` into its position formula (everything before a top-level `|`)
# and its facet part (NULL, a bare symbol, or a `~` call), un-nesting the
# left-associated shape R's parser produces when the facet part is itself a
# two-sided formula. Verified empirically (`quote()`) across every
# position/facet template combination in design/vg_formula.qmd:
#
#   y ~ x | facet        ->  ~(op) len 3, LHS = y (leaf),        RHS = `x | facet` (a `|` call)
#   y ~ x | fy ~ .        ->  ~(op) len 3, LHS = `y ~ x | fy` (a `~` call), RHS = `.`
#
# i.e. a two-sided facet always shows up as an extra layer of `~` wrapped
# around the "clean" shape -- detected here by `is_call(expr[[2]], "~")`
# (only possible in the deep-nesting case; an ordinary two-sided position
# formula's own LHS, e.g. `y`, is never itself a `~` call).
#
# A `(`-wrapped sub-expression is never a call to `~`/`|` at its own top
# level (its head is `` `(` ``), so this same structural test already
# leaves a parenthesized `|`/`~` untouched -- "parens revert to arithmetic"
# (design/vg_formula.qmd) falls out for free, no separate handling needed.
split_position_facet <- function(expr) {
  if (length(expr) == 3 && is_call(expr[[2]], "~")) {
    inner <- split_position_facet(expr[[2]])
    if (is.null(inner$facet)) {
      stop("Malformed `formula`: can't find the `|` that introduces faceting.", call. = FALSE)
    }
    list(
      position = inner$position,
      facet = call("~", inner$facet, expr[[3]])
    )
  } else {
    if (length(expr) == 3) {
      lhs <- expr[[2]]
      rhs <- expr[[3]]
    } else {
      lhs <- NULL
      rhs <- expr[[2]]
    }
    if (is_call(rhs, "|")) {
      list(
        position = if (is.null(lhs)) call("~", rhs[[2]]) else call("~", lhs, rhs[[2]]),
        facet = rhs[[3]]
      )
    } else {
      list(position = expr, facet = NULL)
    }
  }
}

# Merges formula-derived position channels into `args`, letting any
# explicit value for the same channel win -- with a warning only when the
# two genuinely disagree (design/vg_formula.qmd).
merge_vg_formula <- function(args, parsed, mark) {
  for (channel in names(parsed)) {
    if (is.null(args[[channel]])) {
      args[[channel]] <- parsed[[channel]]
    } else if (!identical(args[[channel]], parsed[[channel]])) {
      warning(
        "`", channel, " = ", deparse(args[[channel]]), "` overrides the `formula`-implied ",
        "value (`", deparse(parsed[[channel]]), "`) for the \"", mark, "\" mark.",
        call. = FALSE
      )
    }
  }
  args
}

is_call <- function(expr, op) {
  is.call(expr) && identical(expr[[1]], as.symbol(op))
}

lhs_of <- function(f) if (length(f) == 3) f[[2]] else NULL
rhs_of <- function(f) f[[length(f)]]

is_dot <- function(expr) identical(expr, quote(.))

# Splits a `+`-chained expression into its individual terms, without
# descending into a `(`-wrapped sub-expression (its head is `` `(` ``, never
# `+`) -- e.g. `a ~ (b + c)` keeps `(b + c)` as a single opaque term instead
# of splitting it into two channels (design/vg_formula.qmd).
split_terms <- function(expr) {
  if (is_call(expr, "+")) {
    c(split_terms(expr[[2]]), split_terms(expr[[3]]))
  } else {
    list(expr)
  }
}

# Maps `expr`'s `+`-separated terms onto `slots` (e.g. `c("y", "y1", "y2")`
# for a position formula's LHS, or a single name like `"fx"` for a facet
# channel, which never takes more than one term) and adds each as a
# one-sided formula (`~term`) to `channels`. `.`/NULL means "skip this side"
# (design/vg_formula.qmd's dot placeholder).
assign_terms <- function(channels, expr, slots, env) {
  if (is.null(expr) || is_dot(expr)) return(channels)
  terms <- split_terms(expr)
  n <- length(terms)
  if (n == 1) {
    channels[[slots[[1]]]] <- eval(call("~", terms[[1]]), envir = env)
  } else if (n == 2 && length(slots) >= 3) {
    channels[[slots[[2]]]] <- eval(call("~", terms[[1]]), envir = env)
    channels[[slots[[3]]]] <- eval(call("~", terms[[2]]), envir = env)
  } else {
    stop(
      "`formula` has ", n, " term(s) (", deparse(expr), ") where only 1",
      if (length(slots) >= 3) " or 2 are" else " is", " supported.",
      call. = FALSE
    )
  }
  channels
}
