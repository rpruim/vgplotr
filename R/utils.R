# Names that are attributes of a *plot* (the mosaic-spec container that
# groups one or more marks/interactors on shared axes), as opposed to
# encodings/options that belong to a single mark or interactor.
#
# .vg_plot_attrs comes from mosaic's own JSON schema (R/attrs-generated.R,
# produced by data-raw/update-schema.R) -- these are the *exact* camelCase
# names mosaic-spec uses (e.g., "marginLeft", "xDomain"), the key that's
# actually stored/serialized. The R-facing spelling a caller types is
# snake_case (matching mark/interactor/scale/guide arguments elsewhere in
# the package) and gets translated to this exact key by
# canonicalize_plot_attr_names() below before anything here ever sees it.
vg_plot_level_args <- function() {
  .vg_plot_attrs
}

# Renames any snake_case plot-attribute name in `args` (e.g., `x_domain`) to
# its real camelCase mosaic-spec key (`xDomain`), via `.vg_plot_attrs_snake`
# (R/attrs-generated.R) -- the plot-attribute equivalent of what the
# generated vg_mark_*()/vg_scale_*()/etc. wrappers already do for their own
# declared formals. A name that isn't a recognized plot attribute under
# either spelling (a mark's own property, an actual typo, ...) passes
# through unchanged, so it still surfaces via warn_unknown_attrs() same as
# before.
canonicalize_plot_attr_names <- function(args) {
  nms <- names(args)
  if (is.null(nms)) return(args)
  mapped <- .vg_plot_attrs_snake[nms]
  hit <- !is.na(mapped)
  names(args)[hit] <- unname(mapped[hit])
  args
}

#' Split `...` arguments into plot-level attributes and local (mark/interactor)
#' arguments, based on `vg_plot_level_args()`.
#'
#' Plot-attribute names are accepted snake_case (translated via
#' `canonicalize_plot_attr_names()`) or already-exact-camelCase.
#'
#' `protect` names (typically from `.vg_mark_own_props`/
#' `.vg_interactor_own_props`, R/attrs-generated.R) are always kept local
#' even if they also appear in `vg_plot_level_args()` -- e.g., RectY's own
#' `inset` property collides by name with PlotAttributes' plot-wide `inset`
#' default, but an explicit `inset =` on a `vg_mark_rect_y()` call means
#' the mark's own property, not "bubble this up to the plot."
#' @noRd
split_plot_args <- function(args, protect = character()) {
  args <- canonicalize_plot_attr_names(args)
  plot_names <- setdiff(intersect(names(args), vg_plot_level_args()), protect)
  list(
    plot_attrs = args[plot_names],
    local_args = args[setdiff(names(args), plot_names)]
  )
}

#' Merge a named list of new plot-level attributes into an existing set.
#'
#' A key that hasn't been set yet is added silently. A key that is set again
#' with an identical value is a silent no-op. A key that is set again with a
#' *different* value keeps the new value but emits a warning, since the two
#' specifications are incompatible.
#'
#' Uses single-bracket assignment (`old[nm] <- new[nm]`), not `old[[nm]] <-
#' new[[nm]]`, so that an explicit `NULL` value (e.g., `xAxis = NULL`, mosaic's
#' way of hiding an axis) is stored as-is instead of deleting the attribute --
#' `old[[nm]] <- NULL` always removes the element, even when the caller meant
#' to *set* it to `NULL` rather than unset it.
#' @noRd
merge_attrs <- function(old, new, context = NULL) {
  for (nm in names(new)) {
    if (nm %in% names(old) && !identical(old[[nm]], new[[nm]])) {
      where <- if (is.null(context)) "" else paste0(" in ", context)
      warning(
        sprintf(
          "Conflicting value for `%s`%s: replacing %s with %s.",
          nm, where, deparse_short(old[[nm]]), deparse_short(new[[nm]])
        ),
        call. = FALSE
      )
    }
    old[nm] <- new[nm]
  }
  old
}

#' Warn about names that aren't recognized mosaic-spec plot attributes.
#'
#' For functions whose *entire* purpose is setting plot-level attributes
#' (`vg_attributes()`, `vg_plot_defaults()`, `vg_plot()`) -- unlike
#' `vg_mark()`/`vg_interactor()`, an unrecognized name here has no other
#' place it could still take effect (it isn't a mark/interactor encoding),
#' so it's silently inert: it'll show up in `spec$attrs`/`spec$plot_defaults`
#' and even round-trip through `to_json()`/`to_yaml()`, but never affect the
#' rendered graphic. A typo (or reaching for the wrong function -- `title`
#' belongs in `vg_meta()`, not here) would otherwise fail silently.
#' @noRd
warn_unknown_attrs <- function(names, context) {
  unknown <- setdiff(names, vg_plot_level_args())
  if (length(unknown) == 0) return(invisible(NULL))
  names_str <- paste0("`", unknown, "`", collapse = ", ")
  if (length(unknown) == 1) {
    subject <- paste0(names_str, " is not a recognized mosaic-spec plot attribute")
    pronoun <- "it"
  } else {
    subject <- paste0(names_str, " are not recognized mosaic-spec plot attributes")
    pronoun <- "they"
  }
  warning(
    sprintf(
      "In %s: %s, so %s won't affect the rendered graphic. Check spelling (mosaic's plot-attribute names are camelCase, e.g., `marginLeft`), or use vg_meta() for spec-level metadata like `title`.",
      context, subject, pronoun
    ),
    call. = FALSE
  )
}

# Warns on a mark/interactor argument whose name is a known mosaic-spec
# enum property (.vg_enum_props, R/attrs-generated.R) but whose value isn't
# one of the recognized literals -- most often a typo (e.g.
# curve = "cardinal-open" misspelled "cardinal_open") that would otherwise
# only surface as a confusing failure deep inside mosaic's own JS, far from
# the call that caused it. Only checks a plain length-1 character value: a
# formula/vg_transform/vg_sql_expr (data references) or a vg_param()
# (a reactive placeholder) is never a literal value to validate, and any
# non-character/non-scalar value can't be enum-typed to begin with (an
# enum property is always string-typed in the schema), so both pass
# through untouched rather than risk a false positive.
warn_unrecognized_enum_values <- function(args) {
  for (nm in names(args)) {
    allowed <- .vg_enum_props[[nm]]
    if (is.null(allowed)) next
    v <- args[[nm]]
    if (inherits(v, "formula") || inherits(v, "vg_transform") || inherits(v, "vg_sql_expr") || is_vg_param(v)) next
    if (!is.character(v) || length(v) != 1 || is.na(v)) next
    if (!(v %in% allowed)) {
      warning(
        sprintf(
          "`%s = \"%s\"` is not a recognized value -- check for a typo. Allowed values: %s.",
          nm, v, paste0('"', allowed, '"', collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

# Warns on a mark argument that mosaic-spec doesn't list as a property of
# that mark type (.vg_mark_own_props, R/attrs-generated.R) and that isn't
# one of vgplotr's own data-source arguments -- it would otherwise pass
# straight through into the spec, where mosaic silently ignores it (the
# classic case is `color =`: mosaic marks have `fill`/`stroke`, not
# `color`, and a mark just quietly draws in its default color). `args` is
# the mark's *local* arguments, i.e., after plot-level attributes have
# already been split off (split_plot_args()), so a plot attribute riding
# along (`width =`, `x_domain =`, ...) is never flagged. A mark type with
# no schema entry (nothing to check against) is skipped rather than
# risking a false positive. One warning per call, listing every offender.
warn_unrecognized_mark_args <- function(args, mark) {
  own <- .vg_mark_own_props[[mark]]
  nms <- names(args)
  if (is.null(own) || is.null(nms)) return(invisible(NULL))

  handled <- c(own, "data_from", "filter_by", "data_optimize")
  unknown <- setdiff(nms[nzchar(nms)], handled)
  if (length(unknown) == 0) return(invisible(NULL))

  names_str <- paste0("`", unknown, "`", collapse = ", ")
  subject <- if (length(unknown) == 1) {
    paste0(names_str, " is not a property of the `", mark, "` mark in mosaic-spec, so it won't")
  } else {
    paste0(names_str, " are not properties of the `", mark, "` mark in mosaic-spec, so they won't")
  }
  hints <- Filter(Negate(is.null), lapply(unknown, mark_arg_hint, own = own))
  warning(
    paste(c(sprintf("In mark `%s`: %s affect the rendered graphic.", mark, subject), unlist(hints)), collapse = " "),
    call. = FALSE
  )
  invisible(NULL)
}

# A suggestion for one unrecognized mark argument, or NULL if there's
# nothing useful to say. `color`/`colour` is by far the most common
# mistake -- mosaic (like Observable Plot) has no such channel, only
# `fill` (the inside of a shape) and `stroke` (its outline/line) -- and
# only suggests whichever of the two this mark actually has. Otherwise,
# looks for a property that differs only by case/underscores/dots (e.g.
# `fill_opacity` or `fillopacity` for `fillOpacity`, which is what the
# generic vg_mark() needs even though the vg_mark_*() wrappers take
# snake_case).
mark_arg_hint <- function(nm, own) {
  if (tolower(nm) %in% c("color", "colour")) {
    choices <- intersect(c("fill", "stroke"), own)
    if (length(choices) == 0) return(NULL)
    what <- c(fill = "`fill` (the inside of a shape)", stroke = "`stroke` (its outline or line)")[choices]
    return(sprintf(
      "For `%s`, use %s, e.g., `%s = ~my_column` or `%s = \"steelblue\"`.",
      nm, paste(what, collapse = " and/or "), choices[[1]], choices[[1]]
    ))
  }
  norm <- function(x) tolower(gsub("[_.]", "", x))
  close <- own[norm(own) == norm(nm)]
  if (length(close) == 0) return(NULL)
  sprintf("Did you mean `%s`?", close[[1]])
}

# An ordered factor's level order (e.g. "low" < "medium" < "high") has no
# equivalent in mosaic-spec's own data model -- every rendering path (live
# wasm/native DuckDB registration, to_json()/to_yaml() export) only ever
# sees the character labels, never the R-side ordering, so a scale that
# should respect that order silently falls back to whatever order the
# values happen to sort/appear in instead. Unlike drop_factors()/
# convert_integer64_cols(), nothing here can safely auto-fix this --
# preserving the order needs an explicit xDomain=/colorDomain=/etc.
# attribute vgplotr can't infer on its own -- so this only warns, naming
# the fix, checked once per data source rather than per mark.
warn_ordered_factor_cols <- function(df, name) {
  is_ordered_col <- vapply(df, is.ordered, logical(1))
  if (any(is_ordered_col)) {
    cols <- names(df)[is_ordered_col]
    warning(
      "Data source '", name, "' has ordered factor column(s) (",
      paste(cols, collapse = ", "), ") -- their level order isn't ",
      "preserved when rendered. Set the matching scale's domain explicitly ",
      "(e.g., x_domain = levels(", name, "$", cols[1], ")) if the order matters.",
      call. = FALSE
    )
  }
  invisible(df)
}

# Like utils::modifyList(), but preserves an explicit NULL in `overrides`
# (mosaic's way of unsetting/hiding something, e.g., `xAxis = NULL` to hide
# an axis) instead of treating it as "remove this key" -- modifyList()'s
# NULL-removes-the-element behavior is right for building up an options
# list incrementally, but wrong for spec attributes, where NULL is a real,
# meaningful value to send to mosaic, not "absent." No conflict warning
# (unlike merge_attrs()): overriding a default is the whole point here, not
# an error.
override_attrs <- function(defaults, overrides) {
  for (nm in names(overrides)) defaults[nm] <- overrides[nm]
  defaults
}

deparse_short <- function(x) {
  paste(deparse(x, width.cutoff = 30L), collapse = " ")
}

# list(x = value) without deparsing/quasiquotation, for a dynamic name.
named_list <- function(name, value) {
  out <- list(value)
  names(out) <- name
  out
}

# Sentinel default for the named (but optional) arguments on generated
# per-mark/per-interactor wrapper functions (R/marks-generated.R,
# R/interactors-generated.R) -- lets those functions expose a real,
# documented formal per schema property while still only forwarding the
# ones the caller actually supplied (as opposed to every property the
# schema knows about) on to vg_mark()/vg_interactor().
vg_unset <- structure(list(), class = "vg_unset")

drop_unset <- function(args) {
  args[!vapply(args, identical, logical(1), vg_unset)]
}

# Shared by the vg_scale_*()/vg_guide_*() constructors (R/scale.R, R/guide.R):
# builds a named list of mosaic attrs from a snake_case-argument-name ->
# camelCase-suffix lookup table (e.g., c(type = "Scale") for `xScale`),
# reading each argument's current value out of the caller's own environment
# and dropping the ones left at their vg_unset default. `prefix` is the
# scale/axis this call is for (e.g., "x", "fy").
prefixed_attrs <- function(prefix, suffixes, env) {
  vals <- drop_unset(mget(names(suffixes), envir = env))
  if (length(vals)) names(vals) <- paste0(prefix, suffixes[names(vals)])
  vals
}

# Shared tail of every vg_scale_*()/vg_guide_*() constructor: validates any
# extra (non-formal) named arguments as raw mosaic attrs, merges everything
# into the current plot fragment's attrs, and returns the updated spec (or
# fragment, if `spec` wasn't a vgspec). `attrs` are the ones already built
# from the function's own named arguments (e.g., via prefixed_attrs());
# `extra` is whatever arrived through `...` (accepted snake_case, same as
# vg_plot()/vg_plot_defaults()/vg_attributes()).
apply_plot_attrs <- function(spec, attrs, extra, context) {
  extra <- canonicalize_plot_attr_names(extra)
  warn_unknown_attrs(names(extra), context)
  attrs <- merge_attrs(attrs, extra, context = context)

  fragment <- as_vg_plot_fragment(spec)
  fragment$attrs <- merge_attrs(fragment$attrs, attrs, context = context)
  update_layout(spec, fragment)
}

# Adds `xInsetLeft`/`xInsetRight` (or `yInsetTop`/`yInsetBottom`, or the
# `fx`/`fy` equivalents) to `attrs`, and warns if the caller supplied the
# other, inapplicable pair instead -- mosaic only defines left/right insets
# for `x`/`fx` and top/bottom insets for `y`/`fy`. `vg_inset_side_suffixes`/
# `vg_position_counterpart` (R/scale-generated.R) are generated from the
# schema, not hand-maintained -- see data-raw/update-schema.R.
add_inset_attrs <- function(attrs, which, env, context) {
  own_side <- vg_inset_side_suffixes[[which]]
  other_which <- vg_position_counterpart[[which]]
  other_side <- vg_inset_side_suffixes[[other_which]]

  own_vals <- drop_unset(mget(names(own_side), envir = env))
  if (length(own_vals)) {
    names(own_vals) <- paste0(which, own_side[names(own_vals)])
    attrs <- c(attrs, own_vals)
  }

  other_vals <- drop_unset(mget(names(other_side), envir = env))
  if (length(other_vals)) {
    warning(
      sprintf(
        "In %s: %s only appl%s to the %s scale; use %s for the %s scale.",
        context,
        paste(sprintf("`%s`", names(other_vals)), collapse = ", "),
        if (length(other_vals) == 1) "ies" else "y",
        other_which,
        paste(sprintf("`%s`", names(own_side)), collapse = "/"),
        which
      ),
      call. = FALSE
    )
  }

  attrs
}

#' Create a wrapper that fixes some arguments of another function
#'
#' `wrapper_function(..f, name = value, ...)` returns a new function that
#' calls `..f` with `name = value` (and any other named arguments in `...`)
#' supplied automatically. Unlike a plain `function(...) ..f(name = value,
#' ...)` closure, the result's own formal arguments are `..f`'s real
#' formals (names, defaults, and `...`) minus the ones fixed here -- so
#' e.g., `wrapper_function(vg_scale_position, which = "x")` has the same
#' signature as `vg_scale_position()` (type, domain, ..., but no `which`),
#' which is what lets a generated wrapper like `vg_scale_x()` show its
#' actual arguments for tab completion and `?vg_scale_x` instead of an
#' opaque `(spec = NULL, ...)`. Used to build the thin
#' `vg_scale_x()`/`vg_scale_y()`/`vg_scale_fx()`/`vg_scale_fy()` and
#' `vg_guide_x()`/`vg_guide_y()`/`vg_guide_fx()`/`vg_guide_fy()` wrappers
#' (R/scale.R, R/guide.R), plus `vg_legend_color()`/`vg_legend_opacity()`/
#' `vg_legend_symbol()` (R/legend.R).
#'
#' `drop` removes formals that don't apply to this particular wrapper
#' without fixing them to a value -- e.g., `vg_scale_x()` drops
#' `inset_top`/`inset_bottom` (only `inset_left`/`inset_right` apply to
#' `x`), so its signature doesn't advertise arguments that are accepted
#' syntactically but silently do nothing (`..f` still has `...`, so a
#' caller who passes a dropped name anyway still reaches `..f`'s own
#' formal of that name and gets its normal handling -- dropping only
#' changes what the wrapper's own signature *shows*).
#'
#' @param ..f The function to wrap.
#' @param ... Argument name/value pairs to fix; each name must be one of
#'   `..f`'s own formal arguments, and is removed from the wrapper's
#'   formals.
#' @param drop Names of additional `..f` formals to remove from the
#'   wrapper's formals, without fixing them to a value. `..f` must have
#'   `...` for a dropped argument to remain reachable at all.
#' @noRd
wrapper_function <- function(..f, ..., drop = character()) {
  fixed <- list(...)
  orig_formals <- formals(..f)

  bad <- setdiff(c(names(fixed), drop), names(orig_formals))
  if (length(bad)) {
    stop(
      "wrapper_function(): `", paste(bad, collapse = "`, `"), "` ",
      if (length(bad) == 1) "is not an argument" else "are not arguments",
      " of the wrapped function.",
      call. = FALSE
    )
  }

  keep <- setdiff(names(orig_formals), c(names(fixed), drop))
  has_dots <- "..." %in% keep
  named_keep <- setdiff(keep, "...")

  forwarded <- lapply(named_keep, as.name)
  names(forwarded) <- named_keep

  call_args <- c(list(quote(..f)), fixed, forwarded)
  if (has_dots) call_args <- c(call_args, list(quote(...)))

  wrapper <- function() NULL
  formals(wrapper) <- orig_formals[keep]
  body(wrapper) <- as.call(call_args)
  environment(wrapper) <- list2env(list(..f = ..f), parent = environment(..f))

  wrapper
}
