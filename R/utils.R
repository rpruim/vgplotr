# Names that are attributes of a *plot* (the mosaic-spec container that
# groups one or more marks/interactors on shared axes), as opposed to
# encodings/options that belong to a single mark or interactor.
#
# .vg_plot_attrs comes from mosaic's own JSON schema (R/attrs-generated.R,
# produced by data-raw/update-schema.R) -- note these are the *exact*
# camelCase names mosaic-spec uses (e.g. "marginLeft", "xDomain"), not a
# snake_case translation: there's no case-conversion layer for plot
# attributes, they're passed straight through into the JSON spec, so the
# name used here has to be the name mosaic itself expects.
vg_plot_level_args <- function() {
  .vg_plot_attrs
}

#' Split `...` arguments into plot-level attributes and local (mark/interactor)
#' arguments, based on `vg_plot_level_args()`.
#' @noRd
split_plot_args <- function(args) {
  plot_names <- intersect(names(args), vg_plot_level_args())
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
    old[[nm]] <- new[[nm]]
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
#' rendered plot. A typo (or reaching for the wrong function -- `title`
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
      "In %s: %s, so %s won't affect the rendered plot. Check spelling (mosaic's plot-attribute names are camelCase, e.g. `marginLeft`), or use vg_meta() for spec-level metadata like `title`.",
      context, subject, pronoun
    ),
    call. = FALSE
  )
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
# camelCase-suffix lookup table (e.g. c(type = "Scale") for `xScale`),
# reading each argument's current value out of the caller's own environment
# and dropping the ones left at their vg_unset default. `prefix` is the
# scale/axis this call is for (e.g. "x", "fy").
prefixed_attrs <- function(prefix, suffixes, env) {
  vals <- drop_unset(mget(names(suffixes), envir = env))
  if (length(vals)) names(vals) <- paste0(prefix, suffixes[names(vals)])
  vals
}

# Shared tail of every vg_scale_*()/vg_guide_*() constructor: validates any
# extra (non-formal) named arguments as raw mosaic attrs, merges everything
# into the current plot fragment's attrs, and returns the updated spec (or
# fragment, if `spec` wasn't a vgspec). `attrs` are the ones already built
# from the function's own named arguments (e.g. via prefixed_attrs());
# `extra` is whatever arrived through `...`.
apply_plot_attrs <- function(spec, attrs, extra, context) {
  warn_unknown_attrs(names(extra), context)
  attrs <- merge_attrs(attrs, extra, context = context)

  fragment <- as_vg_plot_fragment(spec)
  fragment$attrs <- merge_attrs(fragment$attrs, attrs, context = context)
  update_layout(spec, fragment)
}

#' Create a wrapper that fixes some arguments of another function
#'
#' `wrapper_function(..f, name = value, ...)` returns a new function that
#' calls `..f` with `name = value` (and any other named arguments in `...`)
#' supplied automatically. Unlike a plain `function(...) ..f(name = value,
#' ...)` closure, the result's own formal arguments are `..f`'s real
#' formals (names, defaults, and `...`) minus the ones fixed here -- so
#' e.g. `wrapper_function(vg_scale_position, which = "x")` has the same
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
#' without fixing them to a value -- e.g. `vg_scale_x()` drops
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
