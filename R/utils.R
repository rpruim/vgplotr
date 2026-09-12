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
