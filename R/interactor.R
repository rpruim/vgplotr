# Which vg_interactor() types are embedded inside a plot's mark list
# (mosaic calls these "interactors"/selections: intervalX, toggle, pan, ...)
# vs. which live in the surrounding layout, as standalone widgets (mosaic
# calls these "inputs": slider, menu, table, search).
#
# .vg_interactor_types/.vg_input_types come from mosaic's own JSON schema
# (R/interactors-generated.R, produced by data-raw/update-schema.R).
vg_interactor_placement <- function(interactor) {
  if (interactor %in% .vg_interactor_types) "plot"
  else if (interactor %in% .vg_input_types) "layout"
  else stop(
    "Unknown interactor type `", interactor, "`. If this is a valid mosaic ",
    "interactor/input type, rerun data-raw/update-schema.R (it may need ",
    "MOSAIC_VERSION bumped first).",
    call. = FALSE
  )
}

#' Add an interactor (a selection like `intervalX`, or a standalone input
#' widget like a slider) to a spec
#'
#' Mosaic distinguishes interactors that live inside a plot, alongside its
#' marks (e.g., `intervalX`, which brushes a Selection), from inputs that live
#' in the surrounding layout as standalone widgets (e.g., `slider`, `menu`).
#' `vg_interactor()` covers both; which kind a given `interactor` is gets
#' looked up internally (see `vg_interactor_placement()`). One convenience
#' wrapper per type -- `vg_toggle()`, `vg_menu()`, etc. -- is generated from
#' mosaic's own JSON schema into `R/interactors-generated.R`.
#'
#' The second argument is named `interactor`, not `type`, on purpose: some
#' interactor/input types have their own unrelated option that mosaic calls
#' `type` (e.g., `vg_search()`'s `type = "prefix"`, its query mode). Naming
#' this parameter `type` would collide with that whenever both are supplied
#' -- `interactor` can never collide with a real mosaic-spec property name.
#'
#' @param spec For a plot-embedded interactor: a plot fragment or `vgspec`
#'   to add it to, or `NULL` to start a new plot with just this interactor.
#'   Layout-level inputs (e.g., `"slider"`) don't take a `spec` -- combine
#'   them with plots using [vg_vconcat()]/[vg_hconcat()] instead.
#' @param interactor The interactor/input type, e.g., `"intervalX"`, `"slider"`.
#' @param ... Options for the interactor/input (e.g., `as = param(brush)`,
#'   `label = "Bias"`, `min = 0`, `max = 100`), and/or, for plot-embedded
#'   interactors, plot-level attributes. Like [vg_mark()], `vg_interactor()`
#'   itself takes each option under mosaic-spec's own camelCase name
#'   (`filterBy =`), where the generated wrappers take snake_case
#'   (`filter_by =`). An argument that is neither an option of this
#'   interactor/input type in mosaic-spec nor (for a plot-embedded
#'   interactor) a plot-level attribute warns, since mosaic would silently
#'   ignore it.
#' @family interactor functions
#' @export
vg_interactor <- function(spec = NULL, interactor, ...) {
  build_interactor(spec, interactor, list(...), style = "camel")
}

# The body of vg_interactor(), shared with the generated wrappers (see
# vg_interactor_() below); `style` is as for build_mark() (R/mark.R).
build_interactor <- function(spec, interactor, args, style) {
  placement <- vg_interactor_placement(interactor)

  if (placement == "plot") {
    split <- split_plot_args(args, protect = .vg_interactor_own_props[[interactor]])
    warn_unrecognized_interactor_args(split$local_args, interactor, style = style)
    warn_unrecognized_enum_values(args, style)
    check_transform_calls(args, paste0("interactor `", interactor, "`"), style)
    interactor_obj <- structure(
      list(type = interactor, options = split$local_args),
      class = "vg_interactor"
    )
    fragment <- as_vg_plot_fragment(spec)
    fragment$items <- c(fragment$items, list(interactor_obj))
    fragment$attrs <- merge_attrs(fragment$attrs, split$plot_attrs, context = paste0("interactor `", interactor, "`"))
    update_layout(spec, fragment)
  } else {
    if (!is.null(spec)) {
      stop(
        "`", interactor, "` is a layout-level input; it doesn't take a spec/plot ",
        "to extend. Combine it with plots using vg_vconcat()/vg_hconcat() ",
        "instead, e.g., vg_vconcat(vg_", interactor, "(...), your_plot).",
        call. = FALSE
      )
    }
    warn_unrecognized_interactor_args(args, interactor, kind = "input", style = style)
    warn_unrecognized_enum_values(args, style)
    check_transform_calls(args, paste0("input `", interactor, "`"), style)
    structure(list(type = interactor, options = args), class = "vg_input")
  }
}

# Shared by every generated vg_<type>() wrapper (R/interactors-generated.R):
# drops whichever named arguments the caller left at their vg_unset default
# (i.e., didn't actually supply) before dispatching to vg_interactor(). The
# discriminant is always passed positionally by the generated wrappers, so
# naming this parameter `interactor` (matching vg_interactor()'s own,
# collision-safe name -- see its documentation) is enough on its own: a
# same-named real property (e.g., vg_search()'s `type`) now flows through
# `...` untouched instead of being intercepted by exact-name matching.
vg_interactor_ <- function(spec, interactor, ...) {
  build_interactor(spec, interactor, drop_unset(list(...)), style = "snake")
}

#' @export
print.vg_interactor <- function(x, ...) {
  cat("<vg_interactor:", x$type, ">\n")
  if (length(x$options)) {
    str_opt <- vapply(x$options, deparse_short, character(1))
    cat(paste0("  ", names(x$options), " = ", str_opt, collapse = "\n"), "\n")
  }
  invisible(x)
}

#' @export
print.vg_input <- function(x, ...) {
  cat("<vg_input:", x$type, ">\n")
  if (length(x$options)) {
    str_opt <- vapply(x$options, deparse_short, character(1))
    cat(paste0("  ", names(x$options), " = ", str_opt, collapse = "\n"), "\n")
  }
  invisible(x)
}
