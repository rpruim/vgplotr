# Which vg_interactor() types are embedded inside a plot's mark list
# (mosaic calls these "interactors"/selections: intervalX, toggle, pan, ...)
# vs. which live in the surrounding layout, as standalone widgets (mosaic
# calls these "inputs": slider, menu, table, search, ...).
#
# Hand-maintained for now; see the note on vg_plot_level_args() in utils.R --
# this should eventually come from the mosaic-spec schema instead.
vg_interactor_placement <- function(type) {
  plot_embedded <- c(
    "intervalX", "intervalY", "interval",
    "toggle", "toggleX", "toggleY",
    "panX", "panY", "panZoom", "pan",
    "highlight", "nearest", "nearestX", "nearestY"
  )
  layout_level <- c("slider", "menu", "table", "search")

  if (type %in% plot_embedded) "plot"
  else if (type %in% layout_level) "layout"
  else stop(
    "Unknown interactor type `", type, "`. If this is a valid mosaic ",
    "interactor/input type, it needs to be added to vg_interactor_placement().",
    call. = FALSE
  )
}

#' Add an interactor (a selection like `intervalX`, or a standalone input
#' widget like a slider) to a spec
#'
#' Mosaic distinguishes interactors that live inside a plot, alongside its
#' marks (e.g. `intervalX`, which brushes a Selection), from inputs that live
#' in the surrounding layout as standalone widgets (e.g. `slider`, `menu`).
#' `vg_interactor()` covers both; which kind a given `type` is gets looked up
#' internally (see `vg_interactor_placement()`).
#'
#' @param spec A plot fragment or `vgspec` to add this interactor to, or
#'   `NULL` to start a new plot with just this interactor.
#' @param type The interactor/input type, e.g. `"intervalX"`, `"slider"`.
#' @param ... Options for the interactor (e.g. `as = param(brush)`), and/or,
#'   for plot-embedded interactors, plot-level attributes.
#' @export
vg_interactor <- function(spec = NULL, type, ...) {
  placement <- vg_interactor_placement(type)

  if (placement == "plot") {
    split <- split_plot_args(list(...))
    interactor_obj <- structure(
      list(type = type, options = split$local_args),
      class = "vg_interactor"
    )
    fragment <- as_vg_plot_fragment(spec)
    fragment$items <- c(fragment$items, list(interactor_obj))
    fragment$attrs <- merge_attrs(fragment$attrs, split$plot_attrs, context = paste0("interactor `", type, "`"))
    update_layout(spec, fragment)
  } else {
    stop(
      "`", type, "` is a layout-level input, not a plot-embedded interactor. ",
      "Placing standalone inputs in the layout (alongside plots, via ",
      "vg_vconcat()/vg_hconcat()) isn't implemented yet.",
      call. = FALSE
    )
  }
}

#' @rdname vg_interactor
#' @export
vg_interval_x <- function(spec = NULL, ...) vg_interactor(spec, "intervalX", ...)

#' @rdname vg_interactor
#' @export
vg_toggle <- function(spec = NULL, ...) vg_interactor(spec, "toggle", ...)

#' @export
print.vg_interactor <- function(x, ...) {
  cat("<vg_interactor:", x$type, ">\n")
  if (length(x$options)) {
    str_opt <- vapply(x$options, deparse_short, character(1))
    cat(paste0("  ", names(x$options), " = ", str_opt, collapse = "\n"), "\n")
  }
  invisible(x)
}
