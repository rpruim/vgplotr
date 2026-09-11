#' Create a new mosaic vgplot specification
#'
#' `vg_create()` starts a new `vgspec`, the top-level object that other
#' `vg_*()` functions are piped through to add data sources, params, marks,
#' interactors, and layout. Marks and interactors can be piped directly onto
#' a freshly created spec (e.g. `vg_create() |> vg_dot(x = ~a, y = ~b)`) as
#' long as the spec only needs a single plot; a spec with multiple plots
#' needs an explicit layout (`vg_vconcat()`/`vg_hconcat()`, not yet
#' implemented).
#'
#' @param data Optional default data source name/data frame for the spec.
#'   (Full support for passing an in-memory data frame is not yet
#'   implemented -- see the design notes.)
#' @param ... Top-level plot defaults (mosaic-spec's `plotDefaults`): applied
#'   to every plot in the spec, as opposed to one plot's own attributes (set
#'   via [vg_plot()]) or a single mark's encodings. Equivalent to calling
#'   [vg_attributes()] right after `vg_create()`.
#' @export
vg_create <- function(data = NULL, ...) {
  structure(
    list(
      meta = list(),
      data = list(),
      params = list(),
      config = list(),
      layout = NULL,
      plot_defaults = list(...)
    ),
    class = "vgspec"
  )
}

is_vgspec <- function(x) inherits(x, "vgspec")

#' Set metadata (title, description) on a vgspec
#' @param spec A `vgspec`.
#' @param ... Named metadata fields, e.g. `title =`, `description =`.
#' @export
vg_meta <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  spec$meta <- utils::modifyList(spec$meta, list(...))
  spec
}

#' Add a data source to a vgspec
#' @param spec A `vgspec`.
#' @param name The name other parts of the spec use to refer to this data
#'   (via `data_from =`/`filter_by =` on marks and interactors).
#' @param ... Data source options, e.g. `file =`, `query =`, `where =`.
#' @export
vg_data <- function(spec, name, ...) {
  stopifnot(is_vgspec(spec))
  spec$data[[name]] <- list(...)
  spec
}

#' Declare Params/Selections on a vgspec
#' @param spec A `vgspec`.
#' @param ... Named params, e.g. `point = 0`, or selections, e.g.
#'   `brush = vg_selection("crossfilter")` (selection support not yet
#'   implemented).
#' @export
vg_params <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  spec$params <- utils::modifyList(spec$params, list(...))
  spec
}

#' Set plot defaults on a vgspec
#'
#' Sets mosaic-spec's top-level `plotDefaults`: attributes applied to *every*
#' plot in the spec, not just the current one. This is distinct from a single
#' plot's own attributes (set via arguments to [vg_plot()], possibly
#' accumulated from several marks/interactors -- see [vg_plot()] for that
#' merge policy) and from a single mark's encodings. If per-plot or
#' per-mark attribute-setting is needed later, that should get its own
#' function rather than overloading this one.
#'
#' @param spec A `vgspec`.
#' @param ... Named plot-default attributes, e.g. `width =`, `height =`.
#' @export
vg_attributes <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  spec$plot_defaults <- merge_attrs(spec$plot_defaults, list(...), context = "vg_attributes()")
  spec
}

#' @export
print.vgspec <- function(x, ...) {
  cat("<vgspec>\n")
  if (length(x$meta)) cat("  meta:", paste(names(x$meta), collapse = ", "), "\n")
  if (length(x$data)) cat("  data:", paste(names(x$data), collapse = ", "), "\n")
  if (length(x$params)) cat("  params:", paste(names(x$params), collapse = ", "), "\n")
  if (length(x$plot_defaults)) {
    str_pd <- vapply(x$plot_defaults, deparse_short, character(1))
    cat("  plot_defaults:", paste0(names(x$plot_defaults), " = ", str_pd, collapse = ", "), "\n")
  }
  if (is.null(x$layout)) {
    cat("  layout: (empty)\n")
  } else {
    cat("  layout:\n")
    print(x$layout)
  }
  invisible(x)
}
