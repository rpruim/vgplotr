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
#' @param ... Named values routed automatically to wherever mosaic-spec
#'   allows them: a known plot-level attribute (e.g. `width =`, `height =`)
#'   goes to the spec's own top level (as if passed to [vg_attributes()]);
#'   anything else goes to `meta` (as if passed to [vg_meta()]), since
#'   mosaic-spec's metadata accepts arbitrary keys (e.g. `title =`). To set
#'   mosaic-spec's `plotDefaults` (applied to *every* plot, as opposed to
#'   just the spec's own top level) instead, call [vg_plot_defaults()]
#'   explicitly -- it's never chosen automatically, since every name valid
#'   there is also valid at the top level, which is preferred.
#' @export
vg_create <- function(data = NULL, ...) {
  routed <- route_spec_args(list(...))
  structure(
    list(
      meta = routed$meta,
      data = list(),
      params = list(),
      config = list(),
      layout = NULL,
      attrs = routed$attrs,
      plot_defaults = list()
    ),
    class = "vgspec"
  )
}

is_vgspec <- function(x) inherits(x, "vgspec")

# Routes named arguments (from vg_create()'s `...`) to the top-level `attrs`
# bucket when the name is a known plot-level attribute (vg_plot_level_args()
# -- mosaic-spec's PlotAttributes, the same type Plot's own fields and
# plotDefaults both use), or to `meta` otherwise (mosaic-spec's Meta type
# accepts arbitrary keys, so it's a safe catch-all for anything else, e.g.
# `title`). plotDefaults is deliberately not a third destination here: since
# it accepts exactly the same names as the top level, top level always wins
# under a "first match" priority -- plotDefaults is only reachable via an
# explicit call to vg_plot_defaults().
route_spec_args <- function(args) {
  is_attr <- names(args) %in% vg_plot_level_args()
  list(attrs = args[is_attr], meta = args[!is_attr])
}

#' Set metadata (title, description) on a vgspec
#'
#' Mosaic-spec's own `meta` is inert as far as mosaic's JS runtime is
#' concerned -- it never reads `meta` when building the DOM, so this is
#' pure metadata (e.g. for a spec browser/gallery tool, or round-tripping
#' through [to_json()]/[to_yaml()]), not something that appears on the
#' rendered plot by itself. As a vgplotr-level convenience, [vg_render()]
#' *does* render `meta$title` (as a caption above the widget); `description`/
#' `credit` remain inert for now.
#'
#' @param spec A `vgspec`.
#' @param ... Named metadata fields, e.g. `title =`, `description =`.
#' @export
vg_meta <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  spec$meta <- utils::modifyList(spec$meta, list(...))
  spec
}

#' Add a data source to a vgspec
#'
#' A data frame passed as `data` is loaded into DuckDB directly (see
#' [vg_render()]); other arguments (`file =`, `query =`, `where =`, ...)
#' describe a source for mosaic's own data loading instead.
#' @param spec A `vgspec`.
#' @param name The name other parts of the spec use to refer to this data
#'   (via `data_from =`/`filter_by =` on marks and interactors).
#' @param data An optional data frame to use as this data source.
#' @param ... Data source options, e.g. `file =`, `query =`, `where =`.
#' @export
vg_data <- function(spec, name, data = NULL, ...) {
  stopifnot(is_vgspec(spec))
  spec$data[[name]] <- if (!is.null(data)) list(data = data) else list(...)
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

#' Set top-level attributes on a vgspec
#'
#' Sets attributes directly on the spec's own top level -- e.g. for a
#' single-plot spec, sibling keys to `plot:` such as `width`/`height`,
#' exactly like mosaic's own example specs write them. This is distinct
#' from [vg_plot_defaults()] (mosaic-spec's `plotDefaults`, applied to
#' *every* plot in the spec, including ones nested in a
#' [vg_vconcat()]/[vg_hconcat()]), from a single plot's own attributes (set
#' via arguments to [vg_plot()], possibly accumulated from several
#' marks/interactors -- see [vg_plot()] for that merge policy), and from a
#' single mark's encodings.
#'
#' A name that isn't one of mosaic's own plot attributes triggers a warning,
#' since it won't do anything to the rendered plot (there's nowhere else it
#' could still take effect) -- e.g. `title`, which belongs in [vg_meta()]
#' instead.
#'
#' @param spec A `vgspec`.
#' @param ... Named top-level attributes, e.g. `width =`, `height =`.
#' @export
vg_attributes <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  new <- list(...)
  warn_unknown_attrs(names(new), "vg_attributes()")
  spec$attrs <- merge_attrs(spec$attrs, new, context = "vg_attributes()")
  spec
}

#' Set mosaic-spec `plotDefaults` on a vgspec
#'
#' Sets mosaic-spec's top-level `plotDefaults`: attributes applied to *every*
#' plot in the spec, including ones nested inside a
#' [vg_vconcat()]/[vg_hconcat()] layout -- not just the spec's own top level
#' (see [vg_attributes()] for that). This is distinct from a single plot's
#' own attributes (set via arguments to [vg_plot()], possibly accumulated
#' from several marks/interactors -- see [vg_plot()] for that merge policy)
#' and from a single mark's encodings. If per-plot or per-mark
#' attribute-setting is needed later, that should get its own function
#' rather than overloading this one.
#'
#' A name that isn't one of mosaic's own plot attributes triggers a warning,
#' since it won't do anything to the rendered plot -- see [vg_attributes()].
#'
#' @param spec A `vgspec`.
#' @param ... Named plot-default attributes, e.g. `width =`, `height =`.
#' @export
vg_plot_defaults <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  new <- list(...)
  warn_unknown_attrs(names(new), "vg_plot_defaults()")
  spec$plot_defaults <- merge_attrs(spec$plot_defaults, new, context = "vg_plot_defaults()")
  spec
}

#' @export
print.vgspec <- function(x, ...) {
  cat("<vgspec>\n")
  if (length(x$meta)) cat("  meta:", paste(names(x$meta), collapse = ", "), "\n")
  if (length(x$data)) cat("  data:", paste(names(x$data), collapse = ", "), "\n")
  if (length(x$params)) cat("  params:", paste(names(x$params), collapse = ", "), "\n")
  if (length(x$attrs)) {
    str_at <- vapply(x$attrs, deparse_short, character(1))
    cat("  attrs:", paste0(names(x$attrs), " = ", str_at, collapse = ", "), "\n")
  }
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
