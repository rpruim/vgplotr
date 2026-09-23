#' @include utils.R
NULL

# Builds vg_legend()'s `@param ...` roxygen block via `@eval`, so its
# per-option `<...>` value-type notation (see ?vg_value_types) stays in
# sync with .vg_legend_prop_types (R/attrs-generated.R, schema-derived)
# instead of a hand-copied snapshot that drifts out of date.
legend_options_doc <- function() {
  opts <- paste0("  - `", names(.vg_legend_prop_types), "` ", .vg_legend_prop_types)
  c(
    "@param ... Legend options, e.g., `as = param(brush)`, `label =",
    "  \"Species\"`. Taken under mosaic-spec's own camelCase names, unlike",
    "  the snake_case of marks and interactors. An argument that isn't an",
    "  option of a legend in mosaic-spec warns, since mosaic would silently",
    "  ignore it. Accepted options:",
    opts
  )
}

#' Add a legend
#'
#' A legend can be embedded in a plot (added alongside its marks, picking up
#' a matching encoding automatically) or, given `for_plot`, live as a
#' standalone layout item referencing a plot by name (its `name =` attribute,
#' see [vg_plot()]) -- mirroring mosaic-spec's own two legend forms.
#'
#' @param spec A plot fragment or `vgspec` to embed this legend in, or `NULL`
#'   to start a new plot with just this legend. Not used (and not allowed)
#'   for a standalone legend with `for_plot` set -- combine that with plots
#'   using [vg_vconcat()]/[vg_hconcat()] instead.
#' @param type The legend type: `"color"`, `"opacity"`, or `"symbol"`.
#' @eval legend_options_doc()
#' @param for_plot For a standalone legend: the `name` of the plot it
#'   decorates.
#' @family legend functions
#' @export
vg_legend <- function(spec = NULL, type, ..., for_plot = NULL) {
  warn_unrecognized_legend_args(list(...), type)
  check_transform_calls(list(...), paste0("legend `", type, "`"))
  legend_obj <- structure(
    list(type = type, for_plot = for_plot, options = list(...)),
    class = "vg_legend"
  )

  if (!is.null(for_plot)) {
    if (!is.null(spec)) {
      stop(
        "A legend with `for_plot` is a standalone layout item; it doesn't ",
        "take a spec/plot to extend. Combine it with plots using ",
        "vg_vconcat()/vg_hconcat() instead.",
        call. = FALSE
      )
    }
    legend_obj
  } else {
    fragment <- as_vg_plot_fragment(spec)
    fragment$items <- c(fragment$items, list(legend_obj))
    update_layout(spec, fragment)
  }
}

#' @rdname vg_legend
#' @export
vg_legend_color <- wrapper_function(vg_legend, type = "color")

#' @rdname vg_legend
#' @export
vg_legend_opacity <- wrapper_function(vg_legend, type = "opacity")

#' @rdname vg_legend
#' @export
vg_legend_symbol <- wrapper_function(vg_legend, type = "symbol")

#' @export
print.vg_legend <- function(x, ...) {
  cat("<vg_legend:", x$type, ">")
  if (!is.null(x$for_plot)) cat(" for", x$for_plot)
  cat("\n")
  print_fields(x$options)
  invisible(x)
}
