#' Set axis-guide properties for a position scale (x or y)
#'
#' `vg_guide_x()`/`vg_guide_y()` set the axis-guide properties mosaic-spec
#' exposes per positional axis (`xAxis`, `xTicks`, ... -- substitute `y` for
#' the vertical axis). Like the `xScale`/`yScale` properties handled by
#' [vg_scale_position()], these are already plot-level attributes that
#' [vg_plot()]/[vg_attributes()] accept directly by their raw camelCase
#' names; this is a discoverable, snake_case-argument convenience layer on
#' top of that. `vg_guide_x()` and `vg_guide_y()` are thin wrappers around
#' the generic `vg_guide_position()`.
#'
#' These are named `vg_guide_*()` rather than `vg_axis_*()` because
#' `vg_axis_x()`/`vg_axis_y()` are already taken -- mosaic-spec has a real
#' `axisX`/`axisY` *mark* (a drawable, independently-styled axis), generated
#' as those names in `R/marks-generated.R`. `vg_guide_*()` sets the plain
#' `xAxis`/`yAxis` guide attributes that appear automatically alongside a
#' plot's marks, which is a different (if related) thing. Same story for the
#' analogous facet guides, [vg_guide_facet()] (vs. `vg_axis_fx()`/
#' `vg_axis_fy()`, mosaic's `axisFx`/`axisFy` marks).
#'
#' Like [vg_plot()], this can be piped in alongside marks/interactors -- it
#' only ever sets attributes on the current plot fragment, so it never needs
#' to come last in a chain.
#'
#' @param spec A plot fragment or `vgspec` to set this guide on, or `NULL` to
#'   start a new plot fragment with just these attributes.
#' @param which Which axis this sets: `"x"` or `"y"`.
#' @param position Axis position/visibility: `"top"`/`"bottom"` for `x`,
#'   `"left"`/`"right"` for `y`, or `NULL`/`FALSE` to hide it
#'   (`xAxis`/`yAxis`).
#' @param ticks Number of ticks, or an array of explicit tick values
#'   (`xTicks`/`yTicks`).
#' @param tick_spacing Target distance between ticks in pixels
#'   (`xTickSpacing`/`yTickSpacing`).
#' @param tick_size Length of tick marks in pixels (`xTickSize`/`yTickSize`).
#' @param tick_padding Distance between a tick mark and its label in pixels
#'   (`xTickPadding`/`yTickPadding`).
#' @param tick_format Format specifier string (e.g. `"%Y"`, `"d"`, `".2f"`)
#'   or function for tick labels (`xTickFormat`/`yTickFormat`).
#' @param tick_rotate Rotation angle of tick labels in degrees
#'   (`xTickRotate`/`yTickRotate`).
#' @param grid Boolean, or a line stroke count, to render grid lines along
#'   this axis (`xGrid`/`yGrid`).
#' @param line Boolean; whether to draw the axis line itself
#'   (`xLine`/`yLine`).
#' @param label Axis title string (`xLabel`/`yLabel`).
#' @param label_anchor Position anchor for the label text: `"left"`,
#'   `"center"`, or `"right"` (`xLabelAnchor`/`yLabelAnchor`).
#' @param label_offset Distance between the axis and its label in pixels
#'   (`xLabelOffset`/`yLabelOffset`).
#' @param label_arrow Whether/how to draw an arrow on the axis label
#'   (`xLabelArrow`/`yLabelArrow`).
#' @param font_variant CSS font-variant for tick labels, e.g.
#'   `"tabular-nums"` (`xFontVariant`/`yFontVariant`).
#' @param aria_label ARIA accessibility label for the axis
#'   (`xAriaLabel`/`yAriaLabel`).
#' @param aria_description ARIA accessibility description for the axis
#'   (`xAriaDescription`/`yAriaDescription`).
#' @param ... Additional plot-level attributes, by their raw mosaic-spec
#'   camelCase name.
#' @family guide functions
#' @export
#' @examples
#' vg_dot(x = ~a, y = ~b) |>
#'   vg_guide_x(label = "A", grid = TRUE) |>
#'   vg_guide_y(label = "B", tick_format = ".0f")
vg_guide_position <- function(spec = NULL,
                               which = c("x", "y"),
                               position = vg_unset,
                               ticks = vg_unset,
                               tick_spacing = vg_unset,
                               tick_size = vg_unset,
                               tick_padding = vg_unset,
                               tick_format = vg_unset,
                               tick_rotate = vg_unset,
                               grid = vg_unset,
                               line = vg_unset,
                               label = vg_unset,
                               label_anchor = vg_unset,
                               label_offset = vg_unset,
                               label_arrow = vg_unset,
                               font_variant = vg_unset,
                               aria_label = vg_unset,
                               aria_description = vg_unset,
                               ...) {
  which <- match.arg(which)
  context <- paste0("vg_guide_", which, "()")

  suffixes <- c(
    position = "Axis", ticks = "Ticks", tick_spacing = "TickSpacing",
    tick_size = "TickSize", tick_padding = "TickPadding",
    tick_format = "TickFormat", tick_rotate = "TickRotate", grid = "Grid",
    line = "Line", label = "Label", label_anchor = "LabelAnchor",
    label_offset = "LabelOffset", label_arrow = "LabelArrow",
    font_variant = "FontVariant", aria_label = "AriaLabel",
    aria_description = "AriaDescription"
  )
  attrs <- prefixed_attrs(which, suffixes, environment())

  extra <- list(...)
  warn_unknown_attrs(names(extra), context)
  attrs <- merge_attrs(attrs, extra, context = context)

  fragment <- as_vg_plot_fragment(spec)
  fragment$attrs <- merge_attrs(fragment$attrs, attrs, context = context)
  update_layout(spec, fragment)
}

#' @rdname vg_guide_position
#' @export
vg_guide_x <- function(spec = NULL, ...) vg_guide_position(spec, which = "x", ...)

#' @rdname vg_guide_position
#' @export
vg_guide_y <- function(spec = NULL, ...) vg_guide_position(spec, which = "y", ...)

#' Set axis-guide properties for a facet scale (fx or fy)
#'
#' `vg_guide_fx()`/`vg_guide_fy()` set the axis-guide properties mosaic-spec
#' exposes per facet axis (`fxAxis`, `fxTicks`, ... -- substitute `fy` for
#' the row facet axis). This is the facet counterpart of
#' [vg_guide_position()]; it accepts the same properties minus
#' `label_arrow`, which mosaic doesn't define for facet guides.
#' `vg_guide_fx()` and `vg_guide_fy()` are thin wrappers around the generic
#' `vg_guide_facet()`.
#'
#' See [vg_guide_position()] for why these are `vg_guide_*()` rather than
#' `vg_axis_*()` (already taken by mosaic's `axisFx`/`axisFy` marks).
#'
#' @inheritParams vg_guide_position
#' @param which Which facet axis this sets: `"fx"` or `"fy"`.
#' @param position Facet header position: `"top"`/`"bottom"` for `fx`,
#'   `"left"`/`"right"` for `fy`, or `NULL`/`FALSE` to hide it
#'   (`fxAxis`/`fyAxis`).
#' @param ticks Number of ticks, or an array of explicit tick values
#'   (`fxTicks`/`fyTicks`).
#' @param tick_spacing Target distance between ticks in pixels
#'   (`fxTickSpacing`/`fyTickSpacing`).
#' @param tick_size Length of tick marks in pixels
#'   (`fxTickSize`/`fyTickSize`).
#' @param tick_padding Distance between a tick mark and its label in pixels
#'   (`fxTickPadding`/`fyTickPadding`).
#' @param tick_format Format specifier string or function for tick labels
#'   (`fxTickFormat`/`fyTickFormat`).
#' @param tick_rotate Rotation angle of tick labels in degrees
#'   (`fxTickRotate`/`fyTickRotate`).
#' @param grid Boolean, or a line stroke count, to render grid lines along
#'   this facet axis (`fxGrid`/`fyGrid`).
#' @param line Boolean; whether to draw the facet axis line itself
#'   (`fxLine`/`fyLine`).
#' @param label Facet axis title string (`fxLabel`/`fyLabel`).
#' @param label_anchor Position anchor for the label text: `"left"`,
#'   `"center"`, or `"right"` (`fxLabelAnchor`/`fyLabelAnchor`).
#' @param label_offset Distance between the facet axis and its label in
#'   pixels (`fxLabelOffset`/`fyLabelOffset`).
#' @param font_variant CSS font-variant for tick labels
#'   (`fxFontVariant`/`fyFontVariant`).
#' @param aria_label ARIA accessibility label for the facet axis
#'   (`fxAriaLabel`/`fyAriaLabel`).
#' @param aria_description ARIA accessibility description for the facet axis
#'   (`fxAriaDescription`/`fyAriaDescription`).
#' @family guide functions
#' @export
#' @examples
#' vg_dot(x = ~a, y = ~b, fx = ~g) |>
#'   vg_guide_fx(label = "Group")
vg_guide_facet <- function(spec = NULL,
                            which = c("fx", "fy"),
                            position = vg_unset,
                            ticks = vg_unset,
                            tick_spacing = vg_unset,
                            tick_size = vg_unset,
                            tick_padding = vg_unset,
                            tick_format = vg_unset,
                            tick_rotate = vg_unset,
                            grid = vg_unset,
                            line = vg_unset,
                            label = vg_unset,
                            label_anchor = vg_unset,
                            label_offset = vg_unset,
                            font_variant = vg_unset,
                            aria_label = vg_unset,
                            aria_description = vg_unset,
                            ...) {
  which <- match.arg(which)
  context <- paste0("vg_guide_", which, "()")

  suffixes <- c(
    position = "Axis", ticks = "Ticks", tick_spacing = "TickSpacing",
    tick_size = "TickSize", tick_padding = "TickPadding",
    tick_format = "TickFormat", tick_rotate = "TickRotate", grid = "Grid",
    line = "Line", label = "Label", label_anchor = "LabelAnchor",
    label_offset = "LabelOffset", font_variant = "FontVariant",
    aria_label = "AriaLabel", aria_description = "AriaDescription"
  )
  attrs <- prefixed_attrs(which, suffixes, environment())

  extra <- list(...)
  warn_unknown_attrs(names(extra), context)
  attrs <- merge_attrs(attrs, extra, context = context)

  fragment <- as_vg_plot_fragment(spec)
  fragment$attrs <- merge_attrs(fragment$attrs, attrs, context = context)
  update_layout(spec, fragment)
}

#' @rdname vg_guide_facet
#' @export
vg_guide_fx <- function(spec = NULL, ...) vg_guide_facet(spec, which = "fx", ...)

#' @rdname vg_guide_facet
#' @export
vg_guide_fy <- function(spec = NULL, ...) vg_guide_facet(spec, which = "fy", ...)
