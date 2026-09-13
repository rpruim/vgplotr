#' @include utils.R
NULL

#' Set position scale properties (x or y)
#'
#' `vg_scale_x()`/`vg_scale_y()` set the scale properties mosaic-spec exposes
#' per positional axis (`xScale`, `xDomain`, ... -- substitute `y` for the
#' vertical axis). These are already plot-level attributes that
#' [vg_plot()]/[vg_attributes()] accept directly by their raw camelCase
#' names; this is a discoverable, snake_case-argument convenience layer on
#' top of that, following the same real-named-argument approach as the mark
#' constructors (see [vg_mark()]). `vg_scale_x()` and `vg_scale_y()` are thin
#' wrappers around the generic `vg_scale_position()`.
#'
#' Like [vg_plot()], this can be piped in alongside marks/interactors -- it
#' only ever sets attributes on the current plot fragment, so it never needs
#' to come last in a chain. For the analogous facet scales (`fx`/`fy`), see
#' [vg_scale_facet()].
#'
#' @param spec A plot fragment or `vgspec` to set this scale on, or `NULL` to
#'   start a new plot fragment with just these attributes.
#' @param which Which position scale this sets: `"x"` or `"y"`.
#' @param type Scale type, e.g. `"linear"`, `"log"`, `"pow"`, `"sqrt"`,
#'   `"symlog"`, `"utc"`, `"time"`, `"band"`, `"point"`, `"ordinal"`
#'   (`xScale`/`yScale`).
#' @param domain Data domain extent `c(min, max)`, or `"fixed"` to preserve it
#'   across updates (`xDomain`/`yDomain`).
#' @param range Output range in pixels `c(min, max)` (`xRange`/`yRange`).
#' @param nice Boolean; round the scale domain to human-friendly values
#'   (`xNice`/`yNice`).
#' @param zero Boolean; force the domain to include zero (`xZero`/`yZero`).
#' @param reverse Boolean; reverse the scale direction (`xReverse`/`yReverse`).
#' @param clamp Boolean; clamp out-of-domain values to the range
#'   (`xClamp`/`yClamp`).
#' @param round Boolean; round output values to the nearest pixel
#'   (`xRound`/`yRound`).
#' @param padding Inner/outer padding fraction for band scales, 0 to 1
#'   (`xPadding`/`yPadding`).
#' @param padding_inner Inner padding between bands, 0 to 1
#'   (`xPaddingInner`/`yPaddingInner`).
#' @param padding_outer Outer padding at the scale's edges, 0 to 1
#'   (`xPaddingOuter`/`yPaddingOuter`).
#' @param align Alignment of ordinal bands, 0 to 1 (`xAlign`/`yAlign`).
#' @param inset Pixel inset applied to both ends of the range
#'   (`xInset`/`yInset`).
#' @param inset_left,inset_right Pixel inset at the left/right end of the
#'   range; only meaningful for `which = "x"` (`xInsetLeft`/`xInsetRight`).
#' @param inset_top,inset_bottom Pixel inset at the top/bottom end of the
#'   range; only meaningful for `which = "y"` (`yInsetTop`/`yInsetBottom`).
#' @param base Log/pow scale base (`xBase`/`yBase`).
#' @param exponent Pow/symlog scale exponent (`xExponent`/`yExponent`).
#' @param constant Symlog scale constant (`xConstant`/`yConstant`).
#' @param percent Boolean; format/scale values as percentages
#'   (`xPercent`/`yPercent`).
#' @param ... Additional plot-level attributes not covered above, by their
#'   raw mosaic-spec camelCase name (e.g. `xyDomain =`).
#' @family scale functions
#' @export
#' @examples
#' vg_mark_dot(x = ~a, y = ~b) |>
#'   vg_scale_x(type = "log") |>
#'   vg_scale_y(zero = TRUE, nice = TRUE)
vg_scale_position <- function(spec = NULL,
                               which = c("x", "y"),
                               type = vg_unset,
                               domain = vg_unset,
                               range = vg_unset,
                               nice = vg_unset,
                               zero = vg_unset,
                               reverse = vg_unset,
                               clamp = vg_unset,
                               round = vg_unset,
                               padding = vg_unset,
                               padding_inner = vg_unset,
                               padding_outer = vg_unset,
                               align = vg_unset,
                               inset = vg_unset,
                               inset_left = vg_unset,
                               inset_right = vg_unset,
                               inset_top = vg_unset,
                               inset_bottom = vg_unset,
                               base = vg_unset,
                               exponent = vg_unset,
                               constant = vg_unset,
                               percent = vg_unset,
                               ...) {
  which <- match.arg(which)
  context <- paste0("vg_scale_", which, "()")

  suffixes <- c(
    type = "Scale", domain = "Domain", range = "Range", nice = "Nice",
    zero = "Zero", reverse = "Reverse", clamp = "Clamp", round = "Round",
    padding = "Padding", padding_inner = "PaddingInner",
    padding_outer = "PaddingOuter", align = "Align", inset = "Inset",
    base = "Base", exponent = "Exponent", constant = "Constant",
    percent = "Percent"
  )
  attrs <- prefixed_attrs(which, suffixes, environment())
  attrs <- add_inset_attrs(attrs, which, environment(), context)

  apply_plot_attrs(spec, attrs, list(...), context)
}

#' @rdname vg_scale_position
#' @export
vg_scale_x <- wrapper_function(vg_scale_position, which = "x")

#' @rdname vg_scale_position
#' @export
vg_scale_y <- wrapper_function(vg_scale_position, which = "y")

# Facet scales (fx/fy) are a strict subset of the position scale properties
# above: no `type`/`nice`/`zero`/`clamp` (facet scales are always band
# scales) and none of the log/pow/symlog-only properties (`base`,
# `exponent`, `constant`, `percent`).
vg_position_counterpart <- c(x = "y", y = "x", fx = "fy", fy = "fx")

vg_inset_side_suffixes <- list(
  x  = c(inset_left = "InsetLeft",  inset_right  = "InsetRight"),
  fx = c(inset_left = "InsetLeft",  inset_right  = "InsetRight"),
  y  = c(inset_top  = "InsetTop",   inset_bottom = "InsetBottom"),
  fy = c(inset_top  = "InsetTop",   inset_bottom = "InsetBottom")
)

#' Add `xInsetLeft`/`xInsetRight` (or `yInsetTop`/`yInsetBottom`, or the
#' `fx`/`fy` equivalents) to `attrs`, and warn if the caller supplied the
#' other, inapplicable pair instead -- mosaic only defines left/right insets
#' for `x`/`fx` and top/bottom insets for `y`/`fy`.
#' @noRd
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

#' Set facet scale properties (fx or fy)
#'
#' `vg_scale_fx()`/`vg_scale_fy()` set the scale properties mosaic-spec
#' exposes per facet axis (`fxDomain`, `fxPadding`, ... -- substitute `fy`
#' for the row facet axis). Facet scales are always band scales, so this is
#' a strict subset of [vg_scale_position()]'s properties: no `type`, and
#' none of the properties specific to continuous/log/pow scales (`nice`,
#' `zero`, `clamp`, `base`, `exponent`, `constant`, `percent`).
#' `vg_scale_fx()` and `vg_scale_fy()` are thin wrappers around the generic
#' `vg_scale_facet()`.
#'
#' @inheritParams vg_scale_position
#' @param which Which facet scale this sets: `"fx"` or `"fy"`.
#' @param domain Array of facet group categories (`fxDomain`/`fyDomain`).
#' @param range Pixel range for the overall faceting space
#'   (`fxRange`/`fyRange`).
#' @param reverse Boolean; reverse the facet order (`fxReverse`/`fyReverse`).
#' @param round Boolean; round output values to the nearest pixel
#'   (`fxRound`/`fyRound`).
#' @param padding Outer padding between facet subplots, 0 to 1
#'   (`fxPadding`/`fyPadding`).
#' @param padding_inner Inner padding between facet subplots, 0 to 1
#'   (`fxPaddingInner`/`fyPaddingInner`).
#' @param padding_outer Outer padding at the facet space's edges, 0 to 1
#'   (`fxPaddingOuter`/`fyPaddingOuter`).
#' @param align Alignment of facet bands, 0 to 1 (`fxAlign`/`fyAlign`).
#' @param inset Pixel inset applied to both ends of the facet range
#'   (`fxInset`/`fyInset`).
#' @param inset_left,inset_right Pixel inset at the left/right end of the
#'   facet range; only meaningful for `which = "fx"`
#'   (`fxInsetLeft`/`fxInsetRight`).
#' @param inset_top,inset_bottom Pixel inset at the top/bottom end of the
#'   facet range; only meaningful for `which = "fy"`
#'   (`fyInsetTop`/`fyInsetBottom`).
#' @param ... Additional plot-level attributes not covered above, by their
#'   raw mosaic-spec camelCase name.
#' @family scale functions
#' @export
#' @examples
#' vg_mark_dot(x = ~a, y = ~b, fx = ~g) |>
#'   vg_scale_fx(padding = 0.1)
vg_scale_facet <- function(spec = NULL,
                            which = c("fx", "fy"),
                            domain = vg_unset,
                            range = vg_unset,
                            reverse = vg_unset,
                            round = vg_unset,
                            padding = vg_unset,
                            padding_inner = vg_unset,
                            padding_outer = vg_unset,
                            align = vg_unset,
                            inset = vg_unset,
                            inset_left = vg_unset,
                            inset_right = vg_unset,
                            inset_top = vg_unset,
                            inset_bottom = vg_unset,
                            ...) {
  which <- match.arg(which)
  context <- paste0("vg_scale_", which, "()")

  suffixes <- c(
    domain = "Domain", range = "Range", reverse = "Reverse", round = "Round",
    padding = "Padding", padding_inner = "PaddingInner",
    padding_outer = "PaddingOuter", align = "Align", inset = "Inset"
  )
  attrs <- prefixed_attrs(which, suffixes, environment())
  attrs <- add_inset_attrs(attrs, which, environment(), context)

  apply_plot_attrs(spec, attrs, list(...), context)
}

#' @rdname vg_scale_facet
#' @export
vg_scale_fx <- wrapper_function(vg_scale_facet, which = "fx")

#' @rdname vg_scale_facet
#' @export
vg_scale_fy <- wrapper_function(vg_scale_facet, which = "fy")

#' Set the color scale's properties
#'
#' `vg_scale_color()` sets the scale properties mosaic-spec exposes for the
#' `color` channel (`colorScale`, `colorDomain`, ... -- the scale that
#' `fill =`/`stroke =` encodings are bound to unless they're a literal
#' constant). These are already plot-level attributes that
#' [vg_plot()]/[vg_attributes()] accept directly by their raw camelCase
#' names; this is a discoverable, snake_case-argument convenience layer on
#' top of that. Unlike `vg_scale_x()`/`vg_scale_y()`, there's only one
#' color channel, so `vg_scale_color()` isn't built from a `which =`
#' generic -- it's the whole implementation. For the axis-guide properties
#' (`colorLabel`/`colorTickFormat`), see [vg_guide_color()]; for an actual
#' rendered color legend, see [vg_legend_color()] -- a different (if
#' related) thing, a standalone/embedded legend mark rather than a plot
#' attribute.
#'
#' Like [vg_plot()], this can be piped in alongside marks/interactors -- it
#' only ever sets attributes on the current plot fragment, so it never
#' needs to come last in a chain.
#'
#' @param spec A plot fragment or `vgspec` to set this scale on, or `NULL`
#'   to start a new plot fragment with just these attributes.
#' @param type Scale type, e.g. `"linear"`, `"log"`, `"pow"`, `"sqrt"`,
#'   `"symlog"`, `"ordinal"`, `"categorical"`, `"threshold"`, `"quantile"`,
#'   `"quantize"` (`colorScale`).
#' @param domain Data domain extent `c(min, max)`, or an array of
#'   categories for an ordinal/categorical scale (`colorDomain`).
#' @param range Explicit array of output colors (`colorRange`).
#' @param scheme Named color palette, e.g. `"Viridis"`, `"Blues"`,
#'   `"YlOrRd"`, `"Tableau10"` (`colorScheme`).
#' @param interpolate Custom interpolation function/method for a continuous
#'   scale (`colorInterpolate`).
#' @param pivot Midpoint value for a diverging color scheme
#'   (`colorPivot`).
#' @param symmetric Boolean; force a continuous domain to be symmetric
#'   around `pivot` (`colorSymmetric`).
#' @param nice Boolean; round the scale domain to human-friendly values
#'   (`colorNice`).
#' @param zero Boolean; force the domain to include zero (`colorZero`).
#' @param reverse Boolean; reverse the scale/palette order
#'   (`colorReverse`).
#' @param clamp Boolean; clamp out-of-domain values to the range
#'   (`colorClamp`).
#' @param base Log/pow scale base (`colorBase`).
#' @param exponent Pow/symlog scale exponent (`colorExponent`).
#' @param constant Symlog scale constant (`colorConstant`).
#' @param percent Boolean; format/scale values as percentages
#'   (`colorPercent`).
#' @param n Number of quantiles/buckets for a `"quantile"`/`"quantize"`
#'   scale (`colorN`).
#' @param ... Additional plot-level attributes not covered above, by their
#'   raw mosaic-spec camelCase name.
#' @family scale functions
#' @export
#' @examples
#' vg_mark_dot(x = ~a, y = ~b, fill = ~g) |>
#'   vg_scale_color(scheme = "Viridis", type = "linear")
vg_scale_color <- function(spec = NULL,
                            type = vg_unset,
                            domain = vg_unset,
                            range = vg_unset,
                            scheme = vg_unset,
                            interpolate = vg_unset,
                            pivot = vg_unset,
                            symmetric = vg_unset,
                            nice = vg_unset,
                            zero = vg_unset,
                            reverse = vg_unset,
                            clamp = vg_unset,
                            base = vg_unset,
                            exponent = vg_unset,
                            constant = vg_unset,
                            percent = vg_unset,
                            n = vg_unset,
                            ...) {
  context <- "vg_scale_color()"

  suffixes <- c(
    type = "Scale", domain = "Domain", range = "Range", scheme = "Scheme",
    interpolate = "Interpolate", pivot = "Pivot", symmetric = "Symmetric",
    nice = "Nice", zero = "Zero", reverse = "Reverse", clamp = "Clamp",
    base = "Base", exponent = "Exponent", constant = "Constant",
    percent = "Percent", n = "N"
  )
  attrs <- prefixed_attrs("color", suffixes, environment())

  apply_plot_attrs(spec, attrs, list(...), context)
}

#' Set the opacity scale's properties
#'
#' `vg_scale_opacity()` sets the scale properties mosaic-spec exposes for
#' the `opacity` channel (`opacityScale`, `opacityDomain`, ... -- the scale
#' that `opacity =`/`fillOpacity =`/`strokeOpacity =` encodings are bound
#' to unless they're a literal constant). These are already plot-level
#' attributes that [vg_plot()]/[vg_attributes()] accept directly by their
#' raw camelCase names; this is a discoverable, snake_case-argument
#' convenience layer on top of that. For the axis-guide properties
#' (`opacityLabel`/`opacityTickFormat`), see [vg_guide_opacity()]; for an
#' actual rendered opacity legend, see [vg_legend_opacity()].
#'
#' Like [vg_plot()], this can be piped in alongside marks/interactors -- it
#' only ever sets attributes on the current plot fragment, so it never
#' needs to come last in a chain.
#'
#' @param spec A plot fragment or `vgspec` to set this scale on, or `NULL`
#'   to start a new plot fragment with just these attributes.
#' @param type Scale type, e.g. `"linear"`, `"sqrt"`, `"pow"`, `"log"`
#'   (`opacityScale`).
#' @param domain Data domain extent `c(min, max)` (`opacityDomain`).
#' @param range Output opacity range, e.g. `c(0, 1)` (`opacityRange`).
#' @param nice Boolean; round the scale domain to human-friendly values
#'   (`opacityNice`).
#' @param zero Boolean; force the domain to include zero (`opacityZero`).
#' @param reverse Boolean; reverse the scale direction (`opacityReverse`).
#' @param clamp Boolean; clamp out-of-domain values to the range
#'   (`opacityClamp`).
#' @param base Log/pow scale base (`opacityBase`).
#' @param exponent Pow/symlog scale exponent (`opacityExponent`).
#' @param constant Symlog scale constant (`opacityConstant`).
#' @param percent Boolean; format/scale values as percentages
#'   (`opacityPercent`).
#' @param ... Additional plot-level attributes not covered above, by their
#'   raw mosaic-spec camelCase name.
#' @family scale functions
#' @export
#' @examples
#' vg_mark_dot(x = ~a, y = ~b, opacity = ~g) |>
#'   vg_scale_opacity(range = c(0.2, 1))
vg_scale_opacity <- function(spec = NULL,
                              type = vg_unset,
                              domain = vg_unset,
                              range = vg_unset,
                              nice = vg_unset,
                              zero = vg_unset,
                              reverse = vg_unset,
                              clamp = vg_unset,
                              base = vg_unset,
                              exponent = vg_unset,
                              constant = vg_unset,
                              percent = vg_unset,
                              ...) {
  context <- "vg_scale_opacity()"

  suffixes <- c(
    type = "Scale", domain = "Domain", range = "Range", nice = "Nice",
    zero = "Zero", reverse = "Reverse", clamp = "Clamp", base = "Base",
    exponent = "Exponent", constant = "Constant", percent = "Percent"
  )
  attrs <- prefixed_attrs("opacity", suffixes, environment())

  apply_plot_attrs(spec, attrs, list(...), context)
}

#' Set the radius scale's properties
#'
#' `vg_scale_r()` (aliased as `vg_scale_radius()`) sets the scale
#' properties mosaic-spec exposes for the `r` channel (`rScale`,
#' `rDomain`, ... -- the scale that a `dot`/`circle` mark's `r` encoding is
#' bound to unless it's a literal constant). These are already
#' plot-level attributes that [vg_plot()]/[vg_attributes()] accept
#' directly by their raw camelCase names; this is a discoverable,
#' snake_case-argument convenience layer on top of that. Unlike the color
#' and opacity scales, mosaic doesn't define an `rReverse` property, so
#' there's no `reverse` argument here. For the axis-guide property
#' (`rLabel`; there's no `rTickFormat`), see [vg_guide_r()]/
#' [vg_guide_radius()]; for an actual rendered radius/size legend, see
#' [vg_legend_symbol()] (mosaic doesn't have a dedicated `r`-typed legend).
#'
#' Like [vg_plot()], this can be piped in alongside marks/interactors -- it
#' only ever sets attributes on the current plot fragment, so it never
#' needs to come last in a chain.
#'
#' @param spec A plot fragment or `vgspec` to set this scale on, or `NULL`
#'   to start a new plot fragment with just these attributes.
#' @param type Scale type, e.g. `"sqrt"`, `"linear"`, `"pow"`, `"log"`
#'   (`rScale`).
#' @param domain Data domain extent `c(min, max)` (`rDomain`).
#' @param range Output radius range in pixels, e.g. `c(0, 20)` (`rRange`).
#' @param nice Boolean; round the scale domain to human-friendly values
#'   (`rNice`).
#' @param zero Boolean; force the domain to start at 0 (`rZero`).
#' @param clamp Boolean; clamp out-of-domain values to the range
#'   (`rClamp`).
#' @param base Log/pow scale base (`rBase`).
#' @param exponent Pow/symlog scale exponent (`rExponent`).
#' @param constant Symlog scale constant (`rConstant`).
#' @param percent Boolean; format/scale values as percentages
#'   (`rPercent`).
#' @param ... Additional plot-level attributes not covered above, by their
#'   raw mosaic-spec camelCase name.
#' @family scale functions
#' @export
#' @examples
#' vg_mark_dot(x = ~a, y = ~b, r = ~g) |>
#'   vg_scale_r(range = c(0, 20), zero = TRUE)
vg_scale_r <- function(spec = NULL,
                       type = vg_unset,
                       domain = vg_unset,
                       range = vg_unset,
                       nice = vg_unset,
                       zero = vg_unset,
                       clamp = vg_unset,
                       base = vg_unset,
                       exponent = vg_unset,
                       constant = vg_unset,
                       percent = vg_unset,
                       ...) {
  context <- "vg_scale_r()"

  suffixes <- c(
    type = "Scale", domain = "Domain", range = "Range", nice = "Nice",
    zero = "Zero", clamp = "Clamp", base = "Base", exponent = "Exponent",
    constant = "Constant", percent = "Percent"
  )
  attrs <- prefixed_attrs("r", suffixes, environment())

  apply_plot_attrs(spec, attrs, list(...), context)
}

#' @rdname vg_scale_r
#' @export
vg_scale_radius <- vg_scale_r

#' Set global default scale properties (all ordinal position scales)
#'
#' `vg_scale_all()` sets mosaic-spec's plot-wide fallback defaults for
#' `align`/`inset`/`padding` -- unlike `xAlign`/`xPadding`/etc. (set via
#' [vg_scale_position()]) or `fxAlign`/`fxPadding`/etc. (via
#' [vg_scale_facet()]), which only affect one scale, these bare attributes
#' (`align`, `inset`, `padding`) are mosaic's own defaults applied to
#' *every* ordinal position scale (`x`, `y`, `fx`, `fy`) that doesn't set
#' its own value. They're already plot-level attributes that
#' [vg_plot()]/[vg_attributes()] accept directly by their raw camelCase
#' names; this is just a discoverable, named-argument alias for them (no
#' snake_case translation needed here, since these names have no camelCase
#' compound to convert).
#'
#' Like [vg_plot()], this can be piped in alongside marks/interactors -- it
#' only ever sets attributes on the current plot fragment, so it never
#' needs to come last in a chain. For the analogous axis-guide defaults
#' (`axis`, `grid`, `ariaLabel`, `ariaDescription`), see [vg_guide_all()].
#'
#' @param spec A plot fragment or `vgspec` to set these on, or `NULL` to
#'   start a new plot fragment with just these attributes.
#' @param align Default alignment of ordinal bands, 0 to 1, for any
#'   position scale that doesn't set its own `xAlign`/`yAlign`/etc.
#'   (`align`).
#' @param inset Default pixel inset applied to both ends of the range, for
#'   any position scale that doesn't set its own `xInset`/`yInset`/etc.
#'   (`inset`).
#' @param padding Default inner/outer padding fraction for band/point
#'   scales, for any position scale that doesn't set its own
#'   `xPadding`/`yPadding`/etc. (`padding`).
#' @param ... Additional plot-level attributes not covered above, by their
#'   raw mosaic-spec camelCase name.
#' @family scale functions
#' @export
#' @examples
#' vg_mark_dot(x = ~a, y = ~b) |>
#'   vg_scale_all(padding = 0.2)
vg_scale_all <- function(spec = NULL, align = vg_unset, inset = vg_unset, padding = vg_unset, ...) {
  context <- "vg_scale_all()"

  attrs <- drop_unset(list(align = align, inset = inset, padding = padding))

  apply_plot_attrs(spec, attrs, list(...), context)
}
