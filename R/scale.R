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
#' to come last in a chain.
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
#' @param ... Additional plot-level attributes, by their raw mosaic-spec
#'   camelCase name (e.g. `xyDomain =`).
#' @family scale functions
#' @export
#' @examples
#' vg_dot(x = ~a, y = ~b) |>
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

  common_suffixes <- c(
    type = "Scale", domain = "Domain", range = "Range", nice = "Nice",
    zero = "Zero", reverse = "Reverse", clamp = "Clamp", round = "Round",
    padding = "Padding", padding_inner = "PaddingInner",
    padding_outer = "PaddingOuter", align = "Align", inset = "Inset",
    base = "Base", exponent = "Exponent", constant = "Constant",
    percent = "Percent"
  )
  attrs <- drop_unset(mget(names(common_suffixes), environment()))
  if (length(attrs)) names(attrs) <- paste0(which, common_suffixes[names(attrs)])

  # xInsetLeft/xInsetRight and yInsetTop/yInsetBottom aren't symmetric --
  # mosaic only defines left/right insets for x and top/bottom for y.
  side_suffixes <- list(
    x = c(inset_left = "InsetLeft", inset_right = "InsetRight"),
    y = c(inset_top = "InsetTop", inset_bottom = "InsetBottom")
  )
  own_side <- side_suffixes[[which]]
  other_side <- side_suffixes[[setdiff(c("x", "y"), which)]]

  own_vals <- drop_unset(mget(names(own_side), environment()))
  if (length(own_vals)) {
    names(own_vals) <- paste0(which, own_side[names(own_vals)])
    attrs <- c(attrs, own_vals)
  }

  other_vals <- drop_unset(mget(names(other_side), environment()))
  if (length(other_vals)) {
    warning(
      sprintf(
        "In %s: %s only appl%s to the %s scale; use %s for the %s scale.",
        context,
        paste0("`", names(other_vals), "`", collapse = ", "),
        if (length(other_vals) == 1) "ies" else "y",
        setdiff(c("x", "y"), which),
        paste(sprintf("`%s`", names(own_side)), collapse = "/"),
        which
      ),
      call. = FALSE
    )
  }

  extra <- list(...)
  warn_unknown_attrs(names(extra), context)
  attrs <- merge_attrs(attrs, extra, context = context)

  fragment <- as_vg_plot_fragment(spec)
  fragment$attrs <- merge_attrs(fragment$attrs, attrs, context = context)
  update_layout(spec, fragment)
}

#' @rdname vg_scale_position
#' @export
vg_scale_x <- function(spec = NULL, ...) vg_scale_position(spec, which = "x", ...)

#' @rdname vg_scale_position
#' @export
vg_scale_y <- function(spec = NULL, ...) vg_scale_position(spec, which = "y", ...)
