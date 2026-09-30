# How a mark's channel values are sent to Mosaic, where a formula and a
# string would otherwise look the same.
#
# vgplotr's rule: a formula names data (`symbol = ~species` is the column
# `species`), a string is a constant (`symbol = "square"`). Both would reach
# Mosaic as the JSON string "species"/"square", and Mosaic decides for itself
# what a bare string means: a constant if it's a color name (fill/stroke), a
# symbol name (symbol) or the channel only takes constants (curve, fontSize,
# ...), a column otherwise (`process()` in mosaic-plot's src/marks/Mark.js).
# So a formula naming a column called, say, `square` would silently draw
# squares. serialize_channel_value() sends such a column as `{column: square}`
# instead, which Mosaic always reads as a column; other column names are left
# as plain strings, so specs stay the same as Mosaic's own.
#
# A formula with a literal, `~"b"` or `~5`, is a constant that goes through
# the channel's scale -- drawn with the symbol/color the scale gives "b", or
# at the radius the scale gives 5 -- unlike the plain "b" or 5, a constant
# used as is. Mosaic has no direct way to say that, but a SQL literal
# (`{sql: "'b'"}`, `{sql: "5"}`) works: Mosaic treats it like a column whose
# every value is that constant, and scales it.

# Mosaic's constant-only channel options (mosaic-plot's
# src/marks/util/is-constant-option.js, 0.31.0).
.mosaic_constant_options <- c(
  "offset", "order", "reverse", "sort", "label", "anchor", "curve", "tension",
  "marker", "markerStart", "markerMid", "markerEnd", "textAnchor",
  "lineAnchor", "lineHeight", "textOverflow", "monospace", "fontFamily",
  "fontSize", "fontStyle", "fontVariant", "fontWeight", "frameAnchor",
  "strokeLinejoin", "strokeLinecap", "strokeMiterlimit", "strokeDasharray",
  "strokeDashoffset", "mixBlendMode", "shapeRendering", "imageRendering",
  "preserveAspectRatio", "interpolate", "crossOrigin", "paintOrder",
  "pointerEvents", "target", "select"
)

# Symbol names Mosaic reads as constants (src/marks/util/is-symbol.js).
.mosaic_symbols <- c(
  "asterisk", "circle", "cross", "diamond", "diamond2", "hexagon", "plus",
  "square", "square2", "star", "times", "triangle", "triangle2", "wye"
)

# CSS named colors, as d3-color knows them (d3-color's src/color.js), which
# Mosaic's isColor() relies on.
.css_color_names <- c(
  "aliceblue", "antiquewhite", "aqua", "aquamarine", "azure", "beige",
  "bisque", "black", "blanchedalmond", "blue", "blueviolet", "brown",
  "burlywood", "cadetblue", "chartreuse", "chocolate", "coral",
  "cornflowerblue", "cornsilk", "crimson", "cyan", "darkblue", "darkcyan",
  "darkgoldenrod", "darkgray", "darkgreen", "darkgrey", "darkkhaki",
  "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred",
  "darksalmon", "darkseagreen", "darkslateblue", "darkslategray",
  "darkslategrey", "darkturquoise", "darkviolet", "deeppink",
  "deepskyblue", "dimgray", "dimgrey", "dodgerblue", "firebrick",
  "floralwhite", "forestgreen", "fuchsia", "gainsboro", "ghostwhite",
  "gold", "goldenrod", "gray", "green", "greenyellow", "grey", "honeydew",
  "hotpink", "indianred", "indigo", "ivory", "khaki", "lavender",
  "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral",
  "lightcyan", "lightgoldenrodyellow", "lightgray", "lightgreen",
  "lightgrey", "lightpink", "lightsalmon", "lightseagreen", "lightskyblue",
  "lightslategray", "lightslategrey", "lightsteelblue", "lightyellow",
  "lime", "limegreen", "linen", "magenta", "maroon", "mediumaquamarine",
  "mediumblue", "mediumorchid", "mediumpurple", "mediumseagreen",
  "mediumslateblue", "mediumspringgreen", "mediumturquoise",
  "mediumvioletred", "midnightblue", "mintcream", "mistyrose", "moccasin",
  "navajowhite", "navy", "oldlace", "olive", "olivedrab", "orange",
  "orangered", "orchid", "palegoldenrod", "palegreen", "paleturquoise",
  "palevioletred", "papayawhip", "peachpuff", "peru", "pink", "plum",
  "powderblue", "purple", "rebeccapurple", "red", "rosybrown", "royalblue",
  "saddlebrown", "salmon", "sandybrown", "seagreen", "seashell", "sienna",
  "silver", "skyblue", "slateblue", "slategray", "slategrey", "snow",
  "springgreen", "steelblue", "tan", "teal", "thistle", "tomato",
  "turquoise", "violet", "wheat", "white", "whitesmoke", "yellow",
  "yellowgreen"
)

# Whether Mosaic would read `value` as a color constant: its isColor()
# (src/marks/util/is-color.js) accepts "none", "currentColor", url(...),
# var(...) and anything d3.color() parses -- a named color, "transparent", a
# hex code or an rgb()/hsl() function. The function forms are matched loosely
# here: calling a column name a color only costs a longer (but still correct)
# `{column: ...}` in the spec, never a wrong one.
mosaic_is_color <- function(value) {
  v <- tolower(trimws(value))
  v %in% c("none", "currentcolor", "transparent", .css_color_names) ||
    grepl("^#([0-9a-f]{3}|[0-9a-f]{4}|[0-9a-f]{6}|[0-9a-f]{8})$", v) ||
    grepl(r"(^(url|var|rgba?|hsla?)\(.*\)$)", v)
}

# Whether Mosaic would read the string `value`, given for the mark option
# `channel` (mosaic's exact key, e.g. "fill", "strokeLinecap"), as a constant
# rather than a column name -- the test in mosaic-plot's Mark.js process().
mosaic_reads_as_constant <- function(channel, value) {
  channel %in% .mosaic_constant_options ||
    (channel %in% c("fill", "stroke") && mosaic_is_color(value)) ||
    (channel == "symbol" && tolower(value) %in% .mosaic_symbols)
}

# The constant a formula's right-hand side holds -- a single string, number or
# TRUE/FALSE, or a negated number (`~-2.5` is a call to unary minus, not a
# number) -- or NULL if it isn't one.
formula_literal <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], as.name("-")) && length(expr) == 2) {
    inner <- formula_literal(expr[[2]])
    return(if (is.numeric(inner)) -inner)
  }
  if ((is.character(expr) || is.numeric(expr) || is.logical(expr)) &&
      length(expr) == 1 && !is.na(expr)) {
    expr
  }
}

# A single string, number or TRUE/FALSE as a SQL literal: 'it''s', 5, TRUE.
sql_literal <- function(x) {
  if (is.character(x)) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  else if (is.logical(x)) if (x) "TRUE" else "FALSE"
  else format(x, digits = 15, scientific = FALSE, trim = TRUE)
}

# Serializes one mark option, `channel` being its mosaic key. A one-sided
# formula whose right-hand side is a bare column name that Mosaic would read
# as a constant becomes `{column: name}`, and one whose right-hand side is a
# literal string, number or TRUE/FALSE becomes a SQL literal (see the top of
# this file); everything else is serialized as usual.
serialize_channel_value <- function(channel, x) {
  if (inherits(x, "formula") && length(x) == 2) {
    rhs <- x[[2]]
    literal <- formula_literal(rhs)
    if (!is.null(literal)) {
      return(list(sql = sql_literal(literal)))
    }
    if (is.symbol(rhs)) {
      # A symbol can also be a variable holding a param() -- serialized as
      # "$name", which Mosaic resolves before any of this applies.
      val <- tryCatch(eval(rhs, envir = environment(x)), error = function(e) NULL)
      name <- as.character(rhs)
      if (!is_vg_param(val) && mosaic_reads_as_constant(channel, name)) {
        return(list(column = name))
      }
    }
  }
  serialize_value(x)
}
