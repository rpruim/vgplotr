test_that("similar_names() finds the closest valid name for a typo", {
  pool <- c("fill", "stroke", "strokeWidth", "fillOpacity", "width", "label")
  expect_equal(similar_names("strke", pool), "stroke")          # deletion
  expect_equal(similar_names("strokee", pool), "stroke")        # insertion
  expect_equal(similar_names("labal", pool), "label")           # substitution
  expect_equal(similar_names("widht", pool), "width")           # swapped pair counts as ONE edit
})

test_that("similar_names() ignores case, underscores and dots but returns names as spelled", {
  pool <- c("strokeWidth", "fillOpacity")
  expect_equal(similar_names("stroke_width", pool), "strokeWidth")
  expect_equal(similar_names("fill.opacity", pool), "fillOpacity")
  expect_equal(similar_names("STROKEWIDTH", pool), "strokeWidth")
  # ...and still tolerates a typo on top of that
  expect_equal(similar_names("stroke_widht", pool), "strokeWidth")
})

test_that("similar_names() offers every candidate tied for closest, like git", {
  expect_setequal(similar_names("zDomain", c("xDomain", "yDomain", "label")), c("xDomain", "yDomain"))
})

test_that("similar_names() offers only the best match, never a runner-up", {
  # `stroke` is 1 edit away, `strokes2` is 2: only the closer one comes back
  expect_equal(similar_names("strok", c("stroke", "stroked2")), "stroke")
})

test_that("similar_names() offers nothing when no name is close enough", {
  pool <- c("fill", "stroke", "strokeWidth", "opacity", "label")
  expect_equal(similar_names("bogus", pool), character())
  expect_equal(similar_names("linewidth", pool), character())
  expect_equal(similar_names("alpha", pool), character())
})

test_that("similar_names() scales the allowed distance with the name's length", {
  # 1 edit is fine for a short name; 2 edits on a 5-letter name is not...
  expect_equal(similar_names("fil", c("fill")), "fill")
  expect_equal(similar_names("frlx", c("fill")), character())
  # ...but 2 edits on a long name is
  expect_equal(similar_names("stroke_dashofset", c("strokeDashoffset", "strokeDasharray")), "strokeDashoffset")
})

test_that("similar_names() gives up when too many candidates tie (a vague match)", {
  # a lone letter is one edit from all of these -- suggesting any is a guess
  expect_equal(similar_names("q", c("x", "y", "z", "r")), character())
  expect_equal(similar_names("q", c("x", "y", "z")), c("x", "y", "z"))
  expect_equal(similar_names("q", c("x", "y", "z"), max_suggestions = 2), character())
})

test_that("similar_names() copes with an empty pool or empty name", {
  expect_equal(similar_names("x", character()), character())
  expect_equal(similar_names("", c("x", "y")), character())
  expect_equal(similar_names("___", c("x", "y")), character())
})

test_that("suggest_names() sends color/colour (and near-misses) to fill/stroke", {
  pool <- c("fill", "stroke", "fillOpacity", "width")
  expect_equal(suggest_names("color", pool), c("fill", "stroke"))
  expect_equal(suggest_names("colour", pool), c("fill", "stroke"))
  expect_equal(suggest_names("Color", pool), c("fill", "stroke"))
  expect_equal(suggest_names("colr", pool), c("fill", "stroke"))
  expect_equal(suggest_names("colors", pool), c("fill", "stroke"))
})

test_that("where `color` is a real property (the axis marks), `colour` is pointed at it first", {
  pool <- c("color", "fill", "stroke")
  # the synonym table lists `color` first for `colour`, and only what is valid
  # in the call is offered
  expect_equal(suggest_names("colour", pool), c("color", "fill", "stroke"))
})

test_that("suggest_names() never suggests a name that was already supplied", {
  expect_equal(suggest_names("fil", c("fill", "stroke")), "fill")
  expect_equal(suggest_names("fil", c("fill", "stroke"), present = "fill"), character())
})

test_that("format_suggestion() phrases one, two and three alternatives tersely", {
  expect_equal(format_suggestion("a"), "Did you perhaps mean `a`?")
  expect_equal(format_suggestion(c("a", "b")), "Did you perhaps mean `a` or `b`?")
  expect_equal(format_suggestion(c("a", "b", "c")), "Did you perhaps mean `a`, `b` or `c`?")
  expect_null(format_suggestion(character()))
})

test_that("format_suggestion() names the argument when asked to", {
  expect_equal(format_suggestion(c("fill", "stroke"), arg = "color"), "For `color`, did you perhaps mean `fill` or `stroke`?")
})

test_that("a warning suggests a close name, drawing on plot attributes and vgplotr's own arguments too", {
  # a mark property
  expect_warning(vg_mark_dot(x = ~a, y = ~b, strke = "red"), "Did you perhaps mean `stroke`\\?")
  # snake_case + a swapped pair
  expect_warning(vg_mark_dot(x = ~a, y = ~b, stroke_widht = 2), "Did you perhaps mean `strokeWidth`\\?")
  # a plot attribute a mark also accepts
  expect_warning(vg_mark_dot(x = ~a, y = ~b, widht = 300), "Did you perhaps mean `width`\\?")
  # vgplotr's own data-source argument
  expect_warning(vg_mark_dot(x = ~a, y = ~b, data_form = "d"), "Did you perhaps mean `data_from`\\?")
  # an interactor, an input, a legend
  expect_warning(vg_toggle(as = param(sel), chanels = "x"), "Did you perhaps mean `channels`\\?")
  expect_warning(vg_slider(column = "a", lable = "A"), "Did you perhaps mean `label`\\?")
  expect_warning(vg_legend_color(lable = "L"), "Did you perhaps mean `label`\\?")
})

test_that("a warning makes no suggestion when nothing is similar", {
  w <- character()
  withCallingHandlers(
    vg_mark_dot(x = ~a, y = ~b, bogus = 1),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_no_match(w, "perhaps")
})

test_that("color on a thing with no fill/stroke warns without a suggestion, not an unrelated one", {
  w <- character()
  withCallingHandlers(
    vg_interval_x(as = param(sel), color = "red"),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_no_match(w, "perhaps")   # in particular, not the scale attribute `colorN`
})

test_that("with several unrecognized arguments, each suggestion says which argument it's for", {
  w <- character()
  withCallingHandlers(
    vg_mark_dot(x = ~a, y = ~b, color = "red", strke = "blue", bogus = 1),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "For `color`, did you perhaps mean `fill` or `stroke`\\?")
  expect_match(w, "For `strke`, did you perhaps mean `stroke`\\?")
  expect_no_match(w, "For `bogus`")
})

test_that("a warning doesn't suggest a name that's already been supplied", {
  w <- character()
  withCallingHandlers(
    vg_mark(mark = "dot", x = ~a, fill = "red", fil = "blue"),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_no_match(w, "perhaps")
})

# --- scales, guides and the other plot-attribute setters -------------------

test_that("a scale/guide warning suggests the constructor's own argument", {
  expect_warning(vg_scale_x(domian = c(0, 1)), "In vg_scale_x\\(\\): `domian` is not a recognized.*Did you perhaps mean `domain`\\?")
  expect_warning(vg_scale_color(schme = "blues"), "Did you perhaps mean `scheme`\\?")
  expect_warning(vg_guide_x(tick_sise = 5), "Did you perhaps mean `tick_size`\\?")
  expect_warning(vg_guide_color(tick_fromat = "d"), "Did you perhaps mean `tick_format`\\?")
})

test_that("a scale/guide warning also suggests plot attributes, spelled snake_case", {
  expect_warning(vg_scale_x(x_domian = c(0, 1)), "Did you perhaps mean `x_domain`\\?")
  expect_warning(vg_scale_all(colr_scheme = "blues"), "Did you perhaps mean `color_scheme`\\?")
})

test_that("vg_plot()/vg_attributes()/vg_plot_defaults() suggest plot attributes, spelled snake_case", {
  expect_warning(vg_plot(NULL, marginLeftt = 5), "In vg_plot\\(\\).*Did you perhaps mean `margin_left`\\?")
  expect_warning(vg_create() |> vg_attributes(xdomian = c(0, 1)), "In vg_attributes\\(\\).*Did you perhaps mean `x_domain`\\?")
  expect_warning(vg_create() |> vg_plot_defaults(widht = 300), "In vg_plot_defaults\\(\\).*Did you perhaps mean `width`\\?")
})

test_that("a plot-attribute warning makes no suggestion when nothing is similar, and no longer gives stale camelCase advice", {
  w <- character()
  withCallingHandlers(
    vg_scale_x(bogus = 1),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_no_match(w, "perhaps")
  expect_no_match(w, "camelCase")   # snake_case is what these functions document
})

test_that("spec metadata passed as a plot attribute is pointed at vg_meta(), not spell-checked", {
  w <- character()
  withCallingHandlers(
    vg_create() |> vg_attributes(title = "T"),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "`title` is not a recognized mosaic-spec plot attribute")
  expect_match(w, "`title` belongs in `vg_meta\\(\\)`\\.")
  expect_no_match(w, "perhaps")

  # more than one metadata name: plural
  w <- character()
  withCallingHandlers(
    vg_create() |> vg_attributes(title = "T", credit = "C"),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "`title`, `credit` belong in `vg_meta\\(\\)`\\.")
})

test_that("with several unrecognized attributes, each suggestion says which one it's for", {
  w <- character()
  withCallingHandlers(
    vg_create() |> vg_attributes(widht = 3, title = "T", bogus = 1),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "For `widht`, did you perhaps mean `width`\\?")
  expect_no_match(w, "For `bogus`")
  expect_match(w, "`title` belongs in `vg_meta\\(\\)`")
})

test_that("a mark's plot-attribute suggestions are spelled snake_case too", {
  expect_warning(vg_mark_dot(x = ~a, y = ~b, x_domian = c(0, 1)), "Did you perhaps mean `x_domain`\\?")
})

test_that("context_arg_names() finds a constructor's user-facing arguments from its label", {
  args <- context_arg_names("vg_scale_x()")
  expect_true(all(c("domain", "range", "nice", "zero", "type") %in% args))
  expect_false(any(c("spec", "which", "...") %in% args))
  # arguments the x wrapper drops (they're y-only) aren't offered for it
  expect_false("inset_top" %in% args)
  expect_true("inset_left" %in% args)
  expect_equal(context_arg_names("vg_attributes()"), character())
  expect_equal(context_arg_names("not_a_function()"), character())
})

test_that("vg_plot_level_arg_names() is every plot attribute's snake_case spelling", {
  nms <- vg_plot_level_arg_names()
  expect_true(all(c("x_domain", "margin_left", "width") %in% nms))
  expect_false("xDomain" %in% nms)
  expect_length(nms, length(vg_plot_level_args()))
})

# --- transforms ------------------------------------------------------------


test_that("a misspelled option inside a formula names the transform and suggests the right one", {
  expect_error(
    serialize_unchecked(x = ~vg_bin(delay, stp = 10), y = ~vg_count()),
    "In `vg_bin\\(\\)`: `stp` is not an argument of this transform\\. Did you perhaps mean `step`\\?"
  )
  expect_error(serialize_unchecked(x = ~vg_bin(delay, nce = TRUE), y = ~vg_count()), "Did you perhaps mean `nice`\\?")
})

test_that("a misspelled option in a transform nested inside another is caught, too", {
  expect_error(
    serialize_unchecked(x = ~delay, y = ~vg_avg(vg_bin(delay, stpe = 1))),
    "In `vg_bin\\(\\)`: `stpe` is not an argument.*`step`"
  )
})

test_that("several misspelled options each get their own suggestion", {
  expect_error(
    serialize_unchecked(x = ~vg_bin(delay, stp = 10, intrval = "day"), y = ~vg_count()),
    "`stp`, `intrval` are not arguments of this transform\\. For `stp`, did you perhaps mean `step`\\? For `intrval`, did you perhaps mean `interval`\\?"
  )
})

test_that("with nothing similar, the error lists the transform's arguments instead", {
  expect_error(
    serialize_unchecked(x = ~vg_bin(delay, bogus = 10), y = ~vg_count()),
    "`bogus` is not an argument of this transform\\. Its arguments are `field`, `interval`, `step`, `steps`, `minstep`, `nice`, `offset`\\."
  )
})

test_that("an unrecognized transform name is offered the closest known one", {
  expect_error(serialize_unchecked(x = ~vg_bn(delay), y = ~vg_count()), "Got a call to `vg_bn\\(\\)`\\. Did you perhaps mean `vg_bin\\(\\)`\\?")
  expect_error(serialize_unchecked(x = ~vg_bin(delay), y = ~vg_cont()), "Did you perhaps mean `vg_count\\(\\)`\\?")
  # sql()/agg()/param() are accepted in a formula too
  expect_error(serialize_unchecked(x = ~sqll("a"), y = ~vg_count()), "Did you perhaps mean `sql\\(\\)`\\?")
})

test_that("an unrecognized function that resembles no transform gets no suggestion (and keeps the full list)", {
  err <- tryCatch(serialize_unchecked(x = ~sqrt(delay), y = ~vg_count()), error = function(e) conditionMessage(e))
  expect_match(err, "Got a call to `sqrt\\(\\)`\\.$")
  expect_match(err, "known transform functions \\(vg_bin\\(\\)")
  expect_no_match(err, "perhaps")
})

test_that("valid transform calls -- including an unambiguous partial argument name -- are unaffected", {
  expect_no_error(serialize_with(x = ~vg_bin(delay, step = 10), y = ~vg_count()))
  expect_no_error(serialize_with(x = ~vg_bin(delay, inter = "day"), y = ~vg_count()))
  expect_no_error(serialize_with(x = ~delay, y = ~vg_count()))
})

test_that("a transform error that isn't about an unrecognized name keeps R's own message", {
  # too many positional arguments: nothing to suggest
  expect_error(serialize_with(x = ~vg_bin(delay, 1, 2, 3, 4, 5, 6, 7, 8), y = ~vg_count()), "unused argument")
})

test_that("a direct call with a misspelled option is R's own error (which already names the call)", {
  expect_error(vg_bin(delay, stp = 10), "unused argument")
})

test_that("match_transform_call() is match.call() when nothing is wrong", {
  mc <- match_transform_call("vg_bin", vg_bin, quote(vg_bin(delay, step = 10)))
  expect_equal(names(mc), c("", "field", "step"))
})

test_that("transform_name_suggestion() finds the closest transform for a typo", {
  expect_equal(transform_name_suggestion("vg_bn"), " Did you perhaps mean `vg_bin()`?")
  expect_equal(transform_name_suggestion("vg_stdev"), " Did you perhaps mean `vg_stddev()`?")
  expect_equal(transform_name_suggestion("vg_meadian"), " Did you perhaps mean `vg_median()`?")
  expect_equal(transform_name_suggestion("sqll"), " Did you perhaps mean `sql()`?")
})

test_that("a transform written without its vg_ prefix is pointed at the prefixed one", {
  expect_equal(transform_name_suggestion("bin"), " Did you perhaps mean `vg_bin()`?")
  expect_equal(transform_name_suggestion("count"), " Did you perhaps mean `vg_count()`?")
  expect_equal(transform_name_suggestion("median"), " Did you perhaps mean `vg_median()`?")
})

test_that("a forgotten prefix beats a fuzzy match (avg is one edit from agg, but vg_avg is what was meant)", {
  expect_equal(transform_name_suggestion("avg"), " Did you perhaps mean `vg_avg()`?")
})

test_that("a forgotten prefix is only recognised exactly, so base-R functions aren't second-guessed", {
  expect_equal(transform_name_suggestion("log"), "")     # not `vg_lag()`
  expect_equal(transform_name_suggestion("sqrt"), "")
  expect_equal(transform_name_suggestion("paste"), "")
  expect_equal(transform_name_suggestion("average_of"), "")
})

test_that("the shared vg_ prefix doesn't make unrelated transform names look close", {
  # `hxst` is two edits from `last`: too far for a 4-letter name, even though
  # the full names differ by the same two edits out of 8
  expect_equal(transform_name_suggestion("vg_hxst"), "")
  expect_equal(transform_name_suggestion("vg_mxdx"), "")   # two edits from `mode`
  # ...but a typo *in* the prefix still counts as an edit
  expect_equal(transform_name_suggestion("vh_bin"), " Did you perhaps mean `vg_bin()`?")
})

test_that("similar_names() accepts an explicit distance limit", {
  pool <- c("vg_first", "vg_last")
  expect_equal(similar_names("vg_hist", pool), c("vg_first", "vg_last"))   # default rule: 2 edits allowed
  expect_equal(similar_names("vg_hist", pool, limit = 1), character())
  expect_equal(similar_names("vg_hist", pool, limit = 2), c("vg_first", "vg_last"))
})

test_that("an unknown transform in a formula that is really a base-R function gets no misleading suggestion", {
  err <- tryCatch(serialize_unchecked(x = ~log(delay), y = ~vg_count()), error = function(e) conditionMessage(e))
  expect_match(err, "Got a call to `log\\(\\)`\\.$")
  expect_no_match(err, "perhaps")
})

# --- enum values -----------------------------------------------------------

enum_warning <- function(expr) {
  w <- character()
  withCallingHandlers(
    expr,
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  w
}

test_that("an enum value is offered the closest recognized value, ignoring separators and case", {
  expect_warning(warn_unrecognized_enum_values(list(curve = "cardinal_open")), "Did you perhaps mean `\"cardinal-open\"`\\?")
  expect_warning(warn_unrecognized_enum_values(list(curve = "cardinalOpen")), "Did you perhaps mean `\"cardinal-open\"`\\?")
  expect_warning(warn_unrecognized_enum_values(list(curve = "cardinalopen")), "Did you perhaps mean `\"cardinal-open\"`\\?")
  expect_warning(warn_unrecognized_enum_values(list(curve = "Natural")), "Did you perhaps mean `\"natural\"`\\?")
  expect_warning(warn_unrecognized_enum_values(list(curve = "NATURAL")), "Did you perhaps mean `\"natural\"`\\?")
  expect_warning(warn_unrecognized_enum_values(list(frameAnchor = "top_left")), "Did you perhaps mean `\"top-left\"`\\?")
})

test_that("an enum value is offered the closest recognized value for an ordinary typo", {
  expect_warning(warn_unrecognized_enum_values(list(curve = "natral")), "Did you perhaps mean `\"natural\"`\\?")
  expect_warning(warn_unrecognized_enum_values(list(curve = "step-befor")), "Did you perhaps mean `\"step-before\"`\\?")
  expect_warning(warn_unrecognized_enum_values(list(symbol = "circel")), "Did you perhaps mean `\"circle\"`\\?")   # swap
  expect_warning(warn_unrecognized_enum_values(list(facet = "includ")), "Did you perhaps mean `\"include\"`\\?")
})

test_that("with a suggestion the long list of allowed values is left out; without one it's kept", {
  w <- enum_warning(warn_unrecognized_enum_values(list(curve = "cardinal_open")))
  expect_length(w, 1)
  expect_no_match(w, "Allowed values")

  w <- enum_warning(warn_unrecognized_enum_values(list(curve = "smooth")))
  expect_length(w, 1)
  expect_match(w, "Allowed values: \"basis\"")
  expect_no_match(w, "perhaps")
})

test_that("a tie between recognized values offers every one of them", {
  expect_warning(warn_unrecognized_enum_values(list(tip = "xx")), "Did you perhaps mean `\"x\"` or `\"xy\"`\\?")
  expect_warning(warn_unrecognized_enum_values(list(curve = "monotone")), "`\"monotone-x\"` or `\"monotone-y\"`")
})

test_that("a leading sign on a value is significant, not a separator", {
  # `-value` is a descending order, not a spelling of `value`
  expect_warning(warn_unrecognized_enum_values(list(order = "-valu")), "Did you perhaps mean `\"-value\"`\\?$")
  expect_warning(warn_unrecognized_enum_values(list(order = "valu")), "Did you perhaps mean `\"value\"`\\?$")
})

test_that("similar_names() ignores hyphens and spaces inside a name but not a leading sign", {
  expect_equal(similar_names("cardinal_open", c("cardinal-open", "basis")), "cardinal-open")
  expect_equal(similar_names("3month", c("3 months", "second")), "3 months")
  expect_equal(similar_names("-valu", c("value", "-value")), "-value")
})

test_that("the color -> fill/stroke rule is about argument names and never rewrites a value", {
  w <- enum_warning(warn_unrecognized_enum_values(list(symbol = "color")))
  expect_length(w, 1)
  expect_no_match(w, "fill|stroke")
})

test_that("an enum-value suggestion is made for marks, interactors and inputs alike", {
  expect_warning(vg_mark_line(x = ~a, y = ~b, curve = "cardinal_open"), "Did you perhaps mean `\"cardinal-open\"`\\?")
  expect_warning(vg_mark_dot(x = ~a, y = ~b, frame_anchor = "top_left"), "`frameAnchor = \"top_left\"`.*`\"top-left\"`")
  expect_warning(vg_mark_dot(x = ~a, y = ~b, select = "nearstX"), "Did you perhaps mean `\"nearestX\"`\\?")
  expect_warning(vg_slider(column = "a", select = "pont"), "Did you perhaps mean `\"point\"`\\?")
  expect_warning(vg_menu(column = "a", list_match = "al"), "`listMatch = \"al\"`.*`\"all\"`")
})

test_that("a valid enum value, a formula, or a param() never gets a suggestion", {
  expect_no_warning(vg_mark_line(x = ~a, y = ~b, curve = "cardinal-open"))
  expect_no_warning(vg_mark_line(x = ~a, y = ~b, curve = ~my_curve_col))
  expect_no_warning(vg_mark_line(x = ~a, y = ~b, curve = param("c")))
})
