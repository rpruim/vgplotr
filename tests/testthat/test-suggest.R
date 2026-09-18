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

test_that("suggest_names() doesn't treat color as a mistake where `color` is a real property", {
  pool <- c("color", "fill", "stroke")
  # (axis marks have `color`) -- so `colour` is just a near-miss of it
  expect_equal(suggest_names("colour", pool), "color")
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
