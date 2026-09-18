test_that("a mark's `color =` warns and suggests fill/stroke", {
  # mosaic marks have no `color` channel -- only `fill`/`stroke` -- so it's
  # otherwise silently ignored and the mark draws in its default color.
  expect_warning(
    vg_mark_dot(x = ~a, y = ~b, color = ~g),
    "In mark `dot`: `color` is not a property of the `dot` mark.*Did you perhaps mean `fill` or `stroke`\\?"
  )
  expect_warning(vg_mark_dot(x = ~a, y = ~b, colour = "red"), "`colour` is not a property.*Did you perhaps mean `fill` or `stroke`\\?")
})

test_that("`color =` warns when a data frame is piped in, too", {
  df <- data.frame(a = 1:2, b = 3:4, g = c("x", "y"))
  expect_warning(df |> vg_mark_dot(x = ~a, y = ~b, color = ~g), "`color` is not a property")
})

test_that("`color =` is fine on a mark that genuinely has a `color` property", {
  # the axis marks declare `color` (the axis's own tick/label color)
  expect_true("color" %in% .vg_mark_own_props[["axisX"]])
  expect_no_warning(vg_mark_axis_x(color = "red"))
})

test_that("the color suggestion only offers fill/stroke where the mark actually has them", {
  expect_equal(suggest_names("color", c("stroke", "x")), "stroke")
  expect_equal(suggest_names("color", c("fill", "stroke", "x")), c("fill", "stroke"))
  # neither -> nothing to suggest (and no fuzzy match against the rest either)
  expect_equal(suggest_names("color", c("x", "colorN")), character())
})

test_that("an unrecognized argument with no known fix still warns, just without a hint", {
  expect_warning(
    vg_mark_dot(x = ~a, y = ~b, bogus = 1),
    "In mark `dot`: `bogus` is not a property of the `dot` mark in mosaic-spec, so it won't affect the rendered graphic\\.$"
  )
})

test_that("several unrecognized arguments produce a single warning naming all of them", {
  w <- character()
  withCallingHandlers(
    vg_mark_dot(x = ~a, y = ~b, color = ~g, bogus = 1),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "`color`, `bogus` are not properties of the `dot` mark", fixed = FALSE)
})

test_that("the generic vg_mark() takes camelCase; a snake_case name warns and points at the real one", {
  expect_warning(
    vg_mark(mark = "dot", x = ~a, y = ~b, fill_opacity = 0.5),
    "`fill_opacity` is not a property.*Did you perhaps mean `fillOpacity`\\?"
  )
  expect_no_warning(vg_mark(mark = "dot", x = ~a, y = ~b, fillOpacity = 0.5))
  # ...while the wrappers take snake_case
  expect_no_warning(vg_mark_dot(x = ~a, y = ~b, fill_opacity = 0.5))
})

test_that("recognized arguments, plot-level attributes and data arguments never warn", {
  expect_no_warning(vg_mark_dot(x = ~a, y = ~b, fill = "steelblue", r = 3, tip = TRUE))
  # a plot attribute riding along on a mark, snake_case or camelCase
  expect_no_warning(vg_mark_dot(x = ~a, y = ~b, width = 400, x_domain = c(0, 1), marginLeft = 50))
  # vgplotr's own data-source arguments
  expect_no_warning(vg_mark_dot(x = ~a, y = ~b, data_from = "d", filter_by = param("sel"), data_optimize = FALSE))
})

test_that("a mark type with no schema entry is skipped rather than flagged", {
  expect_no_warning(vg_mark(mark = "notARealMark", x = ~a, color = "red"))
})

test_that("the warning doesn't stop the mark from being built", {
  expect_warning(spec <- vg_mark_dot(x = ~a, y = ~b, color = ~g), "color")
  expect_s3_class(spec, "vg_plot_fragment")
  expect_length(spec$items, 1)
})
