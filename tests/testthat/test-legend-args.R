test_that("a legend warns on an option mosaic-spec doesn't list for it", {
  expect_warning(
    vg_legend_color(bogus = 1),
    "In legend `color`: `bogus` is not a property of the `color` legend in mosaic-spec, so it won't have any effect\\.$"
  )
  # the legend type in the message tracks the wrapper used
  expect_warning(vg_legend_opacity(bogus = 1), "In legend `opacity`")
  expect_warning(vg_legend_symbol(bogus = 1), "In legend `symbol`")
  expect_warning(vg_legend(type = "color", bogus = 1), "In legend `color`")
})

test_that("a standalone legend (with for_plot) warns too", {
  expect_warning(vg_legend_color(for_plot = "p", bogus = 1), "`bogus` is not a property of the `color` legend")
})

test_that("several unrecognized options produce a single warning naming all of them", {
  w <- character()
  withCallingHandlers(
    vg_legend_opacity(bogus = 1, nope = 2),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "`bogus`, `nope` are not properties of the `opacity` legend")
})

test_that("legends take camelCase options; a snake_case name warns and points at the real one", {
  expect_warning(vg_legend_symbol(tick_size = 5), "`tick_size` is not a property.*Did you perhaps mean `tickSize`\\?")
  expect_warning(vg_legend_color(margin_left = 5), "Did you perhaps mean `marginLeft`\\?")
  expect_no_warning(vg_legend_symbol(tickSize = 5, marginLeft = 5))
})

test_that("every real legend option, and for_plot, is accepted without a warning", {
  expect_no_warning(vg_legend_color(
    as = param(sel), columns = 2, field = "species", height = 40, label = "Species",
    marginBottom = 1, marginLeft = 2, marginRight = 3, marginTop = 4, tickSize = 5, width = 100
  ))
  # `for_plot` is vg_legend()'s own argument (mosaic's `for`), not an option
  expect_no_warning(vg_legend_color(for_plot = "p", label = "L"))
  expect_no_warning(vg_legend_color())
})

test_that(".vg_legend_props is the schema's option set, without the legend/for discriminants", {
  expect_false("legend" %in% .vg_legend_props)
  expect_false("for" %in% .vg_legend_props)
  expect_true(all(c("as", "label", "field", "tickSize", "width") %in% .vg_legend_props))
})

test_that("`color =` on a legend warns without a fill/stroke hint (legends have neither)", {
  w <- character()
  withCallingHandlers(
    vg_legend_color(color = "red"),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "`color` is not a property of the `color` legend")
  expect_no_match(w, "fill")
})

test_that("the warning doesn't stop the legend from being built", {
  expect_warning(frag <- vg_legend_color(bogus = 1), "bogus")
  expect_s3_class(frag, "vg_plot_fragment")
  expect_equal(frag$items[[1]]$type, "color")

  expect_warning(standalone <- vg_legend_color(for_plot = "p", bogus = 1), "bogus")
  expect_s3_class(standalone, "vg_legend")
})
