test_that("a plot-embedded interactor warns on an option mosaic-spec doesn't list for it", {
  expect_warning(
    vg_toggle(as = param(sel), bogus = 1),
    "In interactor `toggle`: `bogus` is not a property of the `toggle` interactor in mosaic-spec, so it won't have any effect\\.$"
  )
})

test_that("a layout-level input warns too, and is called an input", {
  expect_warning(
    vg_slider(column = "a", bogus = 1),
    "In input `slider`: `bogus` is not a property of the `slider` input in mosaic-spec"
  )
  expect_warning(vg_menu(column = "a", bogus = 1), "In input `menu`")
})

test_that("several unrecognized options produce a single warning naming all of them", {
  w <- character()
  withCallingHandlers(
    vg_table(from = "d", bogus = 1, nope = 2),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "`bogus`, `nope` are not properties of the `table` input")
})

test_that("`color =` on an interactor gets the fill/stroke hint only where it has them", {
  # highlight styles the unselected marks, so it has fill/stroke...
  expect_warning(
    vg_highlight(by = param(sel), color = "red"),
    "`color` is not a property.*Did you perhaps mean `fill` or `stroke`\\?"
  )
  # ...whereas a brush has neither, so there's nothing sensible to suggest
  w <- character()
  withCallingHandlers(
    vg_interval_x(as = param(sel), color = "red"),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "`color` is not a property of the `intervalX` interactor")
  expect_no_match(w, "fill")
})

test_that("the generic vg_interactor() takes camelCase; a snake_case name warns and points at the real one", {
  expect_warning(
    vg_interactor(interactor = "intervalX", as = param(sel), pixel_size = 2),
    "`pixel_size` is not a property.*Did you perhaps mean `pixelSize`\\?"
  )
  expect_warning(
    vg_interactor(interactor = "menu", column = "a", filter_by = param(sel)),
    "`filter_by` is not a property.*Did you perhaps mean `filterBy`\\?"
  )
  expect_no_warning(vg_interactor(interactor = "intervalX", as = param(sel), pixelSize = 2))
  # ...while the wrappers take snake_case
  expect_no_warning(vg_interval_x(as = param(sel), pixel_size = 2))
  expect_no_warning(vg_menu(column = "a", filter_by = param(sel)))
})

test_that("recognized options and plot-level attributes on an interactor never warn", {
  expect_no_warning(vg_pan_zoom(x = param(xs), y = param(ys)))
  expect_no_warning(vg_highlight(by = param(sel), fill_opacity = 0.2, opacity = 0.1))
  # a plot attribute riding along on a plot-embedded interactor
  expect_no_warning(vg_interval_x(as = param(sel), width = 400, x_domain = c(0, 1)))
  # an input's own `width`/`type` are real properties, not plot attributes
  expect_no_warning(vg_slider(column = "a", label = "A", min = 0, max = 1, width = 200))
  expect_no_warning(vg_search(column = "a", type = "prefix"))
})

test_that("the warning doesn't stop the interactor/input from being built", {
  expect_warning(frag <- vg_toggle(as = param(sel), bogus = 1), "bogus")
  expect_s3_class(frag, "vg_plot_fragment")
  expect_equal(frag$items[[1]]$type, "toggle")

  expect_warning(inp <- vg_slider(column = "a", bogus = 1), "bogus")
  expect_s3_class(inp, "vg_input")
})
