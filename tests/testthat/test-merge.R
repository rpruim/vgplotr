test_that("compatible plot-level attributes from sibling marks accumulate silently", {
  frag <- vg_mark_dot(x = ~a, y = ~b, width = 680) |>
    vg_mark_line_y(x = ~a, y = ~c, height = 200)

  expect_length(frag$items, 2)
  expect_equal(frag$attrs$width, 680)
  expect_equal(frag$attrs$height, 200)
})

test_that("identical repeated plot-level attributes don't warn", {
  expect_no_warning(
    vg_mark_dot(x = ~a, y = ~b, width = 680) |> vg_mark_line_y(x = ~a, y = ~c, width = 680)
  )
})

test_that("conflicting plot-level attributes warn and the later value wins", {
  expect_warning(
    frag <- vg_mark_dot(x = ~a, y = ~b, width = 400) |> vg_mark_line_y(x = ~a, y = ~c, width = 680),
    "Conflicting value for `width`"
  )
  expect_equal(frag$attrs$width, 680)
})

test_that("vg_plot() wrapping children is equivalent to piping them", {
  wrapped <- vg_plot(vg_mark_dot(x = ~a, y = ~b), vg_mark_line_y(x = ~a, y = ~c), width = 680)
  piped <- vg_mark_dot(x = ~a, y = ~b) |> vg_mark_line_y(x = ~a, y = ~c) |> vg_plot(width = 680)

  expect_equal(wrapped, piped)
})

test_that("mark encodings don't leak into plot-level attrs", {
  frag <- vg_mark_dot(x = ~a, y = ~b, width = 680)
  expect_equal(frag$items[[1]]$encodings, list(x = ~a, y = ~b))
  expect_equal(frag$attrs, list(width = 680))
})

test_that("vg_plot() warns about a name that isn't a real plot attribute", {
  expect_warning(
    vg_plot(vg_mark_dot(x = ~a, y = ~b), bogus = 1),
    "`bogus`.*is not a recognized mosaic-spec plot attribute"
  )
})

test_that("vg_plot() doesn't warn about attrs inherited from a child mark/interactor", {
  # width here comes through vg_mark_dot()'s own split_plot_args(), already known
  # valid -- only vg_plot()'s own directly-supplied `...` should be checked.
  expect_no_warning(vg_plot(vg_mark_dot(x = ~a, y = ~b, width = 680)))
})

test_that("merge_attrs() preserves an explicit NULL value instead of dropping it", {
  # xAxis = NULL is mosaic's own way of hiding an axis -- a real, meaningful
  # value, not "unset." merge_attrs() must not treat it as "remove this key."
  frag <- vg_mark_dot(x = ~a, y = ~b) |> vg_plot(xAxis = NULL, width = 100)

  expect_true("xAxis" %in% names(frag$attrs))
  expect_null(frag$attrs$xAxis)
  expect_equal(frag$attrs$width, 100)
})

test_that("merge_attrs() still warns when a real (non-NULL) value conflicts with an explicit NULL", {
  expect_warning(
    frag <- vg_mark_dot(x = ~a, y = ~b, xAxis = "bottom") |> vg_mark_line_y(x = ~a, y = ~c, xAxis = NULL),
    "Conflicting value for `xAxis`"
  )
  expect_null(frag$attrs$xAxis)
})
