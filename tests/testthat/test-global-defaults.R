test_that("vg_scale_all() sets the bare align/inset/padding attrs", {
  frag <- vg_mark_dot(x = ~a, y = ~b) |>
    vg_scale_all(align = 0.5, inset = 2, padding = 0.2)

  expect_equal(frag$attrs$align, 0.5)
  expect_equal(frag$attrs$inset, 2)
  expect_equal(frag$attrs$padding, 0.2)
})

test_that("vg_scale_all() doesn't collide with vg_scale_position()'s per-axis xAlign/xInset/xPadding", {
  frag <- vg_mark_dot(x = ~a, y = ~b) |>
    vg_scale_all(align = 0.5) |>
    vg_scale_x(align = 0.2)

  expect_equal(frag$attrs$align, 0.5)
  expect_equal(frag$attrs$xAlign, 0.2)
})

test_that("vg_scale_all() warns about an unrecognized attribute name", {
  expect_warning(vg_scale_all(bogus = 1), "`bogus`.*is not a recognized mosaic-spec plot attribute")
})

test_that("vg_guide_all() sets the bare axis/grid/ariaLabel/ariaDescription attrs", {
  frag <- vg_mark_dot(x = ~a, y = ~b) |>
    vg_guide_all(position = "both", grid = TRUE, aria_label = "Chart", aria_description = "A chart")

  expect_equal(frag$attrs$axis, "both")
  expect_true(frag$attrs$grid)
  expect_equal(frag$attrs$ariaLabel, "Chart")
  expect_equal(frag$attrs$ariaDescription, "A chart")
})

test_that("vg_guide_all() doesn't collide with vg_guide_position()'s per-axis xAxis/xGrid", {
  frag <- vg_mark_dot(x = ~a, y = ~b) |>
    vg_guide_all(grid = TRUE) |>
    vg_guide_x(grid = FALSE)

  expect_true(frag$attrs$grid)
  expect_false(frag$attrs$xGrid)
})

test_that("vg_guide_all() warns about an unrecognized attribute name", {
  expect_warning(vg_guide_all(bogus = 1), "`bogus`.*is not a recognized mosaic-spec plot attribute")
})
