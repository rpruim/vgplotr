test_that("vg_scale_x()/vg_scale_y() translate snake_case args to the raw mosaic attrs", {
  frag <- vg_mark_dot(x = ~a, y = ~b) |>
    vg_scale_x(type = "log", domain = c(1, 100), nice = TRUE, inset_left = 5) |>
    vg_scale_y(zero = TRUE, padding_inner = 0.2)

  expect_equal(frag$attrs$xScale, "log")
  expect_equal(frag$attrs$xDomain, c(1, 100))
  expect_true(frag$attrs$xNice)
  expect_equal(frag$attrs$xInsetLeft, 5)
  expect_true(frag$attrs$yZero)
  expect_equal(frag$attrs$yPaddingInner, 0.2)
})

test_that("vg_scale_position() warns when the wrong axis's inset argument is used", {
  expect_warning(
    vg_scale_x(inset_top = 3),
    "`inset_top`.*only applies to the y scale.*use `inset_left`/`inset_right`"
  )
  expect_warning(
    vg_scale_y(inset_right = 3),
    "`inset_right`.*only applies to the x scale.*use `inset_top`/`inset_bottom`"
  )
})

test_that("vg_scale_x()/vg_scale_y() pass extra named args through as raw attrs", {
  frag <- vg_scale_x(xyDomain = "fixed")
  expect_equal(frag$attrs$xyDomain, "fixed")
})

test_that("vg_scale_x()/vg_scale_y() warn about an unrecognized attribute name", {
  expect_warning(
    vg_scale_x(bogus = 1),
    "`bogus`.*is not a recognized mosaic-spec plot attribute"
  )
})

test_that("vg_scale_x()/vg_scale_y() can be piped in any order relative to marks and vg_plot()", {
  spec <- vg_create() |>
    vg_scale_x(type = "log") |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_plot(width = 680)

  expect_equal(spec$layout$attrs$xScale, "log")
  expect_equal(spec$layout$attrs$width, 680)
})

test_that("vg_guide_x()/vg_guide_y() translate snake_case args to the raw mosaic attrs", {
  frag <- vg_mark_dot(x = ~a, y = ~b) |>
    vg_guide_x(label = "A", grid = TRUE, tick_format = "d") |>
    vg_guide_y(label = "B", position = "right")

  expect_equal(frag$attrs$xLabel, "A")
  expect_true(frag$attrs$xGrid)
  expect_equal(frag$attrs$xTickFormat, "d")
  expect_equal(frag$attrs$yLabel, "B")
  expect_equal(frag$attrs$yAxis, "right")
})

test_that("vg_guide_x()/vg_guide_y() warn about an unrecognized attribute name", {
  expect_warning(
    vg_guide_y(bogus = 1),
    "`bogus`.*is not a recognized mosaic-spec plot attribute"
  )
})

test_that("vg_mark_axis_x()/vg_mark_axis_y() still refer to the axisX/axisY marks, unaffected by vg_guide_*()", {
  frag <- vg_mark_axis_x(stroke = "red")
  expect_equal(frag$items[[1]]$mark, "axisX")
  expect_equal(frag$items[[1]]$encodings$stroke, "red")
})
