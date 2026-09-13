test_that("vg_scale_fx()/vg_scale_fy() translate snake_case args to the raw mosaic attrs", {
  frag <- vg_mark_dot(x = ~a, y = ~b, fx = ~g) |>
    vg_scale_fx(padding = 0.1, inset_left = 4) |>
    vg_scale_fy(reverse = TRUE, padding_inner = 0.2)

  expect_equal(frag$attrs$fxPadding, 0.1)
  expect_equal(frag$attrs$fxInsetLeft, 4)
  expect_true(frag$attrs$fyReverse)
  expect_equal(frag$attrs$fyPaddingInner, 0.2)
})

test_that("vg_scale_fx()/vg_scale_fy() have no type/nice/zero/clamp (facet scales are always band)", {
  expect_warning(
    vg_scale_fx(type = "log"),
    "`type`.*is not a recognized mosaic-spec plot attribute"
  )
})

test_that("vg_scale_facet() warns when the wrong axis's inset argument is used", {
  expect_warning(
    vg_scale_fx(inset_top = 3),
    "`inset_top`.*only applies to the fy scale.*use `inset_left`/`inset_right`"
  )
  expect_warning(
    vg_scale_fy(inset_right = 3),
    "`inset_right`.*only applies to the fx scale.*use `inset_top`/`inset_bottom`"
  )
})

test_that("vg_guide_fx()/vg_guide_fy() translate snake_case args to the raw mosaic attrs", {
  frag <- vg_mark_dot(x = ~a, y = ~b, fx = ~g) |>
    vg_guide_fx(label = "Group", grid = TRUE, tick_format = "d") |>
    vg_guide_fy(label = "Row", position = "right")

  expect_equal(frag$attrs$fxLabel, "Group")
  expect_true(frag$attrs$fxGrid)
  expect_equal(frag$attrs$fxTickFormat, "d")
  expect_equal(frag$attrs$fyLabel, "Row")
  expect_equal(frag$attrs$fyAxis, "right")
})

test_that("vg_guide_facet() has no label_arrow (mosaic doesn't define fxLabelArrow/fyLabelArrow)", {
  expect_false("label_arrow" %in% names(formals(vg_guide_facet)))
})

test_that("vg_guide_fx()/vg_guide_fy() warn about an unrecognized attribute name", {
  expect_warning(
    vg_guide_fx(bogus = 1),
    "`bogus`.*is not a recognized mosaic-spec plot attribute"
  )
})

test_that("vg_mark_axis_fx()/vg_mark_axis_fy() still refer to the axisFx/axisFy marks, unaffected by vg_guide_facet()", {
  frag <- vg_mark_axis_fx(stroke = "red")
  expect_equal(frag$items[[1]]$mark, "axisFx")
  expect_equal(frag$items[[1]]$encodings$stroke, "red")
})
