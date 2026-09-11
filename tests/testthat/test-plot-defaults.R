test_that("vg_attributes() sets top-level plot_defaults, not the current plot's own attrs", {
  spec <- vg_create() |>
    vg_dot(x = ~a, y = ~b, width = 400) |>
    vg_attributes(height = 200)

  expect_equal(spec$layout$attrs, list(width = 400))
  expect_equal(spec$plot_defaults, list(height = 200))
})

test_that("vg_create(...) and vg_attributes() write to the same place", {
  via_create <- vg_create(width = 680, height = 200)
  via_attributes <- vg_create() |> vg_attributes(width = 680, height = 200)

  expect_equal(via_create$plot_defaults, via_attributes$plot_defaults)
})

test_that("vg_attributes() conflicts are warned about like other attribute merges", {
  spec <- vg_create(width = 400)
  expect_warning(
    spec <- vg_attributes(spec, width = 680),
    "Conflicting value for `width`"
  )
  expect_equal(spec$plot_defaults$width, 680)
})
