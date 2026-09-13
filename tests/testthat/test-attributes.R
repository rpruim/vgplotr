test_that("vg_attributes() sets the spec's own top-level attrs, not plot_defaults", {
  spec <- vg_create() |>
    vg_mark_dot(x = ~a, y = ~b, width = 400) |>
    vg_attributes(height = 200)

  expect_equal(spec$layout$attrs, list(width = 400))
  expect_equal(spec$attrs, list(height = 200))
  expect_equal(spec$plot_defaults, list())
})

test_that("vg_attributes() conflicts are warned about like other attribute merges", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b) |> vg_attributes(width = 400)
  expect_warning(
    spec <- vg_attributes(spec, width = 680),
    "Conflicting value for `width`"
  )
  expect_equal(spec$attrs$width, 680)
})

test_that("vg_create(...) routes known plot attributes to the top level and everything else to meta", {
  spec <- vg_create(width = 680, height = 200, title = "My Chart")

  expect_equal(spec$attrs, list(width = 680, height = 200))
  expect_equal(spec$meta, list(title = "My Chart"))
  expect_equal(spec$plot_defaults, list())
})

test_that("vg_create(...) and vg_attributes()/vg_meta() write to the same places", {
  via_create <- vg_create(width = 680, title = "My Chart")
  via_setters <- vg_create() |> vg_attributes(width = 680) |> vg_meta(title = "My Chart")

  expect_equal(via_create$attrs, via_setters$attrs)
  expect_equal(via_create$meta, via_setters$meta)
})

test_that("vg_attributes() warns about a name that isn't a real plot attribute", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  expect_warning(
    vg_attributes(spec, title = "x"),
    "`title`.*is not a recognized mosaic-spec plot attribute"
  )
  expect_warning(
    vg_attributes(spec, bogus1 = 1, bogus2 = 2),
    "`bogus1`, `bogus2`.*are not recognized mosaic-spec plot attributes"
  )
})

test_that("vg_attributes() doesn't warn for real plot attributes", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  expect_no_warning(vg_attributes(spec, width = 680, height = 200))
})

test_that("vg_attributes()/vg_create() accept snake_case attribute names, stored under mosaic's camelCase key", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b) |> vg_attributes(x_domain = c(0, 100), color_scheme = "blues")
  expect_equal(spec$attrs, list(xDomain = c(0, 100), colorScheme = "blues"))
  expect_no_warning(vg_attributes(vg_create() |> vg_mark_dot(x = ~a, y = ~b), x_domain = c(0, 100)))

  via_create <- vg_create(x_domain = c(0, 100), y_label = "count")
  expect_equal(via_create$attrs, list(xDomain = c(0, 100), yLabel = "count"))
})
