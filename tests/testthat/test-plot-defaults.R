test_that("vg_plot_defaults() sets mosaic-spec plotDefaults, not the current plot's own attrs", {
  spec <- vg_create() |>
    vg_mark_dot(x = ~a, y = ~b, width = 400) |>
    vg_plot_defaults(height = 200)

  expect_equal(spec$layout$attrs, list(width = 400))
  expect_equal(spec$plot_defaults, list(height = 200))
})

test_that("vg_plot_defaults() conflicts are warned about like other attribute merges", {
  spec <- vg_create() |> vg_plot_defaults(width = 400)
  expect_warning(
    spec <- vg_plot_defaults(spec, width = 680),
    "Conflicting value for `width`"
  )
  expect_equal(spec$plot_defaults$width, 680)
})

test_that("vg_plot_defaults() is threaded into every plot, even nested ones, by as_spec_payload()", {
  spec <- vg_create() |>
    vg_vconcat(vg_mark_dot(x = ~a, y = ~b), vg_mark_dot(x = ~a, y = ~b)) |>
    vg_plot_defaults(width = 680)

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$vconcat[[1]]$width, 680)
  expect_equal(payload$spec$vconcat[[2]]$width, 680)
})

test_that("vg_plot_defaults() warns about a name that isn't a real plot attribute", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  expect_warning(
    vg_plot_defaults(spec, bogus = 1),
    "`bogus`.*is not a recognized mosaic-spec plot attribute"
  )
})
