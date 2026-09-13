test_that("vg_params() preserves an explicit NULL value instead of dropping it", {
  # offset = NULL is a real, meaningful mosaic param value (e.g. densityY's
  # offset, later set by a menu) -- utils::modifyList() would silently drop
  # the key entirely, same "NULL removes the element" pitfall as
  # merge_attrs() (test-merge.R) and vg_plot_defaults() (test-plot-defaults.R).
  spec <- vg_create() |> vg_params(bandwidth = 20, offset = NULL)

  expect_true("offset" %in% names(spec$params))
  expect_null(spec$params$offset)
  expect_equal(spec$params$bandwidth, 20)
})

test_that("vg_meta()/vg_config() also preserve an explicit NULL value", {
  spec <- vg_create() |> vg_meta(title = "x", credit = NULL) |> vg_config(extensions = NULL)

  expect_true("credit" %in% names(spec$meta))
  expect_null(spec$meta$credit)
  expect_true("extensions" %in% names(spec$config))
  expect_null(spec$config$extensions)
})
