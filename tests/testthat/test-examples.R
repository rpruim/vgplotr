# These reproduce the worked examples in design/api-brainstorming.qmd,
# updated for the vg_create()/vg_plot() renaming and the vg_mark()-family
# convenience functions.

test_that("simple one-layer plot (design doc example 1) builds the expected structure", {
  spec <- vg_create() |>
    vg_data(name = "aapl", file = "data/stocks.parquet", where = "Symbol = 'AAPL'") |>
    vg_line_y(data_from = "aapl", x = ~Date, y = ~Close) |>
    vg_attributes(width = 680, height = 200)

  expect_true(is_vgspec(spec))
  expect_equal(spec$data$aapl, list(file = "data/stocks.parquet", where = "Symbol = 'AAPL'"))

  expect_true(is_vg_plot_fragment(spec$layout))
  expect_length(spec$layout$items, 1)

  mark <- spec$layout$items[[1]]
  expect_s3_class(mark, "vg_mark")
  expect_equal(mark$mark, "lineY")
  expect_equal(mark$encodings, list(data_from = "aapl", x = ~Date, y = ~Close))

  expect_equal(spec$layout$attrs, list())
  expect_equal(spec$plot_defaults, list(width = 680, height = 200))
})

test_that("a plot with a mark and a plot-embedded interactor (cf. overview-detail.yaml)", {
  spec <- vg_create() |>
    vg_data(name = "walk", file = "data/random-walk.parquet") |>
    vg_area_y(data_from = "walk", x = ~t, y = ~v, fill = "steelblue") |>
    vg_interval_x(as = param(brush)) |>
    vg_attributes(width = 680, height = 200)

  expect_length(spec$layout$items, 2)

  mark <- spec$layout$items[[1]]
  expect_equal(mark$mark, "areaY")

  interactor <- spec$layout$items[[2]]
  expect_s3_class(interactor, "vg_interactor")
  expect_equal(interactor$type, "intervalX")
  expect_true(is_vg_param(interactor$options$as))
  expect_equal(format(interactor$options$as), "$brush")

  expect_equal(spec$plot_defaults, list(width = 680, height = 200))
})

test_that("layout-level inputs (e.g. sliders) aren't implemented yet, and say so clearly", {
  expect_error(vg_interactor(NULL, "slider", as = param(point)), "isn't implemented yet")
})
