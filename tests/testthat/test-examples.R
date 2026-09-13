# These reproduce the worked examples in design/api-brainstorming.qmd,
# updated for the vg_create()/vg_plot() renaming and the vg_mark()-family
# convenience functions.

test_that("simple one-layer plot (design doc example 1) builds the expected structure", {
  spec <- vg_create() |>
    vg_data(name = "aapl", file = "data/stocks.parquet", where = "Symbol = 'AAPL'") |>
    vg_mark_line_y(data_from = "aapl", x = ~Date, y = ~Close) |>
    vg_attributes(width = 680, height = 200)

  expect_true(is_vgspec(spec))
  expect_equal(spec$data$aapl, list(file = "data/stocks.parquet", where = "Symbol = 'AAPL'"))

  expect_true(is_vg_plot_fragment(spec$layout))
  expect_length(spec$layout$items, 1)

  mark <- spec$layout$items[[1]]
  expect_s3_class(mark, "vg_mark")
  expect_equal(mark$mark, "lineY")
  # Argument order isn't semantically meaningful (JSON objects are
  # unordered), and generated mark wrappers now reorder named args to
  # match their formals -- compare regardless of order.
  expect_equal(mark$encodings[c("data_from", "x", "y")], list(data_from = "aapl", x = ~Date, y = ~Close))
  expect_equal(sort(names(mark$encodings)), c("data_from", "x", "y"))

  expect_equal(spec$layout$attrs, list())
  expect_equal(spec$attrs, list(width = 680, height = 200))
})

test_that("a plot with a mark and a plot-embedded interactor (cf. overview-detail.yaml)", {
  spec <- vg_create() |>
    vg_data(name = "walk", file = "data/random-walk.parquet") |>
    vg_mark_area_y(data_from = "walk", x = ~t, y = ~v, fill = "steelblue") |>
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

  expect_equal(spec$attrs, list(width = 680, height = 200))
})

test_that("layout-level inputs (e.g. sliders) build a standalone vg_input", {
  input <- vg_interactor(NULL, "slider", as = param(point))
  expect_s3_class(input, "vg_input")
  expect_equal(input$type, "slider")
})
