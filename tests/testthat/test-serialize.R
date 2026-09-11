test_that("as_spec_payload() serializes a single mark with inline data", {
  df <- data.frame(Date = c("2020-01-01", "2020-02-01"), Close = c(296.24, 313.05))

  spec <- vg_create() |>
    vg_data(name = "aapl", data = df) |>
    vg_line_y(data_from = "aapl", x = ~Date, y = ~Close) |>
    vg_attributes(width = 680, height = 200)

  payload <- as_spec_payload(spec)

  expect_equal(payload$tables$aapl, df)
  expect_null(payload$spec$data)

  expect_equal(payload$spec$plot[[1]]$mark, "lineY")
  expect_equal(payload$spec$plot[[1]]$data, list(from = "aapl"))
  expect_equal(payload$spec$plot[[1]]$x, "Date")
  expect_equal(payload$spec$plot[[1]]$y, "Close")

  expect_equal(payload$spec$width, 680)
  expect_equal(payload$spec$height, 200)
})

test_that("as_spec_payload() serializes an interactor with a param() reference", {
  df <- data.frame(t = 1:3, v = c(1, 2, 3))

  spec <- vg_create() |>
    vg_data(name = "walk", data = df) |>
    vg_area_y(data_from = "walk", x = ~t, y = ~v, fill = "steelblue") |>
    vg_interval_x(as = param(brush))

  payload <- as_spec_payload(spec)

  expect_equal(payload$spec$plot[[1]]$fill, "steelblue")
  expect_equal(payload$spec$plot[[2]], list(select = "intervalX", as = "$brush"))
})

test_that("as_spec_payload() rejects formulas with more than a bare column name", {
  spec <- vg_create() |> vg_dot(x = ~ log(a), y = ~b)
  expect_error(as_spec_payload(spec), "transform functions")
})

test_that("as_spec_payload() refuses non-single-plot layouts", {
  spec <- vg_create()
  spec$layout <- "not a plot fragment"
  expect_error(as_spec_payload(spec), "no vconcat")
})

test_that("vg_data() with `data` stores a data frame, otherwise stores file/query options", {
  df <- data.frame(x = 1)
  spec <- vg_create() |> vg_data(name = "a", data = df) |> vg_data(name = "b", file = "x.parquet")

  expect_equal(spec$data$a, list(data = df))
  expect_equal(spec$data$b, list(file = "x.parquet"))
})
