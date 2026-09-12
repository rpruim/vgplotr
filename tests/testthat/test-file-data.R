test_that("vg_data() with file/where passes through as a data entry (not a table)", {
  spec <- vg_create() |> vg_data(name = "aapl", file = "data/stocks.csv", where = "Symbol = 'AAPL'")
  expect_equal(spec$data$aapl, list(file = "data/stocks.csv", where = "Symbol = 'AAPL'"))
})

test_that("vg_data() with query passes through as a data entry", {
  spec <- vg_create() |> vg_data(name = "aapl", query = "SELECT * FROM read_csv('x.csv')")
  expect_equal(spec$data$aapl, list(query = "SELECT * FROM read_csv('x.csv')"))
})

test_that("as_spec_payload() puts file/query data sources in spec$data, not tables", {
  spec <- vg_create() |>
    vg_data(name = "aapl", file = "data/stocks.csv", where = "Symbol = 'AAPL'") |>
    vg_line_y(data_from = "aapl", x = ~Date, y = ~Close)

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$data$aapl, list(file = "data/stocks.csv", where = "Symbol = 'AAPL'"))
  expect_equal(payload$tables, list())
})

test_that("as_spec_payload() keeps data-frame and file-based sources separate in one spec", {
  spec <- vg_create() |>
    vg_data(name = "aapl", file = "data/stocks.csv") |>
    vg_data(name = "walk", data = data.frame(t = 1:3, v = 1:3)) |>
    vg_dot(data_from = "walk", x = ~t, y = ~v)

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$data, list(aapl = list(file = "data/stocks.csv")))
  expect_equal(names(payload$tables), "walk")
})
