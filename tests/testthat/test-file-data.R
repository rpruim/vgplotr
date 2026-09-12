# The real data file lives in vignettes/data/ (it's what the vignette
# itself uses); test_path() resolves relative to tests/testthat/ regardless
# of the test runner's working directory.
stocks_csv <- testthat::test_path("..", "..", "vignettes", "data", "stocks.csv")

test_that("vg_data() with file/where stores the options as given", {
  spec <- vg_create() |> vg_data(name = "aapl", file = "data/stocks.csv", where = "Symbol = 'AAPL'")
  expect_equal(spec$data$aapl, list(file = "data/stocks.csv", where = "Symbol = 'AAPL'"))
})

test_that("vg_data() with query stores the options as given", {
  spec <- vg_create() |> vg_data(name = "aapl", query = "SELECT * FROM read_csv('x.csv')")
  expect_equal(spec$data$aapl, list(query = "SELECT * FROM read_csv('x.csv')"))
})

test_that("as_spec_payload() leaves a genuine http(s) URL in the spec's data block", {
  spec <- vg_create() |>
    vg_data(name = "aapl", file = "https://example.com/stocks.csv", where = "Symbol = 'AAPL'") |>
    vg_line_y(data_from = "aapl", x = ~Date, y = ~Close)

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$data$aapl, list(file = "https://example.com/stocks.csv", where = "Symbol = 'AAPL'"))
  expect_equal(payload$files, list())
})

test_that("as_spec_payload() embeds a local file's content instead of leaving a path reference", {
  # a local (non-URL) `file =` path is read from disk and embedded, rather
  # than left in spec$data, since duckdb-wasm can't fetch a local path when
  # the rendered page is opened directly (file://) instead of served -- see
  # R/serialize.R's read_local_data_file().
  spec <- vg_create() |>
    vg_data(name = "aapl", file = stocks_csv, where = "Symbol = 'AAPL'") |>
    vg_line_y(data_from = "aapl", x = ~Date, y = ~Close)

  payload <- as_spec_payload(spec)
  expect_null(payload$spec$data)
  expect_equal(payload$files$aapl$ext, "csv")
  expect_equal(payload$files$aapl$encoding, "text")
  expect_true(grepl("AAPL", payload$files$aapl$content, fixed = TRUE))
  expect_equal(payload$files$aapl$options, list(where = "Symbol = 'AAPL'"))
})

test_that("as_spec_payload() keeps data-frame, local-file, and remote-URL sources separate", {
  spec <- vg_create() |>
    vg_data(name = "aapl", file = stocks_csv) |>
    vg_data(name = "remote", file = "https://example.com/x.csv") |>
    vg_data(name = "walk", data = data.frame(t = 1:3, v = 1:3)) |>
    vg_dot(data_from = "walk", x = ~t, y = ~v)

  payload <- as_spec_payload(spec)
  expect_equal(names(payload$files), "aapl")
  expect_equal(names(payload$tables), "walk")
  expect_equal(payload$spec$data, list(remote = list(file = "https://example.com/x.csv")))
})

test_that("as_spec_payload() errors clearly on a missing local file", {
  spec <- vg_create() |> vg_data(name = "x", file = "no/such/file.csv") |> vg_dot(x = ~a, y = ~b)
  expect_error(as_spec_payload(spec), "not found")
})

test_that("as_spec_payload() errors clearly on an unsupported local file extension", {
  path <- tempfile(fileext = ".tsv")
  writeLines("a\tb\n1\t2", path)
  on.exit(unlink(path))

  spec <- vg_create() |> vg_data(name = "x", file = path) |> vg_dot(x = ~a, y = ~b)
  expect_error(as_spec_payload(spec), "\\.tsv")
})
