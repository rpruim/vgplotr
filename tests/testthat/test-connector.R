test_that("vg_wasm_connector()/vg_duckdb_connector() build the expected objects", {
  wc <- vg_wasm_connector()
  expect_s3_class(wc, "vg_connector")
  expect_true(is_vg_connector(wc))

  dc <- vg_duckdb_connector()
  expect_s3_class(dc, "vg_duckdb_connector")
  expect_true(dc$owns_con)
  expect_null(dc$con)

  expect_error(vg_duckdb_connector(con = "not a connection"), "DBI connection")
})

test_that("vg_widget() rejects a non-connector `connector` argument", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  expect_error(vg_widget(spec, connector = "wasm"), "vg_wasm_connector")
})

test_that("vg_widget() with the default connector doesn't touch native-duckdb machinery", {
  spec <- vg_create() |> vg_data(name = "d", data = data.frame(a = 1:3, b = 4:6)) |>
    vg_mark_dot(data_from = "d", x = ~a, y = ~b)
  w <- vg_widget(spec)
  expect_null(w$x$connector)
  expect_length(w$x$tables, 1)
})

test_that("vg_data_source_kind() classifies table/local_file/other correctly", {
  expect_equal(vg_data_source_kind(list(data = data.frame(a = 1))), "table")
  expect_equal(vg_data_source_kind(list(file = "local.csv")), "local_file")
  expect_equal(vg_data_source_kind(list(file = "https://example.com/x.parquet")), "other")
  expect_equal(vg_data_source_kind(list(query = "SELECT 1")), "other")
})

test_that("drop_factors() converts factor columns to character and leaves others alone", {
  df <- data.frame(a = factor(c("x", "y")), b = 1:2, stringsAsFactors = FALSE)
  out <- drop_factors(df)
  expect_type(out$a, "character")
  expect_equal(out$a, c("x", "y"))
  expect_type(out$b, "integer")
})

test_that("register_native_data_sources() registers tables and errors on an unsupported extension", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  spec <- vg_create() |> vg_data(name = "d", data = data.frame(a = 1:3, g = factor(c("x", "y", "x"))))
  register_native_data_sources(spec, con)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM d")$n, 3)

  bad_spec <- vg_create() |> vg_data(name = "bad", file = "no-such-file.tsv")
  expect_error(register_native_data_sources(bad_spec, con), "not found")
})

test_that("vg_duckdb_app()'s handler implements mosaic's REST protocol", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("nanoarrow")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "t", data.frame(a = 1:3, b = c("x", "y", "z")))

  app <- vg_duckdb_app(con)
  fake_req <- function(method, body = "") {
    list(REQUEST_METHOD = method, rook.input = list(read = function(...) charToRaw(body)))
  }

  # CORS preflight
  res <- app$call(fake_req("OPTIONS"))
  expect_equal(res$status, 204L)
  expect_equal(res$headers[["Access-Control-Allow-Origin"]], "*")

  # malformed body
  res <- app$call(fake_req("POST", "not json"))
  expect_equal(res$status, 400L)

  # exec
  res <- app$call(fake_req("POST", '{"type":"exec","sql":"SELECT 1"}'))
  expect_equal(res$status, 200L)

  # json
  res <- app$call(fake_req("POST", '{"type":"json","sql":"SELECT * FROM t ORDER BY a"}'))
  expect_equal(res$status, 200L)
  rows <- jsonlite::fromJSON(res$body, simplifyDataFrame = FALSE)
  expect_equal(length(rows), 3)
  expect_equal(rows[[1]]$a, 1)

  # arrow
  res <- app$call(fake_req("POST", '{"type":"arrow","sql":"SELECT * FROM t ORDER BY a"}'))
  expect_equal(res$status, 200L)
  expect_type(res$body, "raw")
  stream <- nanoarrow::read_nanoarrow(res$body)
  tbl <- as.data.frame(stream)
  expect_equal(tbl$a, 1:3)
  expect_equal(tbl$b, c("x", "y", "z"))

  # unknown query type surfaces as a 500 with a clear message, not a crash
  res <- app$call(fake_req("POST", '{"type":"bogus","sql":"SELECT 1"}'))
  expect_equal(res$status, 500L)
  expect_match(res$body, "Unknown mosaic query type")
})

test_that("ensure_vg_duckdb_server() refuses to switch connections without stopping first", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("nanoarrow")
  skip_if_not_installed("DBI")

  vg_duckdb_server_stop()
  con_a <- DBI::dbConnect(duckdb::duckdb())
  con_b <- DBI::dbConnect(duckdb::duckdb())
  on.exit({
    vg_duckdb_server_stop()
    DBI::dbDisconnect(con_a, shutdown = TRUE)
    DBI::dbDisconnect(con_b, shutdown = TRUE)
  }, add = TRUE)

  info_a <- ensure_vg_duckdb_server(vg_duckdb_connector(con_a))
  expect_identical(info_a$con, con_a)
  # Same connection again -> reuses the running server, no error.
  info_a2 <- ensure_vg_duckdb_server(vg_duckdb_connector(con_a))
  expect_identical(info_a2$uri, info_a$uri)
  # A different connection while one is active -> a clear error.
  expect_error(ensure_vg_duckdb_server(vg_duckdb_connector(con_b)), "already running")
})
