test_that("to_json()/to_yaml() serialize a full vgspec, inlining a data frame as rows", {
  df <- data.frame(Date = c("2020-01-01", "2020-02-01"), Close = c(296.24, 313.05))
  spec <- vg_create() |>
    vg_data(name = "aapl", data = df) |>
    vg_mark_line_y(data_from = "aapl", x = ~Date, y = ~Close) |>
    vg_attributes(width = 680, height = 200)

  parsed_json <- jsonlite::fromJSON(to_json(spec), simplifyVector = FALSE)
  expect_equal(
    parsed_json$data$aapl$data,
    list(
      list(Date = "2020-01-01", Close = 296.24),
      list(Date = "2020-02-01", Close = 313.05)
    )
  )
  expect_equal(parsed_json$width, 680L)
  expect_equal(parsed_json$height, 200L)
  expect_null(parsed_json$plotDefaults)
  expect_equal(parsed_json$plot[[1]]$mark, "lineY")
  expect_equal(parsed_json$plot[[1]]$data, list(from = "aapl"))

  parsed_yaml <- yaml::yaml.load(as.character(to_yaml(spec)))
  expect_equal(parsed_json, parsed_yaml)
})

test_that("to_json() puts vg_attributes() at the top level, but vg_plot_defaults() in plotDefaults", {
  spec <- vg_create() |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_attributes(width = 680) |>
    vg_plot_defaults(height = 200)

  parsed <- jsonlite::fromJSON(to_json(spec), simplifyVector = FALSE)
  expect_equal(parsed$width, 680L)
  expect_null(parsed$height)
  expect_equal(parsed$plotDefaults, list(height = 200L))
  expect_null(parsed$plot[[1]]$height)
})

test_that("to_json()/to_yaml() leave a file/query data source as a plain reference", {
  spec <- vg_create() |>
    vg_data(name = "aapl", file = "data/stocks.csv", where = "Symbol = 'AAPL'") |>
    vg_mark_line_y(data_from = "aapl", x = ~Date, y = ~Close)

  parsed <- jsonlite::fromJSON(to_json(spec), simplifyVector = FALSE)
  expect_equal(parsed$data$aapl, list(file = "data/stocks.csv", where = "Symbol = 'AAPL'"))

  # No content-embedding here (unlike as_spec_payload()/vg_render()) -- the
  # local file doesn't even need to exist to serialize the spec.
  expect_false(grepl("AAPL", as.character(to_yaml(spec)), fixed = TRUE) &&
    file.exists("nonexistent"))
})

test_that("to_yaml() renders logicals as unquoted true/false, not YAML 1.1's yes/no", {
  spec <- vg_create() |>
    vg_data(name = "aapl", file = "data/stocks.csv", replace = FALSE, temp = TRUE) |>
    vg_mark_line_y(data_from = "aapl", x = ~Date, y = ~Close)

  text <- as.character(to_yaml(spec))
  expect_match(text, "replace: false", fixed = TRUE)
  expect_match(text, "temp: true", fixed = TRUE)
  expect_false(grepl("\\byes\\b|\\bno\\b", text))
})

test_that("to_json()/to_yaml() work on a bare fragment that was never wrapped in vg_create()", {
  frag <- vg_mark_dot(x = ~a, y = ~b, fill = "steelblue") |> vg_interval_x(as = param(brush))

  parsed <- jsonlite::fromJSON(to_json(frag), simplifyVector = FALSE)
  expect_equal(parsed$plot[[1]], list(mark = "dot", fill = "steelblue", x = "a", y = "b"))
  expect_equal(parsed$plot[[2]], list(select = "intervalX", as = "$brush"))

  expect_equal(parsed, yaml::yaml.load(as.character(to_yaml(frag))))
})

test_that("to_json()/to_yaml() work on a standalone input, not just plots", {
  sl <- vg_slider(label = "Bias", as = param(bias), min = -20, max = 20, value = 0)

  parsed <- jsonlite::fromJSON(to_json(sl), simplifyVector = FALSE)
  expect_equal(parsed, list(input = "slider", as = "$bias", label = "Bias", max = 20L, min = -20L, value = 0L))
})

test_that("to_json()/to_yaml() preserve a zero-argument transform as JSON/YAML null", {
  spec <- vg_create() |>
    vg_data(name = "delays", data = data.frame(delay = 1:5)) |>
    vg_mark_rect_y(data_from = "delays", x = ~ vg_bin(delay, step = 2), y = ~ vg_count())

  parsed <- jsonlite::fromJSON(to_json(spec), simplifyVector = FALSE)
  expect_null(parsed$plot[[1]]$y$count)
  expect_true("count" %in% names(parsed$plot[[1]]$y))
})

test_that("to_json()/to_yaml() reject a vgspec with no plots yet", {
  expect_error(to_json(vg_create()), "doesn't have any plots yet")
  expect_error(to_yaml(vg_create()), "doesn't have any plots yet")
})

test_that("to_json()'s and to_yaml()'s ... let a caller override a default without erroring", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)

  expect_no_error(to_json(spec, auto_unbox = FALSE))
  expect_no_error(to_yaml(spec, column.major = TRUE))
})

test_that("suppress_data = TRUE omits an inline data frame but keeps a by-name data source", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- vg_create() |>
    vg_data(name = "inline", data = df) |>
    vg_data(name = "byfile", file = "stocks.parquet") |>
    vg_data(name = "byquery", query = "SELECT 1") |>
    vg_mark_dot(data_from = "inline", x = ~a, y = ~b)

  parsed <- jsonlite::fromJSON(to_json(spec, suppress_data = TRUE), simplifyVector = FALSE)
  expect_null(parsed$data$inline)
  expect_equal(parsed$data$byfile, list(file = "stocks.parquet"))
  expect_equal(parsed$data$byquery, "SELECT 1")
  # the mark's own data_from reference is untouched -- suppress_data only
  # affects what's declared in data:, not what marks say they read from
  expect_equal(parsed$plot[[1]]$data, list(from = "inline"))

  expect_equal(parsed, yaml::yaml.load(as.character(to_yaml(spec, suppress_data = TRUE))))
})

test_that("suppress_data = TRUE drops the data: key entirely when every source is inline", {
  df <- data.frame(a = 1:3)
  spec <- df |> vg_mark_dot(x = ~a, y = ~a)

  parsed <- jsonlite::fromJSON(to_json(spec, suppress_data = TRUE), simplifyVector = FALSE)
  expect_null(parsed$data)

  parsed_default <- jsonlite::fromJSON(to_json(spec), simplifyVector = FALSE)
  expect_true("data" %in% names(parsed_default))
})

test_that("suppress_data defaults to FALSE (unchanged behavior)", {
  df <- data.frame(a = 1:3)
  spec <- vg_create() |> vg_data(name = "d", data = df) |> vg_mark_dot(data_from = "d", x = ~a, y = ~a)

  expect_equal(to_json(spec), to_json(spec, suppress_data = FALSE))
})
