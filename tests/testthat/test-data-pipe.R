test_that("vg_data() auto-generates a name when omitted", {
  df <- data.frame(a = 1:3)
  spec <- vg_create() |> vg_data(data = df)
  expect_equal(names(spec$data), "data")

  spec <- spec |> vg_data(data = df)
  expect_equal(names(spec$data), c("data", "data1"))

  spec <- spec |> vg_data(data = df)
  expect_equal(names(spec$data), c("data", "data1", "data2"))
})

test_that("vg_create(data =) registers a data frame as the spec's first data source", {
  df <- data.frame(a = 1:3)
  spec <- vg_create(data = df)
  expect_equal(names(spec$data), "data")
  expect_equal(spec$data$data$data, df)
})

test_that("a data frame piped as spec into a mark auto-registers it and sets data_from", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |> vg_mark_dot(x = ~a, y = ~b)

  expect_true(is_vgspec(spec))
  expect_equal(names(spec$data), "data")
  expect_equal(spec$data$data$data, df)
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "data")

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$plot[[1]]$data$from, "data")
})

test_that("an explicit data_from string on the data-frame shorthand names the registered source", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |> vg_mark_dot(data_from = "my_df", x = ~a, y = ~b)

  expect_equal(names(spec$data), "my_df")
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "my_df")
})

test_that("a second mark can reference the auto-registered data source by name", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_mark_line_y(data_from = "data", x = ~a, y = ~b)

  expect_equal(names(spec$data), "data")
  expect_length(spec$layout$items, 2)
  expect_equal(spec$layout$items[[2]]$encodings$data_from, "data")
})

test_that("the data-frame shorthand is equivalent to vg_create() |> vg_data(data = df) |> vg_mark_*()", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  via_shorthand <- df |> vg_mark_dot(x = ~a, y = ~b)
  via_explicit <- vg_create() |> vg_data(data = df) |> vg_mark_dot(data_from = "data", x = ~a, y = ~b)

  expect_equal(via_shorthand, via_explicit)
})

test_that("a mark with no data_from defaults to the spec's first registered data source", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))

  # data-frame-piped first mark, then a second mark with no data_from at all
  spec <- df |> vg_mark_dot(x = ~a, y = ~b) |> vg_mark_line_y(x = ~a, y = ~b)
  expect_equal(spec$layout$items[[2]]$encodings$data_from, "data")

  # same default via the explicit vg_create()/vg_data() path
  spec2 <- vg_create() |> vg_data(name = "d1", data = df) |> vg_mark_dot(x = ~a, y = ~b) |> vg_mark_line_y(x = ~a, y = ~b)
  expect_equal(spec2$layout$items[[1]]$encodings$data_from, "d1")
  expect_equal(spec2$layout$items[[2]]$encodings$data_from, "d1")

  # with more than one data source registered, it's the *first* one that wins
  spec3 <- vg_create() |>
    vg_data(name = "first", data = df) |>
    vg_data(name = "second", data = df) |>
    vg_mark_dot(x = ~a, y = ~b)
  expect_equal(spec3$layout$items[[1]]$encodings$data_from, "first")
})

test_that("an explicit data_from always wins over the first-data-source default", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- vg_create() |>
    vg_data(name = "first", data = df) |>
    vg_data(name = "second", data = df) |>
    vg_mark_dot(data_from = "second", x = ~a, y = ~b)
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "second")
})

test_that("marks with no `data` property (frame, sphere, hexgrid, axis/grid marks) never get a default data_from", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_mark_frame(stroke = "#ccc") |>
    vg_mark_sphere() |>
    vg_mark_hexgrid()

  expect_null(spec$layout$items[[2]]$encodings$data_from)
  expect_null(spec$layout$items[[3]]$encodings$data_from)
  expect_null(spec$layout$items[[4]]$encodings$data_from)

  payload <- as_spec_payload(spec)
  expect_null(payload$spec$plot[[2]]$data)
  expect_null(payload$spec$plot[[3]]$data)
  expect_null(payload$spec$plot[[4]]$data)
})

test_that("a mark with `data` support but only literal args doesn't get a default data_from", {
  # e.g. vg_mark_rule_x(x = 0, ...) as a plain reference line layered on a
  # data-bound plot -- unlike frame/sphere/etc. (which never have a `data`
  # property at all), ruleX *can* be data-bound, but nothing here
  # references a column, so attaching data_from would make mosaic try to
  # query the table with no selection list (confirmed: this used to
  # silently blank the whole plot).
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_mark_rule_x(x = 0, stroke = "firebrick")

  expect_null(spec$layout$items[[2]]$encodings$data_from)
  payload <- as_spec_payload(spec)
  expect_null(payload$spec$plot[[2]]$data)
  expect_equal(payload$spec$plot[[2]]$x, 0)
})

test_that("a mark using sql()/agg() unwrapped still gets a default data_from", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |> vg_mark_dot(x = sql("a + 1"), y = ~b)
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "data")
})

test_that("a mark using a transform function unwrapped still gets a default data_from", {
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |> vg_mark_rect_y(x = vg_bin(a, step = 1), y = vg_count())
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "data")
})

test_that("a mark using only param() references doesn't get a default data_from", {
  # a param is a reactive scalar, not a per-row data lookup
  spec <- vg_create() |>
    vg_data(name = "d1", data = data.frame(a = 1:3)) |>
    vg_mark_rule_x(x = param(threshold), stroke = "firebrick")
  expect_null(spec$layout$items[[1]]$encodings$data_from)
})

test_that("no default data_from is applied when the spec has no data source yet", {
  frag <- vg_mark_dot(x = ~a, y = ~b)
  expect_null(frag$items[[1]]$encodings$data_from)
})

test_that("data_from accepts a length-1 integer index into the spec's registered data sources", {
  df <- data.frame(a = 1:3)
  spec <- vg_create() |>
    vg_data(name = "first", data = df) |>
    vg_data(name = "second", data = df) |>
    vg_data(name = "third", data = df)

  expect_equal((spec |> vg_mark_dot(data_from = 1L, x = ~a))$layout$items[[1]]$encodings$data_from, "first")
  expect_equal((spec |> vg_mark_dot(data_from = 2L, x = ~a))$layout$items[[1]]$encodings$data_from, "second")
  expect_equal((spec |> vg_mark_dot(data_from = 3L, x = ~a))$layout$items[[1]]$encodings$data_from, "third")
  expect_equal((spec |> vg_mark_dot(data_from = -1L, x = ~a))$layout$items[[1]]$encodings$data_from, "third")
  expect_equal((spec |> vg_mark_dot(data_from = -2L, x = ~a))$layout$items[[1]]$encodings$data_from, "second")
  expect_equal((spec |> vg_mark_dot(data_from = -3L, x = ~a))$layout$items[[1]]$encodings$data_from, "first")
})

test_that("an out-of-range integer data_from index errors clearly", {
  df <- data.frame(a = 1:3)
  spec <- vg_create() |> vg_data(name = "only", data = df)

  expect_error(spec |> vg_mark_dot(data_from = 2L, x = ~a), "not a valid data source index")
  expect_error(spec |> vg_mark_dot(data_from = -2L, x = ~a), "not a valid data source index")
})

test_that("data_from = 0L is never an index -- it's literal inline data, like data_from = 0", {
  # 0 is never a valid 1-based index (there's no "0th" source), so 0L is
  # deliberately treated the same as the double 0 -- mosaic's own literal
  # inline-data shorthand -- rather than erroring.
  spec <- vg_mark_rule_y(data_from = 0L)
  expect_equal(serialize_item(spec$items[[1]])$data, list(0L))

  df <- data.frame(a = 1:3)
  spec2 <- vg_create() |> vg_data(name = "only", data = df) |> vg_mark_dot(data_from = 0L, x = ~a)
  expect_equal(spec2$layout$items[[1]]$encodings$data_from, 0L)
})

test_that("an integer data_from index with no data source registered yet errors clearly", {
  expect_error(vg_mark_dot(data_from = 1L, x = ~a), "needs at least one")
})

test_that("a bare (double) numeric data_from is still mosaic's literal inline-data shorthand, not an index", {
  # data_from = 0 (a double, not 0L) must keep meaning "inline data [0]",
  # e.g. for a single reference line -- only a strict R integer (1L, -1L,
  # ...) is treated as a data-source index. This is what data_from = c(0)
  # already relied on before the index feature existed.
  spec <- vg_mark_rule_y(data_from = c(0))
  expect_equal(serialize_item(spec$items[[1]])$data, list(0))

  spec2 <- vg_mark_rule_y(data_from = c(0, 10, 20))
  expect_equal(serialize_item(spec2$items[[1]])$data, list(0, 10, 20))
})

test_that("the default data_from (unset) is equivalent to data_from = 1L", {
  df <- data.frame(a = 1:3)
  spec <- vg_create() |> vg_data(name = "first", data = df) |> vg_data(name = "second", data = df)

  via_default <- spec |> vg_mark_dot(x = ~a)
  via_explicit <- spec |> vg_mark_dot(data_from = 1L, x = ~a)
  expect_equal(via_default, via_explicit)
})
