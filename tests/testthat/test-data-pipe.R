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

test_that("no default data_from is applied when the spec has no data source yet", {
  frag <- vg_mark_dot(x = ~a, y = ~b)
  expect_null(frag$items[[1]]$encodings$data_from)
})
