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
