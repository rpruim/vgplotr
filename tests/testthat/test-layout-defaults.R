test_that("marks built inside a layout default to the spec's first data source", {
  s <- vg_create() |>
    vg_data(name = "athletes", data = data.frame(a = 1:2, b = 3:4)) |>
    vg_vconcat(
      vg_mark_line(x = ~a, y = ~b),
      vg_mark_dot(x = ~a, y = ~b, data_from = "other"),
      vg_mark_rule_y(y = 0),
      vg_mark_frame()
    )
  plots <- jsonlite::fromJSON(to_json(s), simplifyVector = FALSE)$vconcat
  expect_identical(plots[[1]]$plot[[1]]$data, list(from = "athletes"))
  expect_identical(plots[[2]]$plot[[1]]$data, list(from = "other"))
  # constants only, or a mark that takes no data: no default
  expect_null(plots[[3]]$plot[[1]]$data)
  expect_null(plots[[4]]$plot[[1]]$data)
  # the widget sees the same
  payload <- as_spec_payload(s)
  expect_identical(payload$spec$vconcat[[1]]$plot[[1]]$data, list(from = "athletes"))
})

test_that("inputs take data_from, default to the first data source, and accept an index", {
  d <- data.frame(g = c("a", "b"), v = 1:2)
  s <- vg_create() |>
    vg_data(name = "first", data = d) |>
    vg_data(name = "second", data = d) |>
    vg_vconcat(
      vg_menu(column = "g", as = param(p)),
      vg_menu(options = c("x", "y"), as = param(q)),
      vg_slider(data_from = -1L, column = "v", as = param(r)),
      vg_table(),
      vg_search(data_from = "second", column = "g", as = param(t))
    )
  kids <- jsonlite::fromJSON(to_json(s), simplifyVector = FALSE)$vconcat
  expect_identical(kids[[1]]$from, "first")   # menu with a column: default
  expect_null(kids[[2]]$from)                 # menu with fixed options: no data
  expect_identical(kids[[3]]$from, "second")  # index, from the end
  expect_identical(kids[[4]]$from, "first")   # a table always reads data
  expect_identical(kids[[5]]$from, "second")  # a name, kept
  # Mosaic's own `from =` still reaches the input through `...`
  expect_identical(vg_menu(from = "x", column = "g")$options$from, "x")
})
