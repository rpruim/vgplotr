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
