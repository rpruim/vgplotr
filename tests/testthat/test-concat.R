test_that("vg_vconcat() stacks separate plot fragments as separate plots", {
  concat <- vg_vconcat(vg_mark_dot(x = ~a, y = ~b), vg_mark_line_y(x = ~a, y = ~c))

  expect_true(is_vg_concat(concat))
  expect_equal(concat$direction, "vconcat")
  expect_length(concat$children, 2)
  expect_length(concat$children[[1]]$items, 1)
  expect_length(concat$children[[2]]$items, 1)
})

test_that("piping marks together (same plot) differs from vconcat()-ing them (separate plots)", {
  same_plot <- vg_mark_dot(x = ~a, y = ~b) |> vg_mark_line_y(x = ~a, y = ~c)
  expect_true(is_vg_plot_fragment(same_plot))
  expect_length(same_plot$items, 2)

  separate_plots <- vg_vconcat(vg_mark_dot(x = ~a, y = ~b), vg_mark_line_y(x = ~a, y = ~c))
  expect_true(is_vg_concat(separate_plots))
})

test_that("vg_vconcat() piped onto a vgspec with no layout yet sets the layout", {
  spec <- vg_create() |> vg_vconcat(vg_mark_dot(x = ~a, y = ~b), vg_mark_dot(x = ~a, y = ~c))
  expect_true(is_vgspec(spec))
  expect_true(is_vg_concat(spec$layout))
  expect_length(spec$layout$children, 2)
})

test_that("vg_vconcat() piped onto a vgspec with an existing plot wraps it as the first child", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b) |> vg_vconcat(vg_mark_dot(x = ~a, y = ~c))
  expect_true(is_vg_concat(spec$layout))
  expect_length(spec$layout$children, 2)
  expect_true(is_vg_plot_fragment(spec$layout$children[[1]]))
})

test_that("repeated vg_vconcat() calls of the same direction extend in place rather than nesting", {
  spec <- vg_create() |>
    vg_vconcat(vg_mark_dot(x = ~a, y = ~b)) |>
    vg_vconcat(vg_mark_dot(x = ~a, y = ~c))

  expect_true(is_vg_concat(spec$layout))
  expect_length(spec$layout$children, 2)
})

test_that("vg_hconcat() nested inside vg_vconcat() produces a nested layout node", {
  row <- vg_hconcat(vg_mark_dot(x = ~a, y = ~b), vg_mark_dot(x = ~a, y = ~c))
  spec <- vg_create() |> vg_vconcat(row, vg_mark_dot(x = ~a, y = ~d))

  expect_true(is_vg_concat(spec$layout))
  expect_length(spec$layout$children, 2)
  expect_true(is_vg_concat(spec$layout$children[[1]]))
  expect_equal(spec$layout$children[[1]]$direction, "hconcat")
})

test_that("as_spec_payload() serializes vconcat/hconcat, spacers, and threads plotDefaults to every plot", {
  df <- data.frame(t = 1:3, v = 1:3)

  spec <- vg_create() |>
    vg_data(name = "walk", data = df) |>
    vg_vconcat(
      vg_mark_area_y(data_from = "walk", x = ~t, y = ~v) |> vg_plot(height = 100),
      vg_hspace(10),
      vg_hconcat(vg_mark_dot(x = ~t, y = ~v), vg_mark_dot(x = ~t, y = ~v, height = 300))
    ) |>
    vg_plot_defaults(width = 680)

  payload <- as_spec_payload(spec)
  vconcat <- payload$spec$vconcat

  expect_length(vconcat, 3)
  expect_equal(vconcat[[1]]$width, 680)
  expect_equal(vconcat[[1]]$height, 100)
  expect_equal(vconcat[[2]], list(hspace = 10))
  expect_equal(vconcat[[3]]$hconcat[[1]]$width, 680)
  expect_equal(vconcat[[3]]$hconcat[[2]]$height, 300)
})
