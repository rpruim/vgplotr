test_that("vg_legend() without for_plot embeds a legend into the current plot", {
  frag <- vg_dot(x = ~a, y = ~b, fill = ~group) |> vg_legend(type = "color")

  expect_true(is_vg_plot_fragment(frag))
  expect_length(frag$items, 2)
  expect_s3_class(frag$items[[2]], "vg_legend")
  expect_equal(frag$items[[2]]$type, "color")
  expect_null(frag$items[[2]]$for_plot)
})

test_that("vg_legend() with for_plot returns a standalone layout item, not a plot fragment", {
  legend <- vg_legend(type = "color", for_plot = "focus")

  expect_s3_class(legend, "vg_legend")
  expect_equal(legend$for_plot, "focus")
  expect_false(is_vg_plot_fragment(legend))
})

test_that("vg_legend() with for_plot refuses a spec argument", {
  frag <- vg_dot(x = ~a, y = ~b)
  expect_error(vg_legend(frag, type = "color", for_plot = "focus"), "standalone layout item")
})

test_that("vg_legend_color()/vg_legend_opacity()/vg_legend_symbol() set the right type", {
  expect_equal(vg_legend_color(for_plot = "p")$type, "color")
  expect_equal(vg_legend_opacity(for_plot = "p")$type, "opacity")
  expect_equal(vg_legend_symbol(for_plot = "p")$type, "symbol")
})

test_that("print.vg_plot_fragment() labels an embedded legend item as a legend, not an interactor", {
  frag <- vg_dot(x = ~a, y = ~b, fill = ~group) |> vg_legend_color(label = "Group")
  out <- paste(capture.output(print(frag)), collapse = "\n")

  expect_match(out, "legend : color", fixed = TRUE)
  expect_no_match(out, "interactor : color", fixed = TRUE)
})

test_that("as_spec_payload() serializes an embedded legend inside a plot's mark list", {
  spec <- vg_create() |>
    vg_data(name = "pts", data = data.frame(x = 1, y = 1, group = "a")) |>
    vg_dot(data_from = "pts", x = ~x, y = ~y, fill = ~group) |>
    vg_legend(type = "color")

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$plot[[2]], list(legend = "color"))
})

test_that("as_spec_payload() serializes a standalone legend with `for`", {
  spec <- vg_create() |>
    vg_vconcat(
      vg_dot(x = ~a, y = ~b) |> vg_plot(name = "focus"),
      vg_legend(type = "color", for_plot = "focus", label = "Group")
    )

  payload <- as_spec_payload(spec)
  legend_json <- payload$spec$vconcat[[2]]
  expect_equal(legend_json$legend, "color")
  expect_equal(legend_json[["for"]], "focus")
  expect_equal(legend_json$label, "Group")
})

test_that("vg_interactor() with a layout-level type builds a standalone vg_input", {
  input <- vg_interactor(NULL, "slider", label = "Bias", as = param(point), min = 0, max = 100)

  expect_s3_class(input, "vg_input")
  expect_equal(input$type, "slider")
  expect_equal(input$options$label, "Bias")
  expect_true(is_vg_param(input$options$as))
})

test_that("vg_slider()/vg_menu()/vg_search()/vg_table() are convenience wrappers", {
  expect_equal(vg_slider(min = 0)$type, "slider")
  expect_equal(vg_menu(options = c("a", "b"))$type, "menu")
  expect_equal(vg_search(column = "name")$type, "search")
  expect_equal(vg_table(from = "flights")$type, "table")
})

test_that("a layout-level input refuses a spec argument", {
  expect_error(vg_interactor(vg_dot(x = ~a, y = ~b), "slider"), "doesn't take a spec")
})

test_that("as_spec_payload() serializes a layout-level input alongside a plot", {
  spec <- vg_create() |>
    vg_vconcat(
      vg_slider(label = "Bias", as = param(point), min = 0, max = 100, step = 1),
      vg_dot(x = ~a, y = ~b)
    )

  payload <- as_spec_payload(spec)
  input_json <- payload$spec$vconcat[[1]]
  expect_equal(input_json$input, "slider")
  expect_equal(input_json$label, "Bias")
  expect_equal(input_json$as, "$point")
  expect_equal(input_json$min, 0)
})
