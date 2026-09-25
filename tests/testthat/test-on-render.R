# vg_on_render(): JavaScript hooks that vgplotr.js runs after a graphic renders.
# What they do happens in the browser; these check what R stores, validates,
# and hands to it.

spec <- function() vg_create() |> vg_mark_dot(x = ~a, y = ~b)

test_that("vg_on_render() adds a hook to the spec, leaving the rest alone", {
  s <- spec()
  out <- vg_on_render(s, js("() => 1"))
  expect_s3_class(out, "vgspec")
  expect_length(out$on_render, 1)
  expect_equal(out$on_render[[1]]$code, "() => 1")
  expect_identical(out$layout, s$layout)
})

test_that("hooks accumulate, in the order they were added", {
  out <- spec() |> vg_on_render(js("() => 'a'")) |> vg_on_render(js("() => 'b'"))
  expect_equal(vapply(out$on_render, function(h) h$code, character(1)), c("() => 'a'", "() => 'b'"))
})

test_that("a plain string is refused: the code has to be written with js()", {
  expect_error(vg_on_render(spec(), "() => 1"), "must be JavaScript written with js\\(\\)")
  expect_error(vg_on_render(spec(), NULL), "must be JavaScript written with js\\(\\)")
})

test_that("only a vgspec can take a hook", {
  expect_error(vg_on_render(list(), js("() => 1")), "must be a vgspec")
  expect_error(vg_on_render(NULL, js("() => 1")), "must be a vgspec")
})

test_that("a new spec has no hooks, and nothing is sent to the browser", {
  expect_length(vg_create()$on_render, 0)
  expect_false("onRender" %in% names(vg_widget(spec())$x))
})

test_that("vg_widget() sends the code strings, outside the mosaic spec", {
  w <- vg_widget(spec() |> vg_on_render(js("() => 'a'")) |> vg_on_render(js("async () => 'b'")))
  expect_equal(w$x$onRender, c("() => 'a'", "async () => 'b'"))
  expect_null(w$x$spec$onRender)
  expect_null(w$x$spec$on_render)
  expect_identical(w$x$spec, vg_widget(spec())$x$spec)
})

test_that("a single hook is still sent as something vgplotr.js can read", {
  # jsonlite writes a length-1 vector as a bare string; vgplotr.js wraps it in an array
  w <- vg_widget(spec() |> vg_on_render(js("() => 1")))
  expect_equal(w$x$onRender, "() => 1")
})

test_that("vg_render() carries the hooks to the live widget", {
  w <- vg_render(spec() |> vg_on_render(js("() => 1")), mode = "widget")
  expect_equal(w$x$onRender, "() => 1")
})

test_that("a JSON/YAML string spec carries no hooks", {
  json <- as.character(to_json(spec()))
  expect_false("onRender" %in% names(vg_widget(json)$x))
})

test_that("exporting warns that hooks are left out, and leaves them out", {
  s <- spec() |> vg_on_render(js("() => 1"))
  expect_warning(j <- to_json(s), "not part of mosaic-spec")
  expect_warning(y <- to_yaml(s), "not part of mosaic-spec")
  expect_false(grepl("=>|on_render|onRender", paste(j, y)))
  expect_identical(as.character(j), as.character(suppressWarnings(to_json(s))))
  # an unhooked spec exports silently
  expect_no_warning(to_json(spec()))
  expect_no_warning(to_yaml(spec()))
})

test_that("printing a spec mentions its hooks", {
  expect_output(print(spec() |> vg_on_render(js("() => 1")) |> vg_on_render(js("() => 2"))), "on_render: 2 hook(s)", fixed = TRUE)
  expect_no_match(paste(capture.output(print(spec())), collapse = "\n"), "on_render")
})

test_that("hooks survive the other spec-building steps", {
  s <- vg_create() |>
    vg_on_render(js("() => 1")) |>
    vg_data("d", data.frame(a = 1, b = 2)) |>
    vg_params(p = 1) |>
    vg_mark_dot(x = ~a, y = ~b, data_from = "d") |>
    vg_attributes(width = 300)
  expect_length(s$on_render, 1)
})
