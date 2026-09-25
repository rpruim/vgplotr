test_that("js() wraps a single string of JavaScript", {
  x <- js("() => 0.7")
  expect_s3_class(x, "vg_js")
  expect_equal(x$code, "() => 0.7")
  expect_equal(format(x), 'js("() => 0.7")')
  expect_output(print(x), 'js("() => 0.7")', fixed = TRUE)
})

test_that("js() rejects anything but one non-missing string", {
  expect_error(js(1), "single string")
  expect_error(js(c("a", "b")), "single string")
  expect_error(js(NA_character_), "single string")
  expect_error(js(NULL), "single string")
})

test_that("js() serializes as {js: <code>} wherever a value can appear", {
  spec <- vg_create() |>
    vg_mark_dot(x = ~a, y = ~b, opacity = js("() => 0.7")) |>
    vg_attributes(margin_left = js("() => 5")) |>
    vg_legend_color(label = js("() => 1")) |>
    vg_interval_x(as = param(s), pixel_size = js("() => 2"))
  out <- spec_to_list(spec)

  expect_equal(out$plot[[1]]$opacity, list(js = "() => 0.7"))
  expect_equal(out$marginLeft, list(js = "() => 5"))
  expect_equal(out$plot[[2]]$label, list(js = "() => 1"))
  expect_equal(out$plot[[3]]$pixelSize, list(js = "() => 2"))
})

test_that("js() values come out of to_json() and to_yaml() as {js: ...}", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b, opacity = js("() => 0.7"))

  parsed <- jsonlite::fromJSON(as.character(to_json(spec)), simplifyVector = FALSE)
  expect_equal(parsed$plot[[1]]$opacity, list(js = "() => 0.7"))

  parsed_yaml <- yaml::yaml.load(as.character(to_yaml(spec)))
  expect_equal(parsed_yaml$plot[[1]]$opacity, list(js = "() => 0.7"))
})

test_that("js() reaches the widget payload's spec unchanged", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b, opacity = js("() => 0.7"))
  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$plot[[1]]$opacity, list(js = "() => 0.7"))
})

test_that("a js() value is not mistaken for a data reference or an enum literal", {
  expect_no_warning(vg_mark_line_y(x = ~a, y = ~b, curve = js("() => 'basis'")))
  spec <- vg_create() |> vg_data("d", data.frame(a = 1, b = 2)) |>
    vg_mark_dot(x = ~a, y = ~b, opacity = js("() => 0.7"))
  expect_equal(spec_to_list(spec)$plot[[1]]$data, list(from = "d"))
})

test_that("printing shows js() values readably, not as a raw structure()", {
  expect_output(print(vg_mark_dot(x = ~a, y = ~b, opacity = js("() => 0.7"))$items[[1]]), 'opacity = js("() => 0.7")', fixed = TRUE)
  expect_output(print(vg_create() |> vg_attributes(margin_left = js("() => 5"))), 'marginLeft = js("() => 5")', fixed = TRUE)
  expect_output(print(vg_mark_dot(x = ~a, y = ~b, fill = sql("x"))$items[[1]]), 'fill = sql("x")', fixed = TRUE)
})
