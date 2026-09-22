test_that("vg_widget() renders meta$title as a caption above the widget", {
  spec <- vg_create(title = "AAPL closing value") |> vg_mark_dot(x = ~a, y = ~b)
  w <- vg_widget(spec)

  expect_length(w$prepend, 1)
  expect_true(grepl("AAPL closing value", as.character(w$prepend[[1]]), fixed = TRUE))
})

test_that("vg_widget() adds no prepend when there's no title", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  w <- vg_widget(spec)

  expect_null(w$prepend)
})

test_that("vg_widget() renders a title set via vg_create(title=) or vg_meta() the same way", {
  via_create <- vg_create(title = "Via create") |> vg_mark_dot(x = ~a, y = ~b)
  via_meta <- vg_create() |> vg_mark_dot(x = ~a, y = ~b) |> vg_meta(title = "Via meta")

  expect_true(grepl("Via create", as.character(vg_widget(via_create)$prepend[[1]]), fixed = TRUE))
  expect_true(grepl("Via meta", as.character(vg_widget(via_meta)$prepend[[1]]), fixed = TRUE))
})

test_that("vg_attributes(title=) does NOT render a title -- title isn't a real plot attribute", {
  # vg_attributes() targets the spec's own top-level *plot* attrs (width,
  # height, ...); `title` isn't one of mosaic's plot attributes, so this
  # sets an inert top-level `title` key rather than mosaic-spec's `meta`,
  # which is what vg_widget() actually checks. Use vg_create(title =)/
  # vg_meta(title =) instead -- and vg_attributes() warns about exactly
  # this (see test-attributes.R).
  expect_warning(
    spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b) |> vg_attributes(title = "Not rendered"),
    "not a recognized mosaic-spec plot attribute"
  )

  expect_equal(spec$attrs$title, "Not rendered")
  expect_null(spec$meta$title)
  expect_null(vg_widget(spec)$prepend)
})

test_that("vg_widget() silently ignores arguments meant for vg_snapshot()/vg_iframe()", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  expect_no_error(vg_widget(spec, delay = 5, file = "x.png", vwidth = 100))
})

test_that("vg_widget() warns once on an ordered factor column (order isn't preserved when rendered)", {
  df <- data.frame(g = factor(c("lo", "hi"), levels = c("lo", "hi"), ordered = TRUE), v = 1:2)
  spec <- vg_create() |> vg_data(name = "d", data = df) |> vg_mark_dot(data_from = "d", x = ~g, y = ~v)

  expect_warning(vg_widget(spec), "ordered factor")
})

test_that("vg_widget() defaults width/height from a single plot's own size when the caller doesn't supply one", {
  # For a single-plot spec, mosaic-spec itself serializes the plot's own
  # attrs (from vg_mark_*()/vg_plot()) as top-level keys, sibling to
  # `plot:` -- the same place vg_attributes() writes to -- so this covers
  # the common case (no vg_attributes() call needed) as well as the
  # explicit one below.
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b, width = 680, height = 200)
  w <- vg_widget(spec)

  expect_equal(w$width, 680)
  expect_equal(w$height, 200)
})

test_that("vg_widget(width=, height=) explicitly overrides the spec's own size", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b, width = 680, height = 200)
  w <- vg_widget(spec, width = 300, height = 100)

  expect_equal(w$width, 300)
  expect_equal(w$height, 100)
})

test_that("vg_widget() leaves width/height NULL for a multi-plot layout with no top-level size", {
  # A vconcat/hconcat of several plots has no single size to default to,
  # so this deliberately doesn't try to derive one from the children --
  # htmlwidgets' own default sizing takes over, same as before this
  # feature existed.
  spec <- vg_create() |>
    vg_vconcat(
      vg_mark_dot(x = ~a, y = ~b, width = 680, height = 200),
      vg_mark_line_y(x = ~a, y = ~b, width = 680, height = 200)
    )
  w <- vg_widget(spec)

  expect_null(w$width)
  expect_null(w$height)
})

test_that("vg_widget() picks up vg_attributes(width=, height=) on a multi-plot layout", {
  spec <- vg_create() |>
    vg_attributes(width = 680, height = 400) |>
    vg_vconcat(
      vg_mark_dot(x = ~a, y = ~b, width = 680, height = 200),
      vg_mark_line_y(x = ~a, y = ~b, width = 680, height = 200)
    )
  w <- vg_widget(spec)

  expect_equal(w$width, 680)
  expect_equal(w$height, 400)
})

test_that("vg_widget() ignores a param()-driven width/height instead of erroring", {
  spec <- vg_create() |> vg_attributes(width = param(w)) |> vg_mark_dot(x = ~a, y = ~b)

  expect_no_error(w <- vg_widget(spec))
  expect_null(w$width)
})

test_that("vg_widget() reads a top-level width/height off a raw YAML/JSON string spec", {
  yaml_spec <- "
width: 500
height: 250
plot:
  - mark: dot
    data: {from: pts}
    x: a
    y: b
"
  w <- vg_widget(yaml_spec)

  expect_equal(w$width, 500)
  expect_equal(w$height, 250)
})
