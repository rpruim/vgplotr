test_that("vg_render() renders meta$title as a caption above the widget", {
  spec <- vg_create(title = "AAPL closing value") |> vg_mark_dot(x = ~a, y = ~b)
  w <- vg_render(spec)

  expect_length(w$prepend, 1)
  expect_true(grepl("AAPL closing value", as.character(w$prepend[[1]]), fixed = TRUE))
})

test_that("vg_render() adds no prepend when there's no title", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  w <- vg_render(spec)

  expect_null(w$prepend)
})

test_that("vg_render() renders a title set via vg_create(title=) or vg_meta() the same way", {
  via_create <- vg_create(title = "Via create") |> vg_mark_dot(x = ~a, y = ~b)
  via_meta <- vg_create() |> vg_mark_dot(x = ~a, y = ~b) |> vg_meta(title = "Via meta")

  expect_true(grepl("Via create", as.character(vg_render(via_create)$prepend[[1]]), fixed = TRUE))
  expect_true(grepl("Via meta", as.character(vg_render(via_meta)$prepend[[1]]), fixed = TRUE))
})

test_that("vg_attributes(title=) does NOT render a title -- title isn't a real plot attribute", {
  # vg_attributes() targets the spec's own top-level *plot* attrs (width,
  # height, ...); `title` isn't one of mosaic's plot attributes, so this
  # sets an inert top-level `title` key rather than mosaic-spec's `meta`,
  # which is what vg_render() actually checks. Use vg_create(title =)/
  # vg_meta(title =) instead -- and vg_attributes() warns about exactly
  # this (see test-attributes.R).
  expect_warning(
    spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b) |> vg_attributes(title = "Not rendered"),
    "not a recognized mosaic-spec plot attribute"
  )

  expect_equal(spec$attrs$title, "Not rendered")
  expect_null(spec$meta$title)
  expect_null(vg_render(spec)$prepend)
})
