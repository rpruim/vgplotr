test_that("can_embed_live_widget() defaults to TRUE outside any active knit", {
  old <- options(knitr.in.progress = NULL)
  on.exit(options(old), add = TRUE)
  expect_true(can_embed_live_widget())
})

test_that("can_embed_live_widget() is format-aware inside an active knit", {
  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)

  testthat::local_mocked_bindings(pandoc_to = function(...) "html", .package = "knitr")
  expect_true(can_embed_live_widget())

  testthat::local_mocked_bindings(pandoc_to = function(...) "gfm", .package = "knitr")
  expect_false(can_embed_live_widget())

  testthat::local_mocked_bindings(pandoc_to = function(...) "markdown", .package = "knitr")
  expect_false(can_embed_live_widget())

  testthat::local_mocked_bindings(pandoc_to = function(...) "latex", .package = "knitr")
  expect_false(can_embed_live_widget())

  testthat::local_mocked_bindings(pandoc_to = function(...) character(0), .package = "knitr")
  expect_true(can_embed_live_widget())
})

test_that("vg_render() mode='auto' returns a live widget when embeddable", {
  old <- options(knitr.in.progress = NULL)
  on.exit(options(old), add = TRUE)
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)

  expect_identical(vg_render(spec)$x, vg_widget(spec)$x)
  expect_s3_class(vg_render(spec), "htmlwidget")
})

test_that("vg_render() mode='auto' falls back to vg_snapshot() when not embeddable", {
  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)
  testthat::local_mocked_bindings(pandoc_to = function(...) "gfm", .package = "knitr")
  testthat::local_mocked_bindings(
    vg_snapshot = function(spec, ...) "called vg_snapshot",
    .package = "vgplotr"
  )

  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  expect_equal(vg_render(spec), "called vg_snapshot")
})

test_that("vg_render() mode= forces a specific rendering function regardless of context", {
  old <- options(knitr.in.progress = NULL) # would otherwise auto-pick "widget"
  on.exit(options(old), add = TRUE)
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)

  testthat::local_mocked_bindings(
    vg_widget = function(spec, ...) "called vg_widget",
    vg_snapshot = function(spec, ...) "called vg_snapshot",
    vg_iframe = function(spec, ...) "called vg_iframe",
    .package = "vgplotr"
  )

  expect_equal(vg_render(spec, mode = "widget"), "called vg_widget")
  expect_equal(vg_render(spec, mode = "snapshot"), "called vg_snapshot")
  expect_equal(vg_render(spec, mode = "iframe"), "called vg_iframe")
})

test_that("vg_render() rejects an unrecognized mode", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  expect_error(vg_render(spec, mode = "png"), "should be one of")
})

test_that("vg_render() forwards ... to whichever function ends up handling the request", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)

  testthat::local_mocked_bindings(
    vg_widget = function(spec, ...) list(...),
    .package = "vgplotr"
  )
  expect_equal(vg_render(spec, mode = "widget", width = 500)$width, 500)
})
