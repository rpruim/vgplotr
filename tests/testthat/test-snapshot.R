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

test_that("vg_snapshot() returns the live widget when it can be embedded", {
  old <- options(knitr.in.progress = NULL)
  on.exit(options(old), add = TRUE)
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)

  expect_identical(vg_snapshot(spec)$x, vg_render(spec)$x)
  expect_s3_class(vg_snapshot(spec), "htmlwidget")
})

test_that("vg_snapshot() accepts an already-built widget, not just a vgspec", {
  old <- options(knitr.in.progress = NULL)
  on.exit(options(old), add = TRUE)
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  w <- vg_render(spec)

  expect_identical(vg_snapshot(w), w)
})

test_that("vg_snapshot(file =) always captures a static screenshot, even when embeddable", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("webshot2")
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote:::find_chrome(), error = function(e) NA)
  skip_if(is.na(chrome), "no local Chrome/Chromium found")

  spec <- vg_create() |>
    vg_data(name = "d", data = data.frame(a = 1:3, b = c(4, 5, 6))) |>
    vg_mark_dot(data_from = "d", x = ~a, y = ~b)

  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)
  result <- vg_snapshot(spec, file = out, delay = 2)

  expect_identical(result, out)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 1000) # a real screenshot, not a near-empty file
})
