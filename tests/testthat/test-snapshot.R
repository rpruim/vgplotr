test_that("vg_snapshot() always captures a static screenshot, regardless of context", {
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

test_that("vg_snapshot() accepts an already-built widget, not just a vgspec", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("webshot2")
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote:::find_chrome(), error = function(e) NA)
  skip_if(is.na(chrome), "no local Chrome/Chromium found")

  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  w <- vg_widget(spec)

  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)
  result <- vg_snapshot(w, file = out, delay = 2)

  expect_identical(result, out)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 1000)
})

test_that("vg_snapshot() without file= uses the fig.path convention and returns include_graphics()", {
  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)
  testthat::local_mocked_bindings(fig_path = function(ext, ...) tempfile(fileext = ext), .package = "knitr")
  testthat::local_mocked_bindings(
    vg_snapshot_capture = function(widget, out, ...) { file.create(out); invisible(out) },
    .package = "vgplotr"
  )

  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  result <- vg_snapshot(spec)

  expect_s3_class(result, "knit_image_paths")
  expect_true(file.exists(as.character(result)))
})
