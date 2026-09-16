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

test_that("vg_snapshot(iframe = TRUE) always embeds a live widget via <iframe>, even when not embeddable", {
  skip_if_not_installed("httpuv")

  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)
  testthat::local_mocked_bindings(pandoc_to = function(...) "gfm", .package = "knitr")

  tmp_root <- tempfile()
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)
  old_wd <- setwd(local({ dir.create(tmp_root); tmp_root }))
  on.exit(setwd(old_wd), add = TRUE)

  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  out <- "man/figures/test-vg-snapshot-live.html"

  result <- vg_snapshot(spec, file = out, iframe = TRUE)

  expect_s3_class(result, "knit_asis")
  expect_true(file.exists(out))
  expect_match(as.character(result), '<iframe src="reference/figures/', fixed = TRUE)
})

test_that("vg_snapshot(iframe = TRUE) never embeds the local duckdb-wasm cache", {
  # Regression test: iframe mode saves to a *committed* file, and
  # htmlwidgets bundles a cached dependency as a real file copy --
  # duckdb-wasm's engine is a ~35 MB binary. On a machine with a local
  # cache set up (vg_cache_duckdb()), the old default (use_cache tracking
  # vg_duckdb_cache_status()) would silently embed a full copy of it into
  # every iframe-mode widget saved -- confirmed directly, this happened
  # and would have added ~70 MB to two committed example widgets.
  skip_if_not_installed("httpuv")

  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(c(out, paste0(tools::file_path_sans_ext(out), "_files")), recursive = TRUE), add = TRUE)

  vg_snapshot(spec, file = out, iframe = TRUE)

  files_dir <- paste0(tools::file_path_sans_ext(out), "_files")
  expect_false(any(grepl("duckdb-wasm", list.dirs(files_dir, recursive = TRUE))))
})

test_that("vg_snapshot_iframe() rewrites a man/figures/ path to reference/figures/ for the src", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  w <- vg_render(spec)
  tmp_root <- tempfile()
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)
  old_wd <- setwd(local({ dir.create(tmp_root); tmp_root }))
  on.exit(setwd(old_wd), add = TRUE)

  out <- "man/figures/test-vg-snapshot-iframe.html"
  result <- vg_snapshot_iframe(w, out, vwidth = 500, vheight = 300)

  expect_true(file.exists(out))
  expect_identical(
    as.character(result),
    '<iframe src="reference/figures/test-vg-snapshot-iframe.html" width="500" height="300" style="border: none;" loading="lazy"></iframe>'
  )
})
