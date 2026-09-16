test_that("vg_iframe() always embeds a live widget via <iframe>, even when not embeddable", {
  skip_if_not_installed("httpuv")

  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)
  testthat::local_mocked_bindings(pandoc_to = function(...) "gfm", .package = "knitr")

  tmp_root <- tempfile()
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)
  old_wd <- setwd(local({ dir.create(tmp_root); tmp_root }))
  on.exit(setwd(old_wd), add = TRUE)

  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  out <- "man/figures/test-vg-iframe-live.html"

  result <- vg_iframe(spec, file = out)

  expect_true(is.character(result))
  expect_true(file.exists(out))
})

test_that("vg_iframe() without file= returns an asis <iframe> block under the fig.path convention", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  w <- vg_widget(spec)
  tmp_root <- tempfile()
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)
  old_wd <- setwd(local({ dir.create(tmp_root); tmp_root }))
  on.exit(setwd(old_wd), add = TRUE)

  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)
  testthat::local_mocked_bindings(fig_path = function(ext, ...) "man/figures/test-vg-iframe.html", .package = "knitr")

  result <- vg_iframe(w, vwidth = 500, vheight = 300)

  expect_s3_class(result, "knit_asis")
  expect_true(file.exists("man/figures/test-vg-iframe.html"))
  expect_identical(
    as.character(result),
    '<iframe src="reference/figures/test-vg-iframe.html" width="500" height="300" style="border: none;" loading="lazy"></iframe>'
  )
})

test_that("vg_iframe(file =, src =) uses the custom file location and returns an <iframe> for the given src", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  out <- tempfile(fileext = ".html")
  on.exit(unlink(c(out, paste0(tools::file_path_sans_ext(out), "_files")), recursive = TRUE), add = TRUE)

  result <- vg_iframe(spec, file = out, src = "widgets/plot1.html", vwidth = 400, vheight = 200)

  expect_true(file.exists(out)) # saved where `file` said, not where `src` says
  expect_s3_class(result, "knit_asis") # src given -> returns the embed, not just the path
  expect_identical(
    as.character(result),
    '<iframe src="widgets/plot1.html" width="400" height="200" style="border: none;" loading="lazy"></iframe>'
  )
})

test_that("an explicit src overrides the automatic man/figures/ -> reference/figures/ rewrite", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  tmp_root <- tempfile()
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)
  old_wd <- setwd(local({ dir.create(tmp_root); tmp_root }))
  on.exit(setwd(old_wd), add = TRUE)

  out <- "man/figures/test-vg-iframe-custom-src.html"
  result <- vg_iframe(spec, file = out, src = "totally/different/path.html")

  expect_true(file.exists(out))
  expect_match(as.character(result), '<iframe src="totally/different/path.html"', fixed = TRUE)
})

test_that("vg_iframe() never embeds the local duckdb-wasm cache", {
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

  vg_iframe(spec, file = out)

  files_dir <- paste0(tools::file_path_sans_ext(out), "_files")
  expect_false(any(grepl("duckdb-wasm", list.dirs(files_dir, recursive = TRUE))))
})

test_that("vg_iframe() defaults use_cache to FALSE, but an explicit override wins", {
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b)
  seen_use_cache <- NULL
  testthat::local_mocked_bindings(
    vg_widget = function(spec, ..., use_cache = NULL) {
      seen_use_cache <<- use_cache
      list() # vg_iframe() only needs something to pass to saveWidget()
    },
    .package = "vgplotr"
  )
  testthat::local_mocked_bindings(
    saveWidget = function(...) invisible(NULL),
    .package = "htmlwidgets"
  )

  vg_iframe(spec, file = tempfile(fileext = ".html"))
  expect_false(seen_use_cache)

  vg_iframe(spec, file = tempfile(fileext = ".html"), use_cache = TRUE)
  expect_true(seen_use_cache)
})
