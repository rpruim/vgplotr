test_that("vg_duckdb_cache_status() reports not cached when nothing is there", {
  tmp <- tempfile("vgplotr-cache-")
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp)

  status <- vg_duckdb_cache_status()
  expect_false(status$cached)
  expect_true(is.na(status$size))
})

test_that("vg_duckdb_cache_status() reports cached once both files exist", {
  tmp <- tempfile("vgplotr-cache-")
  dir.create(tmp)
  writeLines("x", file.path(tmp, "duckdb-eh.wasm"))
  writeLines("y", file.path(tmp, "duckdb-browser-eh.worker.js"))
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp)

  status <- vg_duckdb_cache_status()
  expect_true(status$cached)
  expect_gt(status$size, 0)
})

test_that("vg_uncache_duckdb() removes the cache directory", {
  tmp <- tempfile("vgplotr-cache-")
  dir.create(tmp)
  file.create(file.path(tmp, "duckdb-eh.wasm"))
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp)

  vg_uncache_duckdb()
  expect_false(dir.exists(tmp))
})

test_that("vg_duckdb_cache_dependency() is NULL when not cached, a real dependency when cached", {
  tmp <- tempfile("vgplotr-cache-")
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp)
  expect_null(vg_duckdb_cache_dependency())

  dir.create(tmp)
  file.create(file.path(tmp, "duckdb-eh.wasm"))
  file.create(file.path(tmp, "duckdb-browser-eh.worker.js"))
  dep <- vg_duckdb_cache_dependency()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "vgplotr-duckdb-wasm")
  expect_equal(dep$attachment, list(wasm = "duckdb-eh.wasm", worker = "duckdb-browser-eh.worker.js"))
})

test_that("declining to cache persists via the preference file, and can be reset", {
  tmp <- tempfile("vgplotr-pref-")
  testthat::local_mocked_bindings(vg_duckdb_cache_pref_file = function() tmp)

  expect_false(vg_duckdb_cache_declined())
  vg_duckdb_cache_decline()
  expect_true(vg_duckdb_cache_declined())
  vg_duckdb_cache_reset_preference()
  expect_false(vg_duckdb_cache_declined())
})

test_that("maybe_offer_duckdb_cache() is a no-op when already cached", {
  tmp <- tempfile("vgplotr-cache-")
  dir.create(tmp)
  file.create(file.path(tmp, "duckdb-eh.wasm"))
  file.create(file.path(tmp, "duckdb-browser-eh.worker.js"))
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp)

  expect_null(maybe_offer_duckdb_cache())
})

test_that("maybe_offer_duckdb_cache() never prompts outside an interactive session", {
  tmp <- tempfile("vgplotr-cache-")
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp)

  expect_false(interactive())
  # Would error/hang trying to open a menu if this didn't bail out first.
  expect_null(maybe_offer_duckdb_cache())
})

test_that("vg_render() attaches the duckdb-wasm dependency only when the cache exists", {
  spec <- vg_create() |> vg_dot(x = ~a, y = ~b)

  tmp_empty <- tempfile("vgplotr-cache-")
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp_empty)
  w1 <- vg_render(spec)
  expect_null(w1$dependencies)

  tmp_full <- tempfile("vgplotr-cache-")
  dir.create(tmp_full)
  file.create(file.path(tmp_full, "duckdb-eh.wasm"))
  file.create(file.path(tmp_full, "duckdb-browser-eh.worker.js"))
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp_full)
  w2 <- vg_render(spec)
  expect_length(w2$dependencies, 1)
  expect_equal(w2$dependencies[[1]]$name, "vgplotr-duckdb-wasm")
})

test_that("vg_render()'s use_cache lets a single plot override the cache default", {
  spec <- vg_create() |> vg_dot(x = ~a, y = ~b)

  tmp_full <- tempfile("vgplotr-cache-")
  dir.create(tmp_full)
  file.create(file.path(tmp_full, "duckdb-eh.wasm"))
  file.create(file.path(tmp_full, "duckdb-browser-eh.worker.js"))
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp_full)

  # Cached, but this plot opts out -- no dependency attached even though one
  # is available.
  w1 <- vg_render(spec, use_cache = FALSE)
  expect_null(w1$dependencies)

  tmp_empty <- tempfile("vgplotr-cache-")
  testthat::local_mocked_bindings(vg_duckdb_cache_dir = function() tmp_empty)

  # Not cached, but this plot asks for it anyway -- harmless, falls back to
  # no dependency (i.e. the CDN) exactly like the default would.
  w2 <- vg_render(spec, use_cache = TRUE)
  expect_null(w2$dependencies)
})
