test_that("vg_info() reports the expected fields", {
  info <- vg_info()
  expect_s3_class(info, "vg_info")
  expect_equal(info$mosaic, .vg_mosaic_version)
  expect_equal(info$duckdb_wasm, .vg_duckdb_wasm_version)
  expect_s3_class(info$duckdb_cache, "vg_duckdb_cache_status")
  expect_setequal(names(info$native_packages), c("duckdb", "nanoarrow", "DBI"))

  expect_output(print(info), "vgplotr ")
  expect_output(print(info), "Mosaic:")
  expect_output(print(info), "DuckDB-Wasm:")
})

test_that(".vg_mosaic_version/.vg_duckdb_wasm_version match what's actually bundled", {
  # The bundle's own banner comment (data-raw/js/build.js) is generated
  # directly from MOSAIC_VERSION/DUCKDB_WASM_VERSION at vendoring time --
  # the one source of truth that can't silently drift the way three
  # separately hand-maintained constants (here, R/duckdb_cache.R, and
  # data-raw/js/build.js itself) can. Read from inst/, not data-raw/, so
  # this test still works against an installed copy of the package, not
  # just a dev checkout (data-raw/ is excluded from the built package via
  # .Rbuildignore).
  bundle_path <- system.file("htmlwidgets", "lib", "mosaic-bundle.min.js", package = "vgplotr")
  skip_if(!nzchar(bundle_path), "bundled JS not found (not installed?)")
  banner <- readLines(bundle_path, n = 5, warn = FALSE)
  banner <- paste(banner, collapse = " ")

  mosaic_in_banner <- regmatches(banner, regexpr("(?<=@uwdata/mosaic-spec@)[0-9.]+", banner, perl = TRUE))
  duckdb_wasm_in_banner <- regmatches(banner, regexpr("(?<=@duckdb/duckdb-wasm@)[0-9.]+", banner, perl = TRUE))

  expect_equal(mosaic_in_banner, .vg_mosaic_version)
  expect_equal(duckdb_wasm_in_banner, .vg_duckdb_wasm_version)
})
