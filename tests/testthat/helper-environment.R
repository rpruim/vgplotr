# Helpers for tests that depend on the machine they run on. CRAN's check
# machines have no Chrome and shouldn't touch the user's home directory, so
# tests that need either say so here instead of failing there.

# Whether `path` (what chromote:::find_chrome() returned) is a usable browser.
# find_chrome() does not error when it finds nothing: it prints a "Google
# Chrome was not found" message and returns NULL, and if CHROMOTE_CHROME is set
# it returns that value without checking that it exists. So "no Chrome" can
# arrive as NULL, NA, "" or a path that isn't there -- and testing `is.na()`
# alone (as this once did) treats NULL, whose is.na() is logical(0), as
# "found", which is how these tests ran, and failed, on a machine with no Chrome.
usable_chrome_path <- function(path) {
  is.character(path) && length(path) == 1 && !is.na(path) && nzchar(path) && file.exists(path)
}

# Skips the test unless a real screenshot can be taken: not on CRAN (it needs a
# browser, and the page fetches DuckDB-Wasm from a CDN), and only with the
# packages and a Chrome/Chromium that vg_snapshot() needs.
skip_if_no_chrome <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("httpuv")
  testthat::skip_if_not_installed("webshot2")
  testthat::skip_if_not_installed("chromote")
  chrome <- suppressMessages(tryCatch(chromote:::find_chrome(), error = function(e) NULL))
  testthat::skip_if(!usable_chrome_path(chrome), "no local Chrome/Chromium found")
}

# A private in-memory DuckDB whose extensions and secrets live in a temporary
# directory. duckdb::duckdb() by default stores them under ~/.duckdb, shared
# with every other DuckDB client -- it says so on every call -- and a package's
# checks shouldn't write to the user's home directory.
test_duckdb_connection <- function() {
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
}
