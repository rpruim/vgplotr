# The test helpers in helper-environment.R decide whether a test runs at all, so
# they are worth a few tests of their own: a wrong "yes" makes a test fail on a
# machine that can't run it (this happened, on a check machine with no Chrome).

test_that("usable_chrome_path() accepts only a single, non-empty path that exists", {
  real <- tempfile(fileext = ".exe")
  file.create(real)
  on.exit(unlink(real), add = TRUE)
  expect_true(usable_chrome_path(real))

  # every way chromote reports "no Chrome": NULL after a message, or a bad value
  expect_false(usable_chrome_path(NULL))              # is.na(NULL) is logical(0), not TRUE
  expect_false(usable_chrome_path(NA))
  expect_false(usable_chrome_path(NA_character_))
  expect_false(usable_chrome_path(""))
  expect_false(usable_chrome_path(character(0)))
  expect_false(usable_chrome_path(c(real, real)))
  expect_false(usable_chrome_path(file.path(tempdir(), "no-such-browser")))   # CHROMOTE_CHROME set, but wrong
})

test_that("skip_if_no_chrome() skips when chromote can't find a browser", {
  skip_on_cran()   # skip_if_no_chrome() itself skips on CRAN before it looks
  skip_if_not_installed("chromote")
  skip_if_not_installed("webshot2")
  skip_if_not_installed("httpuv")
  outcome <- function() {
    tryCatch({ skip_if_no_chrome(); "ran" }, skip = function(e) "skipped")
  }
  # find_chrome() returns NULL (with a message) when it finds nothing
  testthat::local_mocked_bindings(find_chrome = function() NULL, .package = "chromote")
  expect_equal(outcome(), "skipped")
  # ...and returns whatever CHROMOTE_CHROME says, existing or not
  testthat::local_mocked_bindings(find_chrome = function() file.path(tempdir(), "no-such-browser"), .package = "chromote")
  expect_equal(outcome(), "skipped")
})

test_that("test_duckdb_connection() keeps DuckDB's extensions and secrets out of ~/.duckdb", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  messages <- character()
  con <- withCallingHandlers(
    test_duckdb_connection(),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true(DBI::dbIsValid(con))
  # the "duckdb is storing downloaded extensions and secrets under ~/.duckdb" notice
  expect_no_match(messages, "~/.duckdb", fixed = TRUE)
})
