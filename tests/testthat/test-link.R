# vg_widget(link =): widgets on one page that name the same link group share
# params/selections. The sharing itself happens in the browser
# (inst/htmlwidgets/vgplotr.js); these check what R hands it.

spec <- function() vg_create() |> vg_mark_dot(x = ~a, y = ~b)

test_that("link is carried to the widget payload, outside the mosaic spec", {
  w <- vg_widget(spec(), link = "cf")
  expect_equal(w$x$link, "cf")
  expect_null(w$x$spec$link)
})

test_that("an unlinked widget carries no link at all", {
  expect_null(vg_widget(spec())$x$link)
  expect_false("link" %in% names(vg_widget(spec())$x))
})

test_that("link must be one non-empty string", {
  expect_error(vg_widget(spec(), link = 1), "single, non-empty string")
  expect_error(vg_widget(spec(), link = c("a", "b")), "single, non-empty string")
  expect_error(vg_widget(spec(), link = ""), "single, non-empty string")
  expect_error(vg_widget(spec(), link = NA_character_), "single, non-empty string")
})

test_that("vg_render() forwards link to the live widget", {
  expect_equal(vg_render(spec(), mode = "widget", link = "cf")$x$link, "cf")
})

test_that("link does not change the spec that mosaic parses", {
  expect_identical(vg_widget(spec(), link = "cf")$x$spec, vg_widget(spec())$x$spec)
})

test_that("vg_iframe() warns that link cannot work across documents, and drops it", {
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  expect_warning(vg_iframe(spec(), file = out, link = "cf"), "`link` is ignored by vg_iframe")
  expect_false(grepl('"link"', paste(readLines(out, warn = FALSE), collapse = "\n"), fixed = TRUE))
  expect_no_warning(vg_iframe(spec(), file = out))
})
