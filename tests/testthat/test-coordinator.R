# vg_coordinator(): options for mosaic's client-side coordinator. What they do
# happens in the browser (inst/htmlwidgets/vgplotr.js); these check what R
# validates and hands it.

spec <- function() vg_create() |> vg_mark_dot(x = ~a, y = ~b)

test_that("vg_coordinator() defaults match mosaic's own", {
  x <- vg_coordinator()
  expect_s3_class(x, "vg_coordinator")
  expect_equal(
    unclass(x),
    list(cache = TRUE, consolidate = TRUE, preaggregate = TRUE, preaggregate_schema = "mosaic", logging = "all")
  )
})

test_that("each option is checked", {
  expect_error(vg_coordinator(cache = "yes"), "`cache` must be TRUE or FALSE")
  expect_error(vg_coordinator(consolidate = NA), "`consolidate` must be TRUE or FALSE")
  expect_error(vg_coordinator(preaggregate = c(TRUE, FALSE)), "`preaggregate` must be TRUE or FALSE")
  expect_error(vg_coordinator(logging = "verbose"), "must be one of")
  expect_error(vg_coordinator(logging = c("all", "none")))  # only the default vector means "first"
})

test_that("the pre-aggregation schema must be a simple name, since mosaic puts it in SQL", {
  expect_equal(vg_coordinator(preaggregate_schema = "cubes_2")$preaggregate_schema, "cubes_2")
  for (bad in list("", "my schema", "a\"b", "1abc", "a;drop", NA_character_, 1, c("a", "b"))) {
    expect_error(vg_coordinator(preaggregate_schema = bad), "must be a simple name", info = deparse(bad))
  }
})

test_that("the payload uses the names mosaic's Coordinator takes", {
  x <- vg_coordinator(cache = FALSE, preaggregate = FALSE, preaggregate_schema = "cubes", logging = "errors")
  expect_equal(
    coordinator_payload(x),
    list(cache = FALSE, consolidate = TRUE, preagg = list(enabled = FALSE, schema = "cubes"), logging = "errors")
  )
})

test_that("vg_widget() carries the options, outside the mosaic spec", {
  w <- vg_widget(spec(), coordinator = vg_coordinator(logging = "none"))
  expect_equal(w$x$coordinator$logging, "none")
  expect_equal(w$x$coordinator$preagg, list(enabled = TRUE, schema = "mosaic"))
  expect_null(w$x$spec$coordinator)
  expect_identical(w$x$spec, vg_widget(spec())$x$spec)
})

test_that("with no coordinator, the payload has none and mosaic keeps its defaults", {
  expect_false("coordinator" %in% names(vg_widget(spec())$x))
})

test_that("vgplotr.coordinator sets it for a whole document, and an argument overrides it", {
  old <- options(vgplotr.coordinator = vg_coordinator(logging = "errors"))
  on.exit(options(old), add = TRUE)
  expect_equal(vg_widget(spec())$x$coordinator$logging, "errors")
  expect_equal(vg_widget(spec(), coordinator = vg_coordinator(logging = "none"))$x$coordinator$logging, "none")
  expect_false("coordinator" %in% names(vg_widget(spec(), coordinator = NULL)$x))
})

test_that("coordinator must be NULL or a vg_coordinator()", {
  expect_error(vg_widget(spec(), coordinator = list(cache = FALSE)), "result of vg_coordinator")
  expect_error(vg_widget(spec(), coordinator = "none"), "result of vg_coordinator")
})

test_that("vg_render() forwards it to the live widget", {
  w <- vg_render(spec(), mode = "widget", coordinator = vg_coordinator(cache = FALSE))
  expect_false(w$x$coordinator$cache)
})

test_that("the payload serializes to the JSON vgplotr.js reads", {
  json <- jsonlite::toJSON(coordinator_payload(vg_coordinator(cache = FALSE)), auto_unbox = TRUE)
  expect_equal(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    list(cache = FALSE, consolidate = TRUE, preagg = list(enabled = TRUE, schema = "mosaic"), logging = "all")
  )
})

test_that("a vg_coordinator prints readably", {
  expect_output(print(vg_coordinator(logging = "errors")), "<vg_coordinator>")
  expect_output(print(vg_coordinator(logging = "errors")), 'logging = "errors"', fixed = TRUE)
})
