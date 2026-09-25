test_that("vg_selection() builds mosaic-spec's Selection shape", {
  s <- vg_selection("crossfilter")
  expect_s3_class(s, "vg_selection")
  expect_equal(unclass(s), list(select = "crossfilter"))

  s <- vg_selection("intersect", cross = TRUE, empty = FALSE)
  expect_equal(unclass(s), list(select = "intersect", cross = TRUE, empty = FALSE))

  expect_equal(vg_selection()$select, "crossfilter")   # the default
})

test_that("select must be a real selection type, with a suggestion for a near miss", {
  expect_error(vg_selection("union2"), "union")
  expect_error(vg_selection("value"), "must be one of")
})

test_that("cross and empty must be single TRUE/FALSE", {
  expect_error(vg_selection("intersect", cross = "yes"), "`cross` must be TRUE or FALSE")
  expect_error(vg_selection("intersect", empty = c(TRUE, FALSE)), "`empty` must be TRUE or FALSE")
  expect_error(vg_selection("intersect", empty = NA), "`empty` must be TRUE or FALSE")
})

test_that("`cross` on a crossfilter selection warns, since mosaic ignores it", {
  expect_warning(vg_selection("crossfilter", cross = TRUE), "no effect")
  expect_no_warning(vg_selection("intersect", cross = TRUE))
})

test_that("include takes a param, a list of them, or names -- all the same", {
  expected <- list(select = "union", include = list(new_vg_param("a"), new_vg_param("b")))
  expect_equal(unclass(vg_selection("union", include = list(param(a), param(b)))), expected)
  expect_equal(unclass(vg_selection("union", include = c("a", "b"))), expected)
  expect_equal(unclass(vg_selection("union", include = c("$a", "$b"))), expected)
  expect_equal(unclass(vg_selection("union", include = param(a)))$include, list(new_vg_param("a")))

  expect_error(vg_selection("union", include = 1), "`include` must be")
  expect_error(vg_selection("union", include = list()), "`include` must be")
  expect_error(vg_selection("union", include = c("a", NA)), "`include` must be")
})

test_that("selections serialize inside a spec, with include as $-references", {
  spec <- vg_create() |>
    vg_params(
      brush = vg_selection("crossfilter"),
      either = vg_selection("union", empty = TRUE, include = c("a", "b"))
    ) |>
    vg_mark_dot(x = ~a, y = ~b)
  out <- spec_to_list(spec)$params
  expect_equal(out$brush, list(select = "crossfilter"))
  expect_equal(out$either, list(select = "union", empty = TRUE, include = list("$a", "$b")))

  parsed <- jsonlite::fromJSON(as.character(to_json(spec)), simplifyVector = FALSE)
  expect_equal(parsed$params$either$include, list("$a", "$b"))
})

test_that("a hand-written list still works exactly as before", {
  spec <- vg_create() |> vg_params(brush = list(select = "intersect")) |> vg_mark_dot(x = ~a, y = ~b)
  expect_equal(spec_to_list(spec)$params$brush, list(select = "intersect"))
})

test_that("vg_param_date() writes an ISO date string", {
  expect_equal(unclass(vg_param_date(as.Date("2020-03-04"))), list(date = "2020-03-04"))
  expect_equal(vg_param_date("2020-03-04T05:06:07Z")$date, "2020-03-04T05:06:07Z")
  expect_equal(
    vg_param_date(as.POSIXct("2020-03-04 05:06:07", tz = "UTC"))$date, "2020-03-04T05:06:07Z"
  )
  # a time zone other than UTC is converted, not silently reinterpreted
  expect_equal(
    vg_param_date(as.POSIXct("2020-03-04 12:00:00", tz = "America/New_York"))$date, "2020-03-04T17:00:00Z"
  )
  expect_error(vg_param_date(1), "single Date")
  expect_error(vg_param_date(c("a", "b")), "single Date")
  expect_error(vg_param_date(NA_character_), "single Date")

  spec <- vg_create() |> vg_params(since = vg_param_date(as.Date("2020-01-01"))) |> vg_mark_dot(x = ~a, y = ~b)
  expect_equal(spec_to_list(spec)$params$since, list(date = "2020-01-01"))
})

test_that("selections and date params print readably", {
  expect_output(print(vg_selection("union", empty = TRUE, include = c("a", "b"))), "<vg_selection: union>")
  expect_output(print(vg_selection("union", include = c("a", "b"))), "include = $a, $b", fixed = TRUE)
  expect_output(print(vg_selection("intersect", cross = TRUE)), "cross = TRUE", fixed = TRUE)
  expect_output(print(vg_param_date("2020-01-01")), "<vg_param_date: 2020-01-01>", fixed = TRUE)
})
