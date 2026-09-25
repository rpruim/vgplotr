test_that("the interval helpers build a vg_interval for each unit", {
  units <- c("years", "months", "days", "hours", "minutes", "seconds", "milliseconds", "microseconds")
  for (u in units) {
    x <- get(paste0("vg_", u))(3)
    expect_s3_class(x, "vg_interval")
    expect_equal(serialize_value(x), stats::setNames(list(3), u))
    expect_equal(format(x), paste0("vg_", u, "(3)"))
  }
})

test_that("an interval count must be one finite number (or a param())", {
  expect_error(vg_days("a"), "single non-negative number of days")
  expect_error(vg_days(1:2), "single non-negative number of days")
  expect_error(vg_days(NA_real_), "single non-negative number of days")
  expect_error(vg_days(Inf), "single non-negative number of days")
  expect_error(vg_days(-7), "can't be negative")
  expect_error(vg_days(), "single non-negative number of days|missing")
  expect_equal(serialize_value(vg_days(0)), list(days = 0))
  expect_equal(serialize_value(vg_days(param(n))), list(days = "$n"))
})

test_that("a window frame can mix numbers, NULL and intervals", {
  t <- vg_avg(close, orderby = "date", range = list(vg_days(6), vg_days(0)))
  expect_equal(serialize_transform(t), list(avg = "close", orderby = "date", range = list(list(days = 6), list(days = 0))))

  t <- vg_avg(close, orderby = "date", range = list(vg_hours(2), NULL))
  expect_equal(serialize_transform(t)$range, list(list(hours = 2), NULL))

  # plain numbers are untouched
  expect_equal(serialize_transform(vg_sum(x, rows = c(-1, 1)))$rows, c(-1, 1))
})

test_that("an interval frame works inside a mapping formula, and survives to_json()", {
  spec <- vg_create() |>
    vg_mark_line_y(x = ~date, y = ~ vg_avg(close, orderby = "date", range = list(vg_days(6), vg_days(0))))
  out <- jsonlite::fromJSON(as.character(to_json(spec)), simplifyVector = FALSE)
  expect_equal(out$plot[[1]]$y$range, list(list(days = 6), list(days = 0)))
})

test_that("a plain list is serialized element by element; a data frame is left alone", {
  expect_equal(serialize_value(list(a = param(x), b = 1)), list(a = "$x", b = 1))
  df <- data.frame(a = 1)
  expect_identical(serialize_value(df), df)
})
