test_that("a zero-arg transform (vg_count()) serializes to a bare key with null value", {
  expect_equal(serialize_transform(vg_count()), list(count = NULL))
})

test_that("a one-arg transform with options (vg_bin()) serializes field + options", {
  t <- vg_bin(delay, interval = "month", step = 5)
  expect_equal(
    serialize_transform(t),
    list(bin = "delay", interval = "month", step = 5)
  )
})

test_that("a two-arg transform (vg_quantile()) serializes as an array", {
  expect_equal(
    serialize_transform(vg_quantile(amount, 0.9)),
    list(quantile = list("amount", 0.9))
  )
})

test_that("window options are recognized on both aggregate and window transforms", {
  expect_equal(
    serialize_transform(vg_sum(amount, orderby = "t", distinct = TRUE)),
    list(sum = "amount", distinct = TRUE, orderby = "t")
  )
  expect_equal(
    serialize_transform(vg_rank(orderby = "amount")),
    list(rank = NULL, orderby = "amount")
  )
})

test_that("param() works as a transform option value", {
  t <- vg_count(orderby = param(brush))
  expect_equal(serialize_transform(t), list(count = NULL, orderby = "$brush"))
})

test_that("param() works as a transform's field, e.g., dynamic column selection via a menu", {
  # vg_column(param(x)) is mosaic's `vg.column($x)` pattern -- letting a
  # menu/selection pick which data column an encoding uses at runtime.
  expect_equal(serialize_formula(~ vg_column(param(x))), list(column = "$x"))

  # Also works when the param is stored in a variable first, rather than
  # constructed inline -- a bare symbol whose *value* (not its name) is a
  # vg_param must still resolve to the param, not be treated as a literal
  # column name called "xp".
  xp <- param(x)
  expect_equal(serialize_formula(~ vg_column(xp)), list(column = "$x"))
})

test_that("a bare symbol that isn't a param still serializes as a literal column name", {
  # Most formula fields are like this: `delay` in ~vg_bin(delay) is never a
  # bound R variable, just the data column's name.
  expect_equal(serialize_formula(~ vg_bin(delay, step = 5))$bin, "delay")

  # And an ordinary bound variable that happens to not be a vg_param still
  # falls back to being read as the column name, not its R value.
  delay <- "not a param"
  expect_equal(serialize_formula(~ vg_bin(delay, step = 5))$bin, "delay")
})

test_that("the same transform works identically inside a formula and called directly", {
  via_formula <- serialize_formula(~ vg_bin(delay, step = 5))
  via_direct <- serialize_transform(vg_bin(delay, step = 5))
  expect_equal(via_formula, via_direct)
})

test_that("an unrecognized function call inside a formula errors clearly", {
  expect_error(serialize_formula(~ log(x)), "vg_bin\\(\\)")
})

test_that("as_spec_payload() serializes a histogram mark using vg_bin()/vg_count()", {
  df <- data.frame(delay = c(1, 2, 2, 3, 3, 3))
  spec <- vg_create() |>
    vg_data(name = "flights", data = df) |>
    vg_mark_rect_y(data_from = "flights", x = ~ vg_bin(delay, step = 1), y = ~ vg_count())

  payload <- as_spec_payload(spec)
  mark <- payload$spec$plot[[1]]

  expect_equal(mark$x, list(bin = "delay", step = 1))
  expect_equal(mark$y, list(count = NULL))
})
