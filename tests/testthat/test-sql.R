test_that("sql() pastes string/param() pieces together with no separator", {
  s <- sql("v + ", param(point))
  expect_s3_class(s, "vg_sql_expr")
  expect_equal(s$key, "sql")
  expect_equal(s$text, "v + $point")
})

test_that("sql() with a plain string containing mosaic's own $name syntax works the same way", {
  expect_equal(sql("v + $point")$text, "v + $point")
})

test_that("agg() uses the agg key instead of sql", {
  a <- agg("SUM(x) + ", param(offset))
  expect_equal(a$key, "agg")
  expect_equal(a$text, "SUM(x) + $offset")
})

test_that("sql()/agg() serialize to {sql: ...}/{agg: ...}, with label when given", {
  expect_equal(serialize_value(sql("v + 1")), list(sql = "v + 1"))
  expect_equal(
    serialize_value(agg("SUM(x)", label = "Total")),
    list(agg = "SUM(x)", label = "Total")
  )
})

test_that("sql()/agg() work as a direct mark encoding value (no formula needed)", {
  spec <- vg_create() |> vg_dot(x = ~a, y = sql("v + ", param(point)))
  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$plot[[1]]$y, list(sql = "v + $point"))
})

test_that("sql()/agg() also work inside a mapping formula", {
  spec <- vg_create() |> vg_dot(x = ~a, y = ~ sql("v + $point"))
  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$plot[[1]]$y, list(sql = "v + $point"))
})

test_that("as_spec_payload() output is identical whether sql() is used directly or inside a formula", {
  direct <- as_spec_payload(vg_create() |> vg_dot(x = ~a, y = sql("v + $point")))
  via_formula <- as_spec_payload(vg_create() |> vg_dot(x = ~a, y = ~ sql("v + $point")))
  expect_equal(direct$spec$plot[[1]]$y, via_formula$spec$plot[[1]]$y)
})
