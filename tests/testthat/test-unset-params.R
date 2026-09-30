# A param used for its value that nothing gives a value warns at render and
# export time (warn_unset_params(), R/param.R).

unset_warnings <- function(expr) {
  w <- character()
  withCallingHandlers(expr, warning = function(cnd) {
    if (grepl("nothing gives it a value", conditionMessage(cnd))) w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  w
}
spec_with <- function(...) {
  vg_create() |> vg_data(name = "d", data = data.frame(a = 1:3, b = 4:6, g = c("x", "y", "z"))) |> vg_mark_line(x = ~a, y = ~b, ...)
}

test_that("an R variable holding a param, used as a column name, warns with a hint", {
  g <- param(g)
  s <- spec_with(stroke = ~g, z = ~g)
  w <- unset_warnings(to_json(s))
  expect_length(w, 1)
  expect_match(w, "Param `g` is used by `stroke` and `z` of mark `line`, but nothing gives it a value")
  expect_match(w, "If you meant the column `g`.*`g_p <- param\\(g\\)`")
  # the widget path warns the same way
  expect_length(unset_warnings(as_spec_payload(s)), 1)
})

test_that("a param given a value some other way doesn't warn", {
  expect_length(unset_warnings(to_json(spec_with(stroke = param(p)) |> vg_params(p = "red"))), 0)
  expect_length(unset_warnings(to_json(
    vg_create() |> vg_data(name = "d", data = data.frame(a = 1, b = 2)) |>
      vg_vconcat(vg_slider(as = param(r), min = 1, max = 5), vg_mark_dot(x = ~a, y = ~b, r = param(r)))
  )), 0)
  expect_length(unset_warnings(to_json(
    spec_with(stroke = param(p)) |> vg_params(w = 1) |> vg_computed_param(p = ~ w * 2)
  )), 0)
  # pan/zoom's x =/y = set their selections
  expect_length(unset_warnings(to_json(
    vg_create() |> vg_data(name = "d", data = data.frame(a = 1, b = 2)) |>
      vg_mark_dot(x = ~a, y = ~b, x_domain = param(xs)) |> vg_pan_zoom(x = param(xs))
  )), 0)
})

test_that("an explicit param() warns too, but without the variable-name hint", {
  w <- unset_warnings(to_json(spec_with(stroke = param(p))))
  expect_match(w, "Param `p` is used by `stroke` of mark `line`")
  expect_no_match(w, "If you meant the column")
})

test_that("params inside sql() and in plot attributes count as uses", {
  expect_match(unset_warnings(to_json(spec_with(stroke_width = ~ sql("b * $k")))), "Param `k` is used by `stroke_width` of mark `line`")
  expect_match(unset_warnings(to_json(spec_with() |> vg_plot(x_domain = param(dom)))), "plot attribute `x_domain`")
})

test_that("filter_by isn't checked", {
  expect_length(unset_warnings(to_json(spec_with(filter_by = param(brush)))), 0)
})
