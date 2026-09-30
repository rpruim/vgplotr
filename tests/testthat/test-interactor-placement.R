# An interactor uses the mark just before it in its plot, and selects on that
# mark's data column(s) for the channel(s) it needs (R/interactor.R).

placement_warnings <- function(expr) {
  w <- character()
  withCallingHandlers(expr, warning = function(cnd) {
    if (grepl("to select on|no mark before it", conditionMessage(cnd))) w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  w
}

test_that("an interval after a mark without that channel warns, pointing to a better mark", {
  w <- placement_warnings(
    vg_mark_dot(x = ~hp, y = ~mpg) |> vg_mark_rule_y(y = 20) |> vg_interval_x(as = param(brush))
  )
  expect_length(w, 1)
  expect_match(w, "vg_interval_x\\(\\) uses the mark just before it.*vg_mark_rule_y\\(\\).*`x`")
  expect_match(w, "right after vg_mark_dot\\(\\)")
})

test_that("an interactor with no mark before it warns", {
  expect_match(placement_warnings(vg_interval_x(as = param(b))), "no mark before it")
  expect_match(placement_warnings(vg_create() |> vg_toggle_x(as = param(b))), "no mark before it")
})

test_that("correctly placed interactors don't warn", {
  expect_length(placement_warnings(vg_mark_dot(x = ~hp, y = ~mpg) |> vg_interval_x(as = param(b))), 0)
  expect_length(placement_warnings(vg_mark_dot(x = ~hp, y = ~mpg) |> vg_interval_xy(as = param(b))), 0)
  # x1/x2 count for an interval, which matches any channel starting with "x"
  expect_length(placement_warnings(vg_mark_rect_y(x1 = ~a, x2 = ~b, y = vg_count()) |> vg_interval_x(as = param(b))), 0)
  expect_length(placement_warnings(vg_mark_bar_x(x = vg_count(), y = ~g) |> vg_toggle_y(as = param(b))), 0)
  # a toggle's `color` is satisfied by fill or stroke
  expect_length(placement_warnings(vg_mark_dot(x = ~a, y = ~b, fill = ~g) |> vg_toggle_color(as = param(b))), 0)
  # other interactors in between don't count as marks
  expect_length(placement_warnings(
    vg_mark_dot(x = ~a, y = ~b) |> vg_interval_x(as = param(b)) |> vg_highlight(by = param(b))
  ), 0)
})

test_that("a constant or param channel doesn't count, a SQL or string column does", {
  expect_match(placement_warnings(vg_mark_dot(x = 5, y = ~b) |> vg_interval_x(as = param(b))), "`x`")
  expect_match(placement_warnings(vg_mark_dot(x = param(p), y = ~b) |> vg_interval_x(as = param(b))), "`x`")
  expect_length(placement_warnings(vg_mark_dot(x = sql("a + 1"), y = ~b) |> vg_interval_x(as = param(b))), 0)
  expect_length(placement_warnings(vg_mark_dot(x = "a", y = ~b) |> vg_interval_x(as = param(b))), 0)
  # a string Mosaic reads as a constant is not a column
  expect_match(placement_warnings(vg_mark_dot(x = ~a, fill = "red") |> vg_toggle_color(as = param(b))), "`color`")
})

test_that("options that supply the column skip the check, and `channels` sets what is checked", {
  expect_length(placement_warnings(vg_mark_rule_y(y = 1) |> vg_interval_x(as = param(b), field = "hp")), 0)
  expect_length(placement_warnings(vg_mark_rule_y(y = 1) |> vg_interval_xy(as = param(b), xfield = "a", yfield = "b")), 0)
  expect_match(placement_warnings(vg_mark_rule_y(y = ~a) |> vg_interval_xy(as = param(b), yfield = "b")), "`x`")
  expect_length(placement_warnings(vg_mark_dot(x = ~a, y = ~b, fill = ~g) |> vg_toggle(as = param(b), channels = "fill")), 0)
  expect_match(placement_warnings(vg_mark_dot(x = ~a, y = ~b) |> vg_toggle(as = param(b), channels = "fill")), "`fill`")
  expect_length(placement_warnings(vg_mark_rule_y(y = 1) |> vg_nearest_x(as = param(b), fields = list("a"))), 0)
})

test_that("the generic constructor names the interactor and mark the way it's called", {
  w <- placement_warnings(vg_mark(NULL, "ruleY", y = 1) |> vg_interactor("intervalX", as = param(b)))
  expect_match(w, 'vg_interactor\\("intervalX"\\) uses the mark just before it.*vg_mark\\("ruleY"\\)')
})
