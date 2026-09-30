test_that("a selection or date-param definition used as a reference is a clear error", {
  sel <- vg_selection("union")
  expect_error(vg_menu(as = sel, column = "a"), "`as` \\(in input `menu`\\) is a selection \\*definition\\*.*vg_params\\(name = vg_selection")
  expect_error(vg_mark_line(x = ~a, filter_by = sel), "`filter_by` \\(in mark `line`\\)")
  expect_error(vg_mark_dot(x = ~a) |> vg_interval_x(as = sel), "`as` \\(in interactor `intervalX`\\)")
  expect_error(vg_legend_color(as = sel), "`as` \\(in legend `color`\\)")
  expect_error(vg_mark_dot(x = ~a, fill = vg_param_date("2020-01-01")), "is a date param \\*definition\\*.*vg_param_date")
  # anywhere else, serialization says so too
  expect_error(to_json(vg_mark_dot(x = ~a) |> vg_plot(x_domain = sel)), "selection \\*definition\\*")
  # ...while in vg_params() they're what's wanted
  expect_no_error(to_json(vg_create() |> vg_params(chosen = sel, day = vg_param_date("2020-01-01")) |> vg_mark_dot(x = ~a)))
})
