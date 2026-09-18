test_that("warn_unrecognized_enum_values() warns on an unrecognized enum value, listing the allowed values", {
  expect_warning(
    warn_unrecognized_enum_values(list(curve = "cardinal_open")),
    "curve.*cardinal_open.*not a recognized value.*basis.*step-before"
  )
})

test_that("warn_unrecognized_enum_values() is silent for a recognized enum value", {
  expect_no_warning(warn_unrecognized_enum_values(list(curve = "natural")))
})

test_that("warn_unrecognized_enum_values() is silent for a non-enum property name", {
  expect_no_warning(warn_unrecognized_enum_values(list(not_a_real_prop = "whatever")))
})

test_that("warn_unrecognized_enum_values() skips a formula/param()/sql() value for an enum property, even an unrecognized one", {
  expect_no_warning(warn_unrecognized_enum_values(list(curve = ~my_curve_col)))
  expect_no_warning(warn_unrecognized_enum_values(list(curve = param("p"))))
  expect_no_warning(warn_unrecognized_enum_values(list(curve = sql("some sql"))))
  expect_no_warning(warn_unrecognized_enum_values(list(curve = vg_bin(x))))
})

test_that("warn_unrecognized_enum_values() skips a non-scalar or non-character value", {
  expect_no_warning(warn_unrecognized_enum_values(list(curve = c("natural", "step"))))
  expect_no_warning(warn_unrecognized_enum_values(list(curve = 1)))
  expect_no_warning(warn_unrecognized_enum_values(list(curve = NA_character_)))
})

test_that("vg_mark_line(curve = <bad>) warns; a valid or non-literal curve doesn't", {
  expect_warning(vg_mark_line(x = ~a, y = ~b, curve = "cardinal_open"), "curve")
  expect_no_warning(vg_mark_line(x = ~a, y = ~b, curve = "natural"))
  expect_no_warning(vg_mark_line(x = ~a, y = ~b, curve = ~my_curve_col))
})

test_that("a layout-level input warns on an unrecognized enum value for its own property", {
  expect_warning(vg_slider(column = "a", select = "bogus"), "select")
  expect_no_warning(vg_slider(column = "a", select = "point"))

  expect_warning(vg_menu(column = "a", list_match = "bogus"), "listMatch")
  expect_no_warning(vg_menu(column = "a", list_match = "any"))
})
