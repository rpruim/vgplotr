test_that("compatible plot-level attributes from sibling marks accumulate silently", {
  frag <- vg_dot(x = ~a, y = ~b, width = 680) |>
    vg_line_y(x = ~a, y = ~c, height = 200)

  expect_length(frag$items, 2)
  expect_equal(frag$attrs$width, 680)
  expect_equal(frag$attrs$height, 200)
})

test_that("identical repeated plot-level attributes don't warn", {
  expect_no_warning(
    vg_dot(x = ~a, y = ~b, width = 680) |> vg_line_y(x = ~a, y = ~c, width = 680)
  )
})

test_that("conflicting plot-level attributes warn and the later value wins", {
  expect_warning(
    frag <- vg_dot(x = ~a, y = ~b, width = 400) |> vg_line_y(x = ~a, y = ~c, width = 680),
    "Conflicting value for `width`"
  )
  expect_equal(frag$attrs$width, 680)
})

test_that("vg_plot() wrapping children is equivalent to piping them", {
  wrapped <- vg_plot(vg_dot(x = ~a, y = ~b), vg_line_y(x = ~a, y = ~c), width = 680)
  piped <- vg_dot(x = ~a, y = ~b) |> vg_line_y(x = ~a, y = ~c) |> vg_plot(width = 680)

  expect_equal(wrapped, piped)
})

test_that("mark encodings don't leak into plot-level attrs", {
  frag <- vg_dot(x = ~a, y = ~b, width = 680)
  expect_equal(frag$items[[1]]$encodings, list(x = ~a, y = ~b))
  expect_equal(frag$attrs, list(width = 680))
})
