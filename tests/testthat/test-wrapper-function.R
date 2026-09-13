test_that("wrapper_function() drops the fixed argument from the wrapper's formals", {
  f <- function(spec = NULL, which = c("x", "y"), a = 1, ...) NULL
  w <- wrapper_function(f, which = "x")

  expect_equal(names(formals(w)), c("spec", "a", "..."))
  expect_false("which" %in% names(formals(w)))
})

test_that("wrapper_function() preserves the remaining arguments' names, order, and defaults", {
  f <- function(spec = NULL, which = c("x", "y"), a = 1, b = "z", ...) NULL
  w <- wrapper_function(f, which = "y")

  expect_equal(names(formals(w)), c("spec", "a", "b", "..."))
  expect_equal(formals(w)$a, 1)
  expect_equal(formals(w)$b, "z")
})

test_that("wrapper_function() forwards the fixed value and the caller's own arguments", {
  f <- function(spec = NULL, which = c("x", "y"), a = 1, ...) {
    which <- match.arg(which)
    list(spec = spec, which = which, a = a, dots = list(...))
  }
  w <- wrapper_function(f, which = "x")

  expect_equal(w("s", a = 2), list(spec = "s", which = "x", a = 2, dots = list()))
  expect_equal(w()$a, 1)
  expect_equal(w(extra = "raw")$dots, list(extra = "raw"))
})

test_that("wrapper_function() preserves default expressions that reference objects in the wrapped function's environment", {
  w <- wrapper_function(vg_scale_position, which = "x")
  expect_true(identical(formals(w)$type, quote(vg_unset)))
  expect_null(w()$attrs$xScale)
  expect_equal(w(type = "log")$attrs$xScale, "log")
})

test_that("wrapper_function() errors if a fixed name isn't one of the wrapped function's arguments", {
  f <- function(spec = NULL, which = c("x", "y"), a = 1) NULL
  expect_error(wrapper_function(f, bogus = 1), "`bogus`.*is not an argument")
  expect_error(wrapper_function(f, bogus1 = 1, bogus2 = 2), "`bogus1`, `bogus2`.*are not arguments")
})

test_that("wrapper_function() handles a wrapped function with no `...`", {
  f <- function(spec = NULL, which = c("x", "y"), a = 1) list(spec = spec, which = which, a = a)
  w <- wrapper_function(f, which = "y")

  expect_equal(names(formals(w)), c("spec", "a"))
  expect_equal(w("s", a = 5), list(spec = "s", which = "y", a = 5))
})

test_that("vg_scale_x()/vg_scale_y()/vg_scale_fx()/vg_scale_fy() have real formals, not just spec/...", {
  expect_equal(names(formals(vg_scale_x))[1:3], c("spec", "type", "domain"))
  expect_false("which" %in% names(formals(vg_scale_x)))
  expect_equal(names(formals(vg_scale_fx))[1:2], c("spec", "domain"))
  expect_false("type" %in% names(formals(vg_scale_fx)))
})

test_that("vg_guide_x()/vg_guide_y()/vg_guide_fx()/vg_guide_fy() have real formals, not just spec/...", {
  expect_equal(names(formals(vg_guide_x))[1:2], c("spec", "position"))
  expect_false("which" %in% names(formals(vg_guide_x)))
  expect_true("label_arrow" %in% names(formals(vg_guide_x)))
  expect_false("label_arrow" %in% names(formals(vg_guide_fx)))
})

test_that("wrapper_function()'s `drop` removes formals without fixing them", {
  f <- function(spec = NULL, which = c("x", "y"), a = 1, b = 2, ...) {
    which <- match.arg(which)
    list(spec = spec, which = which, a = a, b = b, dots = list(...))
  }
  w <- wrapper_function(f, which = "x", drop = "b")

  expect_false("b" %in% names(formals(w)))
  expect_equal(names(formals(w)), c("spec", "a", "..."))

  # A dropped argument is still reachable through the wrapper's own `...`,
  # and still reaches the wrapped function's real formal of that name.
  expect_equal(w(a = 5, b = 9)$b, 9)
  expect_equal(w()$b, 2)
})

test_that("wrapper_function()'s `drop` errors on a name that isn't a real argument", {
  f <- function(spec = NULL, which = c("x", "y"), a = 1) NULL
  expect_error(wrapper_function(f, which = "x", drop = "bogus"), "`bogus`.*is not an argument")
})

test_that("vg_scale_x()/vg_scale_y()/vg_scale_fx()/vg_scale_fy() drop the inapplicable inset arguments", {
  expect_true(all(c("inset_left", "inset_right") %in% names(formals(vg_scale_x))))
  expect_false(any(c("inset_top", "inset_bottom") %in% names(formals(vg_scale_x))))

  expect_true(all(c("inset_top", "inset_bottom") %in% names(formals(vg_scale_y))))
  expect_false(any(c("inset_left", "inset_right") %in% names(formals(vg_scale_y))))

  expect_true(all(c("inset_left", "inset_right") %in% names(formals(vg_scale_fx))))
  expect_false(any(c("inset_top", "inset_bottom") %in% names(formals(vg_scale_fx))))

  expect_true(all(c("inset_top", "inset_bottom") %in% names(formals(vg_scale_fy))))
  expect_false(any(c("inset_left", "inset_right") %in% names(formals(vg_scale_fy))))
})

test_that("a dropped inset argument still works (and still warns) via vg_scale_x()'s own ...", {
  expect_warning(
    frag <- vg_scale_x(inset_top = 5),
    "`inset_top`.*only applies to the y scale"
  )
  expect_null(frag$attrs$xInsetTop)
})
