# check_transform_calls() (R/transforms.R) rejects a misspelled transform option
# or function name when the mark is built, instead of at render/export time.

built_error <- function(expr) {
  tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
}

test_that("a misspelled transform option fails when the mark is built, naming the argument", {
  msg <- built_error(vg_mark_dot(x = ~vg_bin(delay, stp = 10), y = ~vg_count()))
  expect_match(msg, "In `vg_bin\\(\\)`: `stp` is not an argument of this transform\\. Did you perhaps mean `step`\\?")
  expect_match(msg, "\\(in the `x` argument of mark `dot`\\)$")
})

test_that("the error points at the offending call in a longer pipeline", {
  build <- function() {
    data.frame(delay = 1:3) |>
      vg_mark_dot(x = ~delay, y = ~vg_count()) |>
      vg_mark_line_y(x = ~vg_bin(delay, nce = TRUE), y = ~vg_count())
  }
  msg <- built_error(build())
  expect_match(msg, "`nce` is not an argument.*`nice`")
  expect_match(msg, "argument of mark `lineY`")   # the second mark, not the first
})

test_that("a misspelled option in a nested transform is caught, and located by its outer argument", {
  msg <- built_error(vg_mark_dot(x = ~delay, y = ~vg_avg(vg_bin(delay, stpe = 1))))
  expect_match(msg, "In `vg_bin\\(\\)`: `stpe` is not an argument.*`step`")
  expect_match(msg, "in the `y` argument of mark `dot`")
})

test_that("an unknown function name in a formula fails at construction, with a suggestion", {
  msg <- built_error(vg_mark_dot(x = ~vg_bn(delay), y = ~vg_count()))
  expect_match(msg, "Got a call to `vg_bn\\(\\)`\\. Did you perhaps mean `vg_bin\\(\\)`\\?")
  expect_match(msg, "in the `x` argument of mark `dot`")
  expect_match(built_error(vg_mark_dot(x = ~bin(delay), y = ~vg_count())), "Did you perhaps mean `vg_bin\\(\\)`\\?")
})

test_that("arithmetic and other calls in a formula are rejected at construction", {
  expect_match(built_error(vg_mark_dot(x = ~a + b, y = ~vg_count())), "Got a call to `\\+\\(\\)`")
  expect_match(built_error(vg_mark_dot(x = ~log(a), y = ~vg_count())), "Got a call to `log\\(\\)`")
})

test_that("a namespaced call gets a readable message (it used to fail with 'condition has length > 1')", {
  expect_match(built_error(vg_mark_dot(x = ~stats::sd(a), y = ~vg_count())), "Got a call to `stats::sd\\(\\)`")
  expect_match(built_error(serialize_unchecked(x = ~stats::sd(delay))), "Got a call to `stats::sd\\(\\)`")
})

test_that("a formula passed through the `formula =` shorthand is checked too", {
  expect_match(built_error(vg_mark_dot(formula = ~vg_bn(delay))), "Got a call to `vg_bn\\(\\)`")
})

test_that("an already-built transform object is checked, including transforms nested in its fields", {
  # vg_avg() captures its field unevaluated, so building this object succeeds;
  # the typo inside is only visible by looking into the field
  expect_no_error(vg_avg(vg_bin(delay, stp = 1)))
  msg <- built_error(vg_mark_dot(x = vg_avg(vg_bin(delay, stp = 1)), y = ~vg_count()))
  expect_match(msg, "In `vg_bin\\(\\)`: `stp` is not an argument.*`step`")
  expect_match(msg, "in the `x` argument of mark `dot`")
})

test_that("interactors, inputs and legends check their formula options too", {
  expect_match(
    built_error(vg_interval_x(as = param(s), pixel_size = ~vg_bin(a, stp = 1))),
    "`stp` is not an argument.*in the `pixelSize` argument of interactor `intervalX`"
  )
  expect_match(
    built_error(vg_slider(column = "a", label = ~vg_bin(a, stp = 1))),
    "in the `label` argument of input `slider`"
  )
  expect_match(
    built_error(vg_legend_color(label = ~vg_bin(a, stp = 1))),
    "in the `label` argument of legend `color`"
  )
})

test_that("valid formulas still build: columns, transforms with options, partial names, nesting, sql/agg/param, literals", {
  expect_no_error(vg_mark_dot(x = ~delay, y = ~value))
  expect_no_error(vg_mark_dot(x = ~vg_bin(delay, step = 10), y = ~vg_count()))
  expect_no_error(vg_mark_dot(x = ~vg_bin(delay, inter = "day"), y = ~vg_count()))   # unambiguous partial name
  expect_no_error(vg_mark_dot(x = ~delay, y = ~vg_avg(vg_bin(delay, step = 1))))
  expect_no_error(vg_mark_dot(x = ~sql("a + b"), y = ~agg("count(*)")))
  expect_no_error(vg_mark_dot(x = ~5, y = ~"a"))
  expect_no_error(vg_mark_dot(x = "a", y = 1, fill = "red"))
  expect_no_error(vg_mark_dot(x = ~vg_bin(delay, step = 10), y = ~vg_count(distinct = TRUE)))
})

test_that("the early check never evaluates anything, so a variable defined later doesn't trip it", {
  # a param held in a variable that doesn't exist yet when the mark is built...
  f <- ~vg_column(xp_defined_later)
  expect_no_error(vg_mark_dot(x = f, y = ~value))
  # ...and a sql() whose argument refers to one
  expect_no_error(vg_mark_dot(x = ~sql(paste0("a", suffix_defined_later)), y = ~value))
})

test_that("a plain mark with no formulas at all is unaffected", {
  expect_no_error(vg_mark_dot(x = "a", y = 1))
  expect_no_error(vg_mark_rule_x(x = 0))
})

test_that("the render/export-time check is still there as a safety net", {
  expect_error(serialize_unchecked(x = ~vg_bin(delay, stp = 10)), "In `vg_bin\\(\\)`: `stp` is not an argument")
  expect_error(serialize_unchecked(x = ~vg_bn(delay)), "Got a call to `vg_bn\\(\\)`")
  # ...and its message has no location suffix (there's no argument to name there)
  expect_no_match(built_error(serialize_unchecked(x = ~vg_bin(delay, stp = 10))), "in the `x` argument")
})

# --- the property that makes an early *error* safe --------------------------

outcome <- function(expr) if (is.null(built_error(expr))) "ok" else "error"

test_that("the early check and real serialization agree on every kind of formula expression", {
  # If the early check ever rejected something serialization accepts, a valid
  # spec would be blocked; if it accepted something serialization rejects, the
  # bug would just come back later. Both directions are checked.
  exprs <- list(
    quote(delay), quote(vg_bin(delay)), quote(vg_bin(delay, step = 10)), quote(vg_bin(delay, stp = 10)),
    quote(vg_bin(delay, inter = "day")), quote(vg_bin(delay, s = 1)),   # `s` is ambiguous (step/steps): R's own error
    quote(vg_count()), quote(vg_count(distinct = TRUE)), quote(vg_count(dstinct = TRUE)),
    quote(vg_avg(delay)), quote(vg_avg(vg_bin(delay))), quote(vg_avg(vg_bin(delay, stp = 1))),
    quote(vg_quantile(delay, 0.5)), quote(vg_quantile(delay, 0.5, 7, 8, 9, 10, 11, 12)),
    quote(vg_argmax(delay, other)), quote(vg_argmax(delay, other, oderby = "a")),
    quote(sql("a + b")), quote(agg("count(*)")), quote(param(p)),
    quote(a + b), quote(log(a)), quote(stats::sd(a)), quote(vg_bn(delay)), quote(bin(delay)), quote((delay)),
    quote(vg_avg(log(delay))), quote(vg_bin(vg_bin(delay))), quote(vg_lag(delay, 1, 0)),
    5, "a", TRUE, NULL
  )
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    expect_identical(
      outcome(check_formula_expr(e)),
      outcome(serialize_expr(e, environment())),
      info = paste("expression", i, ":", paste(deparse(e), collapse = ""))
    )
  }
})

test_that("the early check and serialization agree for every transform, with valid and misspelled options", {
  for (nm in names(vg_transform_specs)) {
    spec <- vg_transform_specs[[nm]]
    fields <- lapply(seq_along(spec$field_names), function(i) as.name(paste0("f", i)))
    valid <- as.call(c(list(as.name(nm)), fields))
    with_option <- if (length(spec$option_names)) {
      opt <- stats::setNames(list(TRUE), spec$option_names[[1]])
      as.call(c(list(as.name(nm)), fields, opt))
    }
    typo <- if (length(spec$option_names)) {
      opt <- stats::setNames(list(TRUE), paste0(spec$option_names[[1]], "_zz"))
      as.call(c(list(as.name(nm)), fields, opt))
    }
    too_many <- as.call(c(list(as.name(nm)), fields, as.list(1:20)))
    for (e in Filter(Negate(is.null), list(valid, with_option, typo, too_many))) {
      expect_identical(
        outcome(check_formula_expr(e)),
        outcome(serialize_expr(e, environment())),
        info = paste(nm, ":", paste(deparse(e), collapse = ""))
      )
    }
    expect_equal(outcome(check_formula_expr(valid)), "ok", info = nm)
    if (!is.null(typo)) expect_equal(outcome(check_formula_expr(typo)), "error", info = nm)
  }
})
