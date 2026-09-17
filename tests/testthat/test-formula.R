test_that("formula shorthand with a one-sided facet matches the equivalent explicit args", {
  df <- data.frame(Sepal.Length = 1, Sepal.Width = 2, Species = "a")

  by_formula <- vg_create() |>
    vg_data(name = "d", data = df) |>
    vg_mark_dot(Sepal.Length ~ Sepal.Width | ~ Species, data_from = "d", fill = ~Species)
  by_hand <- vg_create() |>
    vg_data(name = "d", data = df) |>
    vg_mark_dot(y = ~Sepal.Length, x = ~Sepal.Width, fx = ~Species, data_from = "d", fill = ~Species)

  pf <- as_spec_payload(by_formula)$spec$plot[[1]]
  ph <- as_spec_payload(by_hand)$spec$plot[[1]]
  expect_equal(pf[order(names(pf))], ph[order(names(ph))])
})

test_that("formula shorthand with a two-sided facet matches the equivalent explicit args", {
  df <- data.frame(Sepal.Length = 1, Sepal.Width = 2, Species = "a")

  by_formula <- vg_create() |>
    vg_data(name = "d", data = df) |>
    vg_mark_dot(Sepal.Length ~ Sepal.Width | Species ~ ., data_from = "d", fill = ~Species)
  by_hand <- vg_create() |>
    vg_data(name = "d", data = df) |>
    vg_mark_dot(y = ~Sepal.Length, x = ~Sepal.Width, fy = ~Species, data_from = "d", fill = ~Species)

  pf <- as_spec_payload(by_formula)$spec$plot[[1]]
  ph <- as_spec_payload(by_hand)$spec$plot[[1]]
  expect_equal(pf[order(names(pf))], ph[order(names(ph))])
})

test_that("a bare facet term (no ~) is equivalent to | ~ facet (fx only)", {
  spec <- vg_create() |> vg_mark_dot(mpg ~ hp | cyl)
  p <- as_spec_payload(spec)$spec$plot[[1]]
  expect_equal(p$x, "hp")
  expect_equal(p$y, "mpg")
  expect_equal(p$fx, "cyl")
  expect_null(p$fy)
})

test_that("two-sided facet with both sides set works (fy and fx together)", {
  spec <- vg_create() |> vg_mark_dot(mpg ~ hp | cyl ~ gear)
  p <- as_spec_payload(spec)$spec$plot[[1]]
  expect_equal(p$fy, "cyl")
  expect_equal(p$fx, "gear")
})

test_that("paired channels (y1+y2 ~ x1+x2) are supported for marks that have them", {
  spec <- vg_create() |> vg_mark_rect(mpg + wt ~ hp + disp)
  p <- as_spec_payload(spec)$spec$plot[[1]]
  expect_equal(p$y1, "mpg")
  expect_equal(p$y2, "wt")
  expect_equal(p$x1, "hp")
  expect_equal(p$x2, "disp")
})

test_that("asymmetric term counts map each side independently", {
  spec <- vg_create() |> vg_mark_rect(mpg ~ hp + disp)
  p <- as_spec_payload(spec)$spec$plot[[1]]
  expect_equal(p$y, "mpg")
  expect_equal(p$x1, "hp")
  expect_equal(p$x2, "disp")
  expect_null(p$x)
})

test_that("a dot placeholder skips that side/channel", {
  spec_x <- vg_create() |> vg_mark_dot(mpg ~ .)
  p_x <- as_spec_payload(spec_x)$spec$plot[[1]]
  expect_equal(p_x$y, "mpg")
  expect_null(p_x$x)

  spec_y <- vg_create() |> vg_mark_dot(. ~ hp)
  p_y <- as_spec_payload(spec_y)$spec$plot[[1]]
  expect_equal(p_y$x, "hp")
  expect_null(p_y$y)

  spec_one_sided <- vg_create() |> vg_mark_dot(~hp)
  p_one <- as_spec_payload(spec_one_sided)$spec$plot[[1]]
  expect_equal(p_one$x, "hp")
  expect_null(p_one$y)
})

test_that("parens keep a term opaque instead of splitting it on + or |", {
  parsed <- parse_vg_formula(mpg ~ (hp + wt), "dot")
  expect_equal(parsed$y, ~mpg)
  expect_equal(parsed$x, ~(hp + wt))
})

test_that("an explicit channel arg wins over the formula, with a warning only on real conflict", {
  expect_warning(
    {
      spec <- vg_create() |> vg_mark_dot(mpg ~ hp, x = ~wt)
      p <- as_spec_payload(spec)$spec$plot[[1]]
      expect_equal(p$x, "wt")
    },
    "overrides"
  )

  expect_no_warning(
    vg_create() |> vg_mark_dot(mpg ~ hp, x = ~hp)
  )
})

test_that("a formula implying an unsupported channel for the mark errors clearly", {
  expect_error(
    vg_create() |> vg_mark_dot(mpg + wt ~ hp + disp),
    "doesn't support"
  )
})
