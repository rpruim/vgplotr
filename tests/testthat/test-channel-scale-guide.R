test_that("vg_scale_color() translates snake_case args to the raw mosaic attrs", {
  frag <- vg_mark_dot(x = ~a, y = ~b, fill = ~g) |>
    vg_scale_color(type = "linear", scheme = "Viridis", pivot = 0, symmetric = TRUE, n = 5)

  expect_equal(frag$attrs$colorScale, "linear")
  expect_equal(frag$attrs$colorScheme, "Viridis")
  expect_equal(frag$attrs$colorPivot, 0)
  expect_true(frag$attrs$colorSymmetric)
  expect_equal(frag$attrs$colorN, 5)
})

test_that("vg_scale_opacity() translates snake_case args to the raw mosaic attrs", {
  frag <- vg_scale_opacity(range = c(0.2, 1), zero = TRUE, reverse = TRUE)

  expect_equal(frag$attrs$opacityRange, c(0.2, 1))
  expect_true(frag$attrs$opacityZero)
  expect_true(frag$attrs$opacityReverse)
})

test_that("vg_scale_r()/vg_scale_radius() are identical and have no `reverse` argument", {
  expect_identical(vg_scale_r, vg_scale_radius)
  expect_false("reverse" %in% names(formals(vg_scale_r)))

  frag <- vg_scale_r(range = c(0, 20), zero = TRUE)
  expect_equal(frag$attrs$rRange, c(0, 20))
  expect_true(frag$attrs$rZero)
})

test_that("vg_scale_color()/vg_scale_opacity()/vg_scale_r() warn about unrecognized attributes", {
  expect_warning(vg_scale_color(bogus = 1), "`bogus`.*is not a recognized mosaic-spec plot attribute")
  expect_warning(vg_scale_opacity(bogus = 1), "`bogus`.*is not a recognized mosaic-spec plot attribute")
  expect_warning(vg_scale_r(bogus = 1), "`bogus`.*is not a recognized mosaic-spec plot attribute")
})

test_that("vg_guide_color()/vg_guide_opacity() set label/tick_format", {
  frag <- vg_mark_dot(x = ~a, y = ~b, fill = ~g, opacity = ~o) |>
    vg_guide_color(label = "Group", tick_format = "d") |>
    vg_guide_opacity(label = "Weight", tick_format = ".0%")

  expect_equal(frag$attrs$colorLabel, "Group")
  expect_equal(frag$attrs$colorTickFormat, "d")
  expect_equal(frag$attrs$opacityLabel, "Weight")
  expect_equal(frag$attrs$opacityTickFormat, ".0%")
})

test_that("vg_guide_r()/vg_guide_radius() are identical and have no `tick_format` argument", {
  expect_identical(vg_guide_r, vg_guide_radius)
  expect_false("tick_format" %in% names(formals(vg_guide_r)))

  frag <- vg_guide_r(label = "Size")
  expect_equal(frag$attrs$rLabel, "Size")
})

test_that("vg_guide_color()/vg_guide_opacity()/vg_guide_r() are distinct from vg_legend_color()/vg_legend_opacity()", {
  guide_frag <- vg_guide_color(label = "Group")
  expect_equal(guide_frag$attrs$colorLabel, "Group")
  expect_equal(length(guide_frag$items), 0)

  legend_frag <- vg_legend_color(label = "Group")
  expect_equal(length(legend_frag$items), 1)
  expect_s3_class(legend_frag$items[[1]], "vg_legend")
})

test_that("vg_guide_color()/vg_guide_opacity()/vg_guide_r() warn about unrecognized attributes", {
  expect_warning(vg_guide_color(bogus = 1), "`bogus`.*is not a recognized mosaic-spec plot attribute")
  expect_warning(vg_guide_opacity(bogus = 1), "`bogus`.*is not a recognized mosaic-spec plot attribute")
  expect_warning(vg_guide_r(bogus = 1), "`bogus`.*is not a recognized mosaic-spec plot attribute")
})

test_that("vg_scale_length() sets the length channel's scale properties (no guide/legend counterpart exists)", {
  frag <- vg_mark_vector(x = ~a, y = ~b, length = ~g) |>
    vg_scale_length(range = c(0, 20), zero = TRUE)

  expect_equal(frag$attrs$lengthRange, c(0, 20))
  expect_true(frag$attrs$lengthZero)
  expect_false(exists("vg_guide_length"))
  expect_false(exists("vg_legend_length"))
})

test_that("vg_scale_symbol() only has type/domain/range (no nice/zero/reverse/etc.)", {
  frag <- vg_mark_dot(x = ~a, y = ~b, symbol = ~g) |>
    vg_scale_symbol(range = c("circle", "square"))

  expect_equal(frag$attrs$symbolRange, c("circle", "square"))
  expect_setequal(setdiff(names(formals(vg_scale_symbol)), c("spec", "...")), c("type", "domain", "range"))
})
