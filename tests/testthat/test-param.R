test_that("param() captures a bare name and formats as $name", {
  p <- param(brush)
  expect_s3_class(p, "vg_param")
  expect_equal(p$name, "brush")
  expect_equal(format(p), "$brush")
})
