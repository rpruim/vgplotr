test_that("vg_config() sets the spec's top-level config, serialized by to_json()/to_yaml()", {
  spec <- vg_create() |> vg_config(extensions = "spatial") |> vg_mark_dot(x = ~a)

  expect_equal(spec$config, list(extensions = "spatial"))

  json <- to_json(spec)
  expect_match(as.character(json), '"config":\\s*\\{\\s*"extensions":\\s*"spatial"', perl = TRUE)
})

test_that("vg_config() is included in vg_render()'s payload", {
  spec <- vg_create() |> vg_config(extensions = "spatial") |> vg_mark_dot(x = ~a)
  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$config, list(extensions = "spatial"))
})

test_that("a vgspec with no vg_config() call has no config key at all", {
  spec <- vg_create() |> vg_mark_dot(x = ~a)
  expect_false("config" %in% names(as_spec_payload(spec)$spec))
})
