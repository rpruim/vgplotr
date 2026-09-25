# nearest and toggleZ are accepted by mosaic's runtime parser but missing
# from its JSON schema; the generator adds them (data-raw/update-schema.R).

test_that("vg_nearest() and vg_toggle_z() exist and are interactors", {
  expect_true(all(c("nearest", "toggleZ") %in% .vg_interactor_types))
  expect_equal(vg_interactor_placement("nearest"), "plot")
  expect_equal(vg_interactor_placement("toggleZ"), "plot")
})

test_that("vg_nearest() takes exactly nearestX's options, in snake_case", {
  expect_setequal(.vg_interactor_own_props$nearest, .vg_interactor_own_props$nearestX)
  expect_true(all(c("as", "channels", "fields", "max_radius") %in% names(formals(vg_nearest))))
})

test_that("vg_toggle_z() takes exactly toggleY's options", {
  expect_setequal(.vg_interactor_own_props$toggleZ, .vg_interactor_own_props$toggleY)
  expect_true(all(c("as", "peers") %in% names(formals(vg_toggle_z))))
})

test_that("they serialize as the select value mosaic's parser expects", {
  spec <- vg_create() |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_nearest(as = param(hover), max_radius = 20) |>
    vg_toggle_z(as = param(pick))
  out <- spec_to_list(spec)$plot
  expect_equal(out[[2]], list(select = "nearest", as = "$hover", maxRadius = 20))
  expect_equal(out[[3]], list(select = "toggleZ", as = "$pick"))
})

test_that("the generic constructor accepts them too, and still warns on a bad option", {
  expect_no_warning(vg_interactor(interactor = "nearest", as = param(h)))
  expect_warning(vg_interactor(interactor = "toggleZ", as = param(h), bogus = 1), "not a property")
  expect_warning(vg_nearest(as = param(h), maxradius = 3), "max_radius")
})
