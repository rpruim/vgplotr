test_that("generated mark wrappers exist for marks beyond the original hand-written set", {
  # vg_dot/vg_line_y/vg_area_y/vg_rect_y already existed by hand; these did
  # not, and should now come from R/marks-generated.R.
  expect_true(exists("vg_hexbin"))
  expect_true(exists("vg_hexgrid"))
  expect_true(exists("vg_text"))

  frag <- vg_hexbin(data_from = "flights", x = ~a, y = ~b, binWidth = 10)
  expect_equal(frag$items[[1]]$mark, "hexbin")
})

test_that("generated mark wrappers use the exact schema mark name, not the R function name", {
  expect_equal(vg_delaunay_link(x = ~a, y = ~b)$items[[1]]$mark, "delaunayLink")
  expect_equal(vg_rule_x(x = ~a)$items[[1]]$mark, "ruleX")
})

test_that("generated interactor/input wrappers exist for types beyond the original hand-written set", {
  expect_true(exists("vg_pan_zoom_x"))
  expect_true(exists("vg_region"))
  expect_true(exists("vg_toggle_color"))
  expect_true(exists("vg_nearest_x"))

  input <- vg_pan_zoom_x(NULL, as = param(brush))
  expect_equal(input$items[[1]]$type, "panZoomX")
})

test_that("vg_interactor_placement() classifies generated types correctly", {
  expect_equal(vg_interactor_placement("panZoomX"), "plot")
  expect_equal(vg_interactor_placement("region"), "plot")
  expect_equal(vg_interactor_placement("toggleColor"), "plot")
  expect_equal(vg_interactor_placement("menu"), "layout")
  expect_error(vg_interactor_placement("not_a_real_type"), "Unknown interactor type")
})

test_that("split_plot_args() recognizes real (camelCase) plot attributes missing from the old hand-written list", {
  split <- split_plot_args(list(x = ~a, xDomain = c(0, 10), marginLeft = 20, colorScheme = "blues"))
  expect_equal(sort(names(split$plot_attrs)), sort(c("xDomain", "marginLeft", "colorScheme")))
  expect_equal(names(split$local_args), "x")
})

test_that("split_plot_args() no longer recognizes the old (incorrect) snake_case attribute names", {
  # margin_left/aspect_ratio were never real mosaic-spec keys -- they're
  # mark-local (unrecognized) args now, same as any other typo would be.
  split <- split_plot_args(list(margin_left = 20, aspect_ratio = 1))
  expect_length(split$plot_attrs, 0)
  expect_equal(names(split$local_args), c("margin_left", "aspect_ratio"))
})

test_that("as_spec_payload() correctly places a previously-unsupported plot attribute at the plot level", {
  spec <- vg_create() |> vg_dot(x = ~a, y = ~b, xDomain = c(0, 100), marginLeft = 40)
  payload <- as_spec_payload(spec)

  expect_equal(payload$spec$xDomain, c(0, 100))
  expect_equal(payload$spec$marginLeft, 40)
  expect_null(payload$spec$plot[[1]]$xDomain)
  expect_null(payload$spec$plot[[1]]$marginLeft)
})

test_that("a mark/interactor's own property can be set under its exact schema name, even when that name would otherwise collide with vg_mark()/vg_interactor()'s own discriminant parameter", {
  # vg_search() has its own "type" option (query mode: contains/prefix/
  # suffix/regexp) distinct from the interactor/input type ("search").
  # vg_mark()/vg_interactor() are named `mark`/`interactor` (not `type`)
  # specifically so this doesn't collide -- see their documentation.
  input <- vg_search(type = "prefix", column = "name")
  expect_equal(input$type, "search")
  expect_equal(input$options$type, "prefix")
  expect_equal(input$options$column, "name")

  # Same guarantee calling the generic constructor directly.
  input2 <- vg_interactor(NULL, "search", type = "prefix", column = "name")
  expect_equal(input2$type, "search")
  expect_equal(input2$options$type, "prefix")
})
