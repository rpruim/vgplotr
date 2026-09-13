test_that("generated mark wrappers exist for marks beyond the original hand-written set", {
  # vg_mark_dot/vg_mark_line_y/vg_mark_area_y/vg_mark_rect_y already existed by hand; these did
  # not, and should now come from R/marks-generated.R.
  expect_true(exists("vg_mark_hexbin"))
  expect_true(exists("vg_mark_hexgrid"))
  expect_true(exists("vg_mark_text"))

  frag <- vg_mark_hexbin(data_from = "flights", x = ~a, y = ~b, binWidth = 10)
  expect_equal(frag$items[[1]]$mark, "hexbin")
})

test_that("generated mark wrappers use the exact schema mark name, not the R function name", {
  expect_equal(vg_mark_delaunay_link(x = ~a, y = ~b)$items[[1]]$mark, "delaunayLink")
  expect_equal(vg_mark_rule_x(x = ~a)$items[[1]]$mark, "ruleX")
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

test_that("split_plot_args() accepts snake_case plot attribute names and translates them to mosaic's camelCase key", {
  # x_domain/margin_left/aspect_ratio/color_scheme are real mosaic-spec
  # attributes (xDomain/marginLeft/aspectRatio/colorScheme) -- the
  # snake_case form matches every other argument name in the package
  # (marks, interactors, scales, guides).
  split <- split_plot_args(list(
    x = ~a, x_domain = c(0, 10), margin_left = 20, aspect_ratio = 1, color_scheme = "blues"
  ))
  expect_equal(sort(names(split$plot_attrs)), sort(c("xDomain", "marginLeft", "aspectRatio", "colorScheme")))
  expect_equal(names(split$local_args), "x")
})

test_that("split_plot_args() doesn't recognize a genuine typo/unknown name under either spelling", {
  split <- split_plot_args(list(bogus_attr = 1, alsoNotReal = 2))
  expect_length(split$plot_attrs, 0)
  expect_equal(names(split$local_args), c("bogus_attr", "alsoNotReal"))
})

test_that("as_spec_payload() correctly places a previously-unsupported plot attribute at the plot level", {
  # xDomain/colorScheme aren't dot's own properties, so they bubble up to
  # the plot the same way width/name always have.
  spec <- vg_create() |> vg_mark_dot(x = ~a, y = ~b, xDomain = c(0, 100), colorScheme = "blues")
  payload <- as_spec_payload(spec)

  expect_equal(payload$spec$xDomain, c(0, 100))
  expect_equal(payload$spec$colorScheme, "blues")
  expect_null(payload$spec$plot[[1]]$xDomain)
  expect_null(payload$spec$plot[[1]]$colorScheme)
})

test_that("a mark's own property stays on the mark instead of bubbling up, even when a same-named PlotAttributes property exists", {
  # marginLeft/inset/clip/aria* are real per-mark properties *and* separate
  # plot-wide defaults of the same name (mosaic-spec disambiguates by
  # where the key appears, not by name) -- split_plot_args() has to keep
  # an explicit `marginLeft =`/`inset =` on the mark that declared it.
  spec <- vg_create() |> vg_mark_rect_y(x = ~a, y1 = ~b, y2 = ~c, inset = 1, marginLeft = 40)
  payload <- as_spec_payload(spec)

  expect_equal(payload$spec$plot[[1]]$inset, 1)
  expect_equal(payload$spec$plot[[1]]$marginLeft, 40)
  expect_null(payload$spec$inset)
  expect_null(payload$spec$marginLeft)
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

test_that("generated mark wrappers make a genuinely schema-required property a real R argument, not vg_unset", {
  # ErrorBarX/ErrorBarY require x/y respectively (beyond the mark/data the
  # generator always strips) -- these should fail fast in R rather than
  # silently produce a spec with no x/y encoding.
  expect_true("x" %in% names(formals(vg_mark_errorbar_x)))
  expect_false(identical(formals(vg_mark_errorbar_x)$x, quote(vg_unset)))
  expect_error(vg_mark_errorbar_x(), "argument \"x\" is missing")

  expect_true("y" %in% names(formals(vg_mark_errorbar_y)))
  expect_error(vg_mark_errorbar_y(), "argument \"y\" is missing")

  frag <- vg_mark_errorbar_x(x = ~a)
  expect_equal(frag$items[[1]]$mark, "errorbarX")
  expect_equal(frag$items[[1]]$encodings$x, ~a)
})

test_that("generated mark/interactor wrappers have snake_case formals forwarded under the exact schema key", {
  expect_true("stroke_width" %in% names(formals(vg_mark_dot)))
  expect_false("strokeWidth" %in% names(formals(vg_mark_dot)))

  frag <- vg_mark_rule_x(x = 0, stroke_width = 2)
  expect_equal(frag$items[[1]]$encodings$strokeWidth, 2)
  expect_null(frag$items[[1]]$encodings$stroke_width)

  expect_true("filter_by" %in% names(formals(vg_search)))
  input <- vg_search(column = "name", filter_by = param(brush))
  expect_true(is_vg_param(input$options$filterBy))
})

test_that("a required property that's really a branch discriminant with its own mosaic default stays optional", {
  # densityX/densityY's `type` is required in every anyOf branch, but with
  # a *different* const per branch (areaX/lineX/dotX/textX) -- mosaic
  # documents this as defaulting to areaX, so it should stay vg_unset
  # rather than becoming a hard-required argument.
  expect_identical(formals(vg_mark_density_x)$type, quote(vg_unset))
  expect_identical(formals(vg_mark_density_y)$type, quote(vg_unset))

  frag <- vg_mark_density_x(x = ~a)
  expect_equal(frag$items[[1]]$mark, "densityX")
})
