# A suggested argument name is spelled the way the function that was called
# accepts it: snake_case (`fill_opacity`) for the generated vg_mark_*() and
# interactor/input wrappers, which document snake_case, and for legends
# (`vg_legend()`/`vg_legend_color()`/etc.), which also accept snake_case
# (translated to mosaic's exact key via canonicalize_legend_prop_names());
# mosaic's exact key (`fillOpacity`) for the generic vg_mark()/
# vg_interactor(), which accept only that. See build_mark() in R/mark.R and
# spell_names() in R/utils.R.

unrecognized_warnings <- function(expr) {
  w <- character()
  withCallingHandlers(
    tryCatch(expr, error = function(e) NULL),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  w
}

# The names in the "Did you perhaps mean ...?" part of a warning.
suggested_names <- function(message) {
  part <- sub("^.*?[Dd]id you perhaps mean ", "", message)
  gsub("`", "", regmatches(part, gregexpr("`[^`]+`", part))[[1]])
}

is_unrecognized_warning <- function(w) grepl("is not a property of|are not properties of", w)

# --- the guarantee that makes snake_case safe to suggest ---------------------

test_that("camel_to_snake() gives the argument names the generator gave the wrappers", {
  expect_equal(camel_to_snake("strokeWidth"), "stroke_width")
  expect_equal(camel_to_snake("fillOpacity"), "fill_opacity")
  expect_equal(camel_to_snake("frameAnchor"), "frame_anchor")
  expect_equal(camel_to_snake("x1"), "x1")
  expect_equal(camel_to_snake("data_from"), "data_from")          # already snake: unchanged
  expect_equal(camel_to_snake(c("marginLeft", "fill")), c("margin_left", "fill"))
})

test_that("every property of every mark has a snake_case argument on its wrapper", {
  # this is what makes a snake_case suggestion always typeable
  for (mark in names(.vg_mark_own_props)) {
    wrapper <- get0(paste0("vg_mark_", camel_to_snake(mark)), envir = asNamespace("vgplotr"), mode = "function")
    expect_false(is.null(wrapper), info = paste("no wrapper for mark", mark))
    if (is.null(wrapper)) next
    missing <- setdiff(camel_to_snake(.vg_mark_own_props[[mark]]), names(formals(wrapper)))
    expect_equal(missing, character(0), info = paste("mark", mark))
  }
})

test_that("every property of every interactor and input has a snake_case argument on its wrapper", {
  for (type in names(.vg_interactor_own_props)) {
    wrapper <- get0(paste0("vg_", camel_to_snake(type)), envir = asNamespace("vgplotr"), mode = "function")
    expect_false(is.null(wrapper), info = paste("no wrapper for", type))
    if (is.null(wrapper)) next
    missing <- setdiff(camel_to_snake(.vg_interactor_own_props[[type]]), names(formals(wrapper)))
    expect_equal(missing, character(0), info = type)
  }
})

test_that("legends accept snake_case, translated to mosaic's exact key before checking", {
  expect_false("tick_size" %in% names(formals(vg_legend_color))) # still `...`, not a real formal
  expect_false(any(is_unrecognized_warning(unrecognized_warnings(vg_legend_color(tick_size = 5)))))
  expect_equal(.vg_legend_props_snake[["tick_size"]], "tickSize")
})

# --- wrapper versus generic ---------------------------------------------------

test_that("a wrapper suggests snake_case", {
  expect_warning(vg_mark_dot(x = ~a, y = ~b, alpha = 0.5), "Did you perhaps mean `opacity` or `fill_opacity`\\?")
  expect_warning(vg_mark_dot(x = ~a, y = ~b, fill_opacty = 0.5), "Did you perhaps mean `fill_opacity`\\?")
  expect_warning(vg_mark_dot(x = ~a, y = ~b, stroke_widht = 2), "Did you perhaps mean `stroke_width`\\?")
  expect_warning(vg_interval_x(as = param(s), pixelsize = 2), "Did you perhaps mean `pixel_size`\\?")
  expect_warning(vg_menu(column = "a", filterby = param(s)), "Did you perhaps mean `filter_by`\\?")
})

test_that("the generic constructors suggest mosaic's exact key, since that is all they accept", {
  expect_warning(vg_mark(mark = "dot", x = ~a, y = ~b, alpha = 0.5), "Did you perhaps mean `opacity` or `fillOpacity`\\?")
  expect_warning(vg_mark(mark = "dot", x = ~a, y = ~b, fill_opacity = 0.5), "Did you perhaps mean `fillOpacity`\\?")
  expect_warning(vg_interactor(interactor = "intervalX", as = param(s), pixel_size = 2), "Did you perhaps mean `pixelSize`\\?")
  expect_warning(vg_interactor(interactor = "menu", column = "a", filter_by = param(s)), "Did you perhaps mean `filterBy`\\?")
})

test_that("legends suggest snake_case", {
  expect_warning(vg_legend_color(tick_siz = 5), "Did you perhaps mean `tick_size`\\?")
  expect_warning(vg_legend_color(margin_lft = 5), "Did you perhaps mean `margin_left`\\?")
  expect_warning(vg_legend(type = "color", ticksize = 5), "Did you perhaps mean `tick_size`\\?")
})

test_that("scales, guides and attribute setters were already snake_case, and still are", {
  expect_warning(vg_scale_x(domian = c(0, 1)), "Did you perhaps mean `domain`\\?")
  expect_warning(vg_guide_x(tick_sise = 5), "Did you perhaps mean `tick_size`\\?")
  expect_warning(vg_create() |> vg_attributes(xdomian = c(0, 1)), "Did you perhaps mean `x_domain`\\?")
})

test_that("accepting either spelling: typing the exact key through a wrapper is fine and silent", {
  expect_no_warning(vg_mark_dot(x = ~a, y = ~b, fillOpacity = 0.5))
  expect_no_warning(vg_mark_dot(x = ~a, y = ~b, fill_opacity = 0.5))
  expect_no_warning(vg_interval_x(as = param(s), pixelSize = 2))
  expect_no_warning(vg_legend_color(tickSize = 5))
  expect_no_warning(vg_legend_color(tick_size = 5))
})

# --- the argument named in the rest of a message uses the same spelling -------

test_that("an enum warning names the argument as the caller spells it", {
  expect_warning(vg_mark_dot(x = ~a, y = ~b, frame_anchor = "top_left"), "`frame_anchor = \"top_left\"` is not a recognized value")
  expect_warning(vg_mark(mark = "dot", x = ~a, y = ~b, frameAnchor = "top_left"), "`frameAnchor = \"top_left\"` is not a recognized value")
  expect_warning(vg_menu(column = "a", list_match = "some"), "`list_match = \"some\"`")
  expect_warning(vg_interactor(interactor = "menu", column = "a", listMatch = "some"), "`listMatch = \"some\"`")
})

test_that("a transform error names the argument as the caller spells it", {
  msg <- function(expr) tryCatch({ force(expr); "" }, error = function(e) conditionMessage(e))
  expect_match(msg(vg_interval_x(as = param(s), pixel_size = ~vg_bin(a, stp = 1))), "in the `pixel_size` argument of interactor")
  expect_match(msg(vg_interactor(interactor = "intervalX", as = param(s), pixelSize = ~vg_bin(a, stp = 1))), "in the `pixelSize` argument of interactor")
  expect_match(msg(vg_mark_dot(x = ~vg_bin(delay, stp = 1))), "in the `x` argument of mark")
})

# --- "already supplied" -------------------------------------------------------

test_that("the typo being corrected is never treated as 'already supplied' (pixelsize normalises to pixel_size)", {
  # regression: comparing normalised names made `pixelsize` exclude its own correction
  expect_warning(vg_interval_x(as = param(s), pixelsize = 2), "`pixel_size`")
  expect_warning(vg_mark_dot(x = ~a, y = ~b, strokewidth = 2), "Did you perhaps mean `stroke_width`\\?")
})

test_that("a recognised name already supplied, in either spelling, is not suggested again", {
  # `strokeWidth` (exact key) is supplied, so `linewidth` has nothing left to point at
  expect_no_match(unrecognized_warnings(vg_mark_dot(x = ~a, y = ~b, strokeWidth = 2, linewidth = 3)), "perhaps")
  expect_no_match(unrecognized_warnings(vg_mark_dot(x = ~a, y = ~b, stroke_width = 2, linewidth = 3)), "perhaps")
  # ...and `color =` next to `fill =` only points at what is left
  expect_warning(vg_mark_dot(x = ~a, y = ~b, color = "red", fill = "blue"), "Did you perhaps mean `stroke`\\?$")
})

# --- closing the loop: every suggestion is typeable ---------------------------

test_that("every suggestion a mark wrapper makes is accepted when passed back to that wrapper", {
  wrong <- c("alpha", "linewidth", "size", "color", "colour", "lwd", "linetype", "shape", "group", "strke", "opacty", "stroke_widht", "fill_opacty")
  checked <- 0
  for (mark in names(.vg_mark_own_props)) {
    wrapper <- get(paste0("vg_mark_", camel_to_snake(mark)), envir = asNamespace("vgplotr"), mode = "function")
    for (bad in wrong) {
      w <- unrecognized_warnings(do.call(wrapper, stats::setNames(list(1), bad)))
      w <- w[is_unrecognized_warning(w) & grepl("perhaps", w)]
      if (length(w) == 0) next
      for (name in suggested_names(w[[1]])) {
        again <- unrecognized_warnings(do.call(wrapper, stats::setNames(list(1), name)))
        checked <- checked + 1
        expect_false(
          any(is_unrecognized_warning(again)),
          info = sprintf("%s(%s = ...) suggested `%s`, which is not accepted by %s", mark, bad, name, mark)
        )
      }
    }
  }
  expect_gt(checked, 100)   # make sure the loop really exercised something
})

test_that("every suggestion the generic vg_mark() and legends make is accepted when passed back", {
  generic <- function(name) do.call(vg_mark, c(list(mark = "dot", x = ~a, y = ~b), stats::setNames(list(1), name)))
  legend <- function(name) do.call(vg_legend_color, stats::setNames(list(1), name))

  loop <- function(call, wrong, label) {
    checked <- 0
    for (bad in wrong) {
      w <- unrecognized_warnings(call(bad))
      w <- w[grepl("perhaps", w)]
      if (length(w) == 0) next
      for (name in suggested_names(w[[1]])) {
        again <- unrecognized_warnings(call(name))
        checked <- checked + 1
        expect_false(any(is_unrecognized_warning(again)), info = paste(label, ":", bad, "->", name))
      }
    }
    checked
  }
  # (each loop must really have checked something: an empty loop proves nothing)
  expect_gt(loop(generic, c("alpha", "linewidth", "size", "color", "fill_opacity", "stroke_width", "frame_anchor"), "vg_mark"), 8)
  expect_gt(loop(legend, c("tick_siz", "margin_lft", "lable", "title", "ncol"), "legend"), 3)
})
