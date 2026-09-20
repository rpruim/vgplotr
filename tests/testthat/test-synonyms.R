# The synonym table (inst/extdata/suggestion-synonyms.yaml) maps a different WORD
# for something (`alpha`, `linewidth`, `mean`) to what vgplotr calls it, for the
# "Did you perhaps mean ...?" suggestions. See R/suggest.R.

# --- the file itself: what makes it safe to amend ----------------------------

test_that("the synonym file is present, parses, and has every section", {
  expect_true(nzchar(synonym_table_path()))
  raw <- raw_synonym_table()
  expect_setequal(names(raw), c("arguments", "values", "transform_options", "transform_names"))
  expect_gt(length(raw$arguments), 30)
})

test_that("every entry in the synonym file is well-formed and points at something real", {
  # This is the check to run after editing the file. A typo in a candidate, a
  # duplicate key, an entry that can never fire, or a name a future mosaic
  # version drops shows up here instead of as a bad suggestion.
  expect_equal(synonym_table_problems(raw_synonym_table()), character(0))
})

test_that("loading drops nothing: the cleaned table has exactly the entries the file has", {
  expect_equal(count_synonym_entries(vg_synonym_table()), count_synonym_entries(raw_synonym_table()))
})

test_that("the validator itself catches each kind of mistake (so the check above can't quietly stop guarding)", {
  bad <- list(
    arguments = list(alpha = c("opacity", "opacty"), alpha = "opacity", color = "color", width = "opacity", empty = character()),
    values = list(textAnchor = list(center = "midle", left = "start"), notAnArgument = list(x = "y")),
    transform_options = list(bins = "stepz"),
    transform_names = list(vg_mean = "vg_avg", median2 = "vg_nope"),
    bogus_section = list()
  )
  problems <- paste(synonym_table_problems(bad), collapse = "\n")
  expect_match(problems, "unknown top-level section\\(s\\): bogus_section")
  expect_match(problems, "`alpha` -> `opacty`, which doesn't exist")
  expect_match(problems, "`alpha` is listed more than once")
  expect_match(problems, "`color` suggests itself")
  expect_match(problems, "`width` is always valid here")
  expect_match(problems, "`empty` has no candidates")
  expect_match(problems, "`center` -> `midle`, which doesn't exist")
  expect_match(problems, "`notAnArgument` is not an argument with a fixed set of values")
  expect_match(problems, "`bins` -> `stepz`, which doesn't exist")
  expect_match(problems, "key `vg_mean` should be written without the `vg_` prefix")
  expect_match(problems, "`median2` -> `vg_nope`, which doesn't exist")
  expect_equal(synonym_table_problems(list(arguments = list(), values = list(), transform_options = list(), transform_names = list())), character(0))
})

# --- reading the file ---------------------------------------------------------

test_that("read_synonym_table() reads an amended file, with YAML 1.2 rules (`n`, `on`, `no` need no quotes)", {
  path <- tempfile(fileext = ".yaml")
  writeLines(c(
    "arguments:", "  color: [fill, stroke]", "  n: [x]", "  on: [y]", "  no: [z]",
    "values:", "  textAnchor:", "    center: [middle]",
    "transform_names:", "  n: [vg_count]", "  mean: [vg_avg]"
  ), path)
  table <- read_synonym_table(path)
  expect_equal(table$arguments$color, c("fill", "stroke"))
  expect_named(table$arguments, c("color", "n", "on", "no"))   # not converted to TRUE/FALSE
  expect_equal(table$values$textAnchor$center, "middle")
  expect_equal(table$transform_names$n, "vg_count")
  expect_equal(table$transform_options, list())                 # a missing section is just empty
})

test_that("a missing or malformed synonym file means no synonyms, never an error", {
  empty <- list(arguments = list(), values = list(), transform_options = list(), transform_names = list())
  expect_equal(read_synonym_table(tempfile(fileext = ".yaml")), empty)
  expect_equal(read_synonym_table(""), empty)
  bad <- tempfile(fileext = ".yaml")
  writeLines(c("arguments: [unclosed", "  color: {"), bad)
  expect_equal(read_synonym_table(bad), empty)
})

test_that("malformed entries are dropped, well-formed ones kept", {
  path <- tempfile(fileext = ".yaml")
  writeLines(c("arguments:", "  good: [fill]", "  scalar_not_list: fill", "  empty: []", "transform_options: notamapping"), path)
  table <- read_synonym_table(path)
  expect_equal(table$arguments$good, "fill")
  expect_equal(table$arguments$scalar_not_list, "fill")   # a lone string is a one-candidate list
  expect_false("empty" %in% names(table$arguments))
  expect_equal(table$transform_options, list())
})

test_that("the table is read once and kept", {
  expect_identical(vg_synonym_table(), vg_synonym_table())
  expect_equal(synonym_section("arguments")$color, c("fill", "stroke"))
  expect_equal(synonym_section("values", "textAnchor")$center, "middle")
  expect_equal(synonym_section("values", "noSuchArgument"), list())
})

# --- looking a word up --------------------------------------------------------

table <- list(alpha = c("opacity", "fill_opacity"), color = c("fill", "stroke"), linewidth = "stroke_width", size = "r", n = "vg_count")

test_that("lookup_synonym() returns the candidates that are valid here, spelled as the caller spells them, in the table's order", {
  valid <- c("stroke", "fillOpacity", "opacity", "fill", "x")
  expect_equal(lookup_synonym("alpha", valid, table), c("opacity", "fillOpacity"))   # table order, caller's spelling
  expect_equal(lookup_synonym("color", valid, table), c("fill", "stroke"))
})

test_that("lookup_synonym() returns NULL for a word that isn't in the table", {
  expect_null(lookup_synonym("bogus", c("fill", "stroke"), table))
  expect_null(lookup_synonym("", c("fill"), table))
  expect_null(lookup_synonym("x", c("fill"), list()))
})

test_that("a word in the table with nothing applicable here gives an empty result, not NULL (so no fuzzy fallback)", {
  expect_equal(lookup_synonym("color", c("x", "y", "colorN"), table), character(0))
  expect_equal(lookup_synonym("alpha", "x", table), character(0))
})

test_that("lookup_synonym() ignores case, underscores, dots and hyphens in the word", {
  for (w in c("LineWidth", "line_width", "line.width", "line-width", "LINEWIDTH")) {
    expect_equal(lookup_synonym(w, "strokeWidth", table), "strokeWidth", info = w)
  }
})

test_that("a one-edit misspelling of a key of five or more letters matches it; shorter keys must match exactly", {
  expect_equal(lookup_synonym("colr", c("fill", "stroke"), table), c("fill", "stroke"))      # `color`, 5 letters
  expect_equal(lookup_synonym("colors", c("fill", "stroke"), table), c("fill", "stroke"))
  expect_equal(lookup_synonym("alpa", "opacity", table), "opacity")
  # `size` has 4 letters: one edit is too big a share of it
  expect_null(lookup_synonym("sise", "r", table))
  expect_null(lookup_synonym("sizes", "r", table))
  expect_equal(lookup_synonym("size", "r", table), "r")
  # two edits is too far even for a long key
  expect_null(lookup_synonym("colxr2", c("fill"), table))
})

test_that("when several keys are within one edit, the closest wins", {
  tbl <- list(colour = "a", colors = "b")
  expect_equal(lookup_synonym("colours", c("a", "b"), tbl), "a")
})

test_that("an entry with a leading sign in a candidate keeps it", {
  expect_equal(lookup_synonym("desc", c("value", "-value"), list(desc = "-value")), "-value")
})

# --- how suggestions use the table ---------------------------------------------

test_that("the synonym table is tried before edit distance", {
  # `size` is not close to anything in this pool by spelling, but it's a known synonym for `r`
  expect_equal(suggest_names("size", c("r", "fill", "stroke")), "r")
  # a table hit is final: it doesn't also fall back to edit distance
  expect_equal(suggest_names("color", c("x", "colorN")), character(0))
  expect_equal(suggest_names("colors", c("x", "colorN")), character(0))
})

test_that("a plain misspelling that isn't a synonym still uses edit distance", {
  pool <- c("fill", "stroke", "opacity", "strokeWidth")
  expect_equal(suggest_names("strke", pool), "stroke")
  expect_equal(suggest_names("opacty", pool), "opacity")
  expect_equal(suggest_names("stroke_widht", pool), "strokeWidth")
})

test_that("a synonym is never suggested when its target was already supplied", {
  expect_equal(suggest_names("color", c("fill", "stroke"), present = "fill"), "stroke")
  expect_equal(suggest_names("color", c("fill", "stroke"), present = c("fill", "stroke")), character(0))
  expect_equal(suggest_names("alpha", c("opacity", "fillOpacity"), present = "opacity"), "fillOpacity")
})

test_that("suggest_values() uses the argument's own synonyms, then edit distance", {
  expect_equal(suggest_values("center", c("start", "middle", "end"), "textAnchor"), "middle")
  expect_equal(suggest_values("descending", c("value", "-value", "sum"), "order"), "-value")
  expect_equal(suggest_values("midle", c("start", "middle", "end"), "textAnchor"), "middle")   # a typo, not a synonym
  # the same word means nothing for an argument that has no such synonym
  expect_equal(suggest_values("center", c("start", "middle", "end"), "noSuchArgument"), character(0))
})

# --- end to end: the warnings people actually see ------------------------------

test_that("the headline examples: color -> fill|stroke, alpha -> opacity|fill_opacity", {
  expect_warning(vg_mark_dot(x = ~a, y = ~b, color = "red"), "Did you perhaps mean `fill` or `stroke`\\?")
  expect_warning(vg_mark_dot(x = ~a, y = ~b, alpha = 0.5), "Did you perhaps mean `opacity` or `fillOpacity`\\?")
})

test_that("ggplot2 and base R names on a mark", {
  expect_warning(vg_mark_dot(x = ~a, y = ~b, size = 3), "Did you perhaps mean `r`\\?")
  expect_warning(vg_mark_text(x = ~a, y = ~b, text = ~c, size = 3), "Did you perhaps mean `fontSize`\\?")
  expect_warning(vg_mark_line(x = ~a, y = ~b, linewidth = 2), "Did you perhaps mean `strokeWidth`\\?")
  expect_warning(vg_mark_line(x = ~a, y = ~b, lwd = 2), "Did you perhaps mean `strokeWidth`\\?")
  expect_warning(vg_mark_line(x = ~a, y = ~b, linetype = "dashed"), "Did you perhaps mean `strokeDasharray`\\?")
  expect_warning(vg_mark_line(x = ~a, y = ~b, group = ~g), "Did you perhaps mean `z`\\?")
  expect_warning(vg_mark_rect_y(x = ~a, ymin = ~b, ymax = ~c), "For `ymin`, did you perhaps mean `y1`\\?.*For `ymax`, did you perhaps mean `y2`\\?")
  expect_warning(vg_mark_dot(data = mtcars, x = ~a), "Did you perhaps mean `data_from`\\?")
})

test_that("a synonym is only suggested where it applies, and silence beats an unrelated guess", {
  quiet <- function(expr) {
    w <- character()
    withCallingHandlers(expr, warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
    w
  }
  # a line has neither a radius nor a font size, so `size` has nothing to point at
  expect_no_match(quiet(vg_mark_line(x = ~a, y = ~b, size = 2)), "perhaps")
  # a brush has no fill/stroke; and it must not be told `colorN`
  expect_no_match(quiet(vg_interval_x(as = param(s), color = "red")), "perhaps")
  expect_no_match(quiet(vg_interval_x(as = param(s), colors = "red")), "perhaps")
})

test_that("colour on the axis marks, which do have `color`, is pointed at it first", {
  expect_warning(vg_mark_axis_x(colour = "red"), "Did you perhaps mean `color`, `fill` or `stroke`\\?")
})

test_that("scales, guides, legends, inputs and interactors", {
  expect_warning(vg_scale_x(xlim = c(0, 1)), "Did you perhaps mean `domain` or `x_domain`\\?")
  expect_warning(vg_scale_color(palette = "blues"), "Did you perhaps mean `scheme` or `range`\\?")
  expect_warning(vg_guide_x(breaks = 5), "Did you perhaps mean `ticks`\\?")
  expect_warning(vg_legend_color(title = "Species"), "Did you perhaps mean `label`\\?")
  expect_warning(vg_legend_color(ncol = 2), "Did you perhaps mean `columns`\\?")
  expect_warning(vg_slider(column = "a", default = 5), "Did you perhaps mean `value`\\?")
  expect_warning(vg_menu(column = "a", choices = c("x", "y")), "Did you perhaps mean `options`\\?")
  expect_warning(vg_interval_x(selection = param(b)), "Did you perhaps mean `as`\\?")
})

test_that("enum values", {
  expect_warning(vg_mark_text(x = ~a, y = ~b, text = ~c, text_anchor = "center"), "Did you perhaps mean `\"middle\"`\\?")
  expect_warning(vg_mark_bar_x(x = ~a, y = ~b, order = "descending"), "Did you perhaps mean `\"-value\"`\\?")
  expect_warning(vg_mark_line(x = ~a, y = ~b, curve = "straight"), "Did you perhaps mean `\"linear\"`\\?")
  expect_warning(vg_slider(column = "a", select = "range"), "Did you perhaps mean `\"interval\"`\\?")
  expect_warning(vg_menu(column = "a", list_match = "some"), "Did you perhaps mean `\"any\"`\\?")
})

test_that("transform function names", {
  built <- function(...) tryCatch(vg_mark_dot(...), error = function(e) conditionMessage(e))
  expect_match(built(x = ~delay, y = ~mean(delay)), "Did you perhaps mean `vg_avg\\(\\)`\\?")
  expect_match(built(x = ~delay, y = ~n()), "Did you perhaps mean `vg_count\\(\\)`\\?")
  expect_match(built(x = ~hist(delay), y = ~vg_count()), "Did you perhaps mean `vg_bin\\(\\)`\\?")
  expect_match(built(x = ~delay, y = ~vg_sd(delay)), "Did you perhaps mean `vg_stddev\\(\\)`\\?")
  # a base R function with no counterpart still gets no suggestion
  expect_no_match(built(x = ~log(delay), y = ~vg_count()), "perhaps")
})

test_that("transform options", {
  built <- function(...) tryCatch(vg_mark_dot(...), error = function(e) conditionMessage(e))
  expect_match(built(x = ~vg_bin(delay, bins = 20), y = ~vg_count()), "`bins` is not an argument.*Did you perhaps mean `steps`\\?")
  expect_match(built(x = ~vg_bin(delay, binwidth = 5), y = ~vg_count()), "Did you perhaps mean `step`\\?")
})

test_that("real misspellings of real names still get their spelling suggestion (the table doesn't get in the way)", {
  expect_warning(vg_mark_dot(x = ~a, y = ~b, strke = "red"), "Did you perhaps mean `stroke`\\?")
  expect_warning(vg_mark_dot(x = ~a, y = ~b, fill_opacty = 0.5), "Did you perhaps mean `fillOpacity`\\?")
  expect_warning(vg_legend_color(lable = "L"), "Did you perhaps mean `label`\\?")
})

# --- priority: a real name beats a guess -------------------------------------

test_that("an exact synonym key beats a spelling match", {
  # `labels` is one edit from `label`, but it's a ggplot2 word with its own meaning
  pool <- c("label", "tickFormat")
  expect_equal(suggest_names("labels", pool, table = list(labels = c("tick_format", "label"))), c("tickFormat", "label"))
})

test_that("a typo of a real name is read as that typo, even when it is one edit from a synonym key", {
  # each of these used to be steered to the key's answer (or to nothing)
  expect_equal(suggest_names("ttle", c("title", "fill")), "title")           # key `title` -> label
  expect_equal(suggest_names("itle", c("title", "fill")), "title")
  expect_equal(suggest_names("lnieWidth", c("lineWidth", "strokeWidth")), "lineWidth")   # key `linewidth` -> stroke_width
  expect_equal(suggest_names("labes", c("label", "tickFormat")), "label")    # key `labels` -> tick_format
  expect_equal(suggest_names("olor", c("color", "fill", "stroke")), "color") # key `color` -> fill/stroke
})

test_that("a near-miss of a key is only a last resort: used when no real name is close", {
  # nothing in the pool is near `colr`, so the key `color` is what it was a slip for
  expect_equal(suggest_names("colr", c("fill", "stroke", "opacity")), c("fill", "stroke"))
  expect_equal(suggest_names("alpa", c("opacity", "fillOpacity")), c("opacity", "fillOpacity"))
  # ...but when a real name is close, that wins
  expect_equal(suggest_names("colr", c("color", "fill", "stroke")), "color")
})

test_that("lookup_synonym(near = FALSE) matches exact keys only", {
  tbl <- list(color = c("fill", "stroke"))
  expect_equal(lookup_synonym("color", c("fill", "stroke"), tbl, near = FALSE), c("fill", "stroke"))
  expect_equal(lookup_synonym("Color", c("fill", "stroke"), tbl, near = FALSE), c("fill", "stroke"))   # case is ignored
  expect_null(lookup_synonym("colr", c("fill", "stroke"), tbl, near = FALSE))
  expect_equal(lookup_synonym("colr", c("fill", "stroke"), tbl, near = TRUE), c("fill", "stroke"))
})

test_that("`labels` and `values` still point at the real answer where the ggplot2 meaning doesn't apply", {
  expect_warning(vg_legend_color(labels = c("a", "b")), "Did you perhaps mean `label`\\?")
  expect_warning(vg_guide_x(labels = "abc"), "Did you perhaps mean `tick_format` or `label`\\?")
  expect_warning(vg_slider(column = "a", values = 1:3), "Did you perhaps mean `value`\\?")
  expect_warning(vg_menu(column = "a", values = c("x", "y")), "Did you perhaps mean `options` or `value`\\?")
  expect_warning(vg_scale_color(values = c("red", "blue")), "Did you perhaps mean `range`\\?")
})

test_that("the plural and British spellings of color have their own entries", {
  expect_warning(vg_mark_dot(x = ~a, y = ~b, colors = "red"), "Did you perhaps mean `fill` or `stroke`\\?")
  expect_warning(vg_mark_dot(x = ~a, y = ~b, colours = "red"), "Did you perhaps mean `fill` or `stroke`\\?")
  # and on a brush there is still nothing sensible to say -- notably not `colorN`
  w <- character()
  withCallingHandlers(
    vg_interval_x(as = param(s), colors = "red"),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") }
  )
  expect_no_match(w, "perhaps")
})
