# A formula names data and a string is a constant, even where Mosaic would
# read the bare JSON string the other way (R/channels.R).

mark_json <- function(...) {
  spec <- vg_create() |>
    vg_data(name = "d", data = data.frame(x = 1:3, g = c("a", "b", "c"))) |>
    vg_mark_dot(x = ~x, ...)
  as_spec_payload(spec)$spec$plot[[1]]
}

test_that("a formula naming a column Mosaic would read as a constant is sent as {column: ...}", {
  expect_identical(mark_json(symbol = ~square)$symbol, list(column = "square"))
  expect_identical(mark_json(symbol = ~Circle)$symbol, list(column = "Circle"))
  expect_identical(mark_json(fill = ~red)$fill, list(column = "red"))
  expect_identical(mark_json(stroke = ~steelblue)$stroke, list(column = "steelblue"))
  expect_identical(mark_json(stroke = ~transparent)$stroke, list(column = "transparent"))
})

test_that("other column names stay plain strings, as in Mosaic's own specs", {
  expect_identical(mark_json(symbol = ~species)$symbol, "species")
  expect_identical(mark_json(fill = ~g)$fill, "g")
  # "red" is only a constant for the color channels
  expect_identical(mark_json(y = ~red)$y, "red")
  expect_identical(mark_json(symbol = ~red)$symbol, "red")
})

test_that("strings are still sent as the constants they are", {
  expect_identical(mark_json(symbol = "square")$symbol, "square")
  expect_identical(mark_json(fill = "red")$fill, "red")
})

test_that("a formula with a string literal is a scaled constant, sent as a SQL literal", {
  expect_identical(mark_json(symbol = ~"b")$symbol, list(sql = "'b'"))
  expect_identical(mark_json(fill = ~"red")$fill, list(sql = "'red'"))
  expect_identical(mark_json(fill = ~"it's")$fill, list(sql = "'it''s'"))
})

test_that("a variable holding a param() is still sent as a param reference", {
  square <- param(square)
  expect_identical(mark_json(symbol = ~square)$symbol, "$square")
})

test_that("to_json() and to_yaml() use the same channel handling", {
  spec <- vg_create() |>
    vg_data(name = "d", data = data.frame(x = 1, square = "a")) |>
    vg_mark_dot(x = ~x, symbol = ~square, fill = ~"b")
  json <- jsonlite::fromJSON(to_json(spec), simplifyVector = FALSE)$plot[[1]]
  expect_identical(json$symbol, list(column = "square"))
  expect_identical(json$fill, list(sql = "'b'"))
  yaml <- yaml::yaml.load(to_yaml(spec))$plot[[1]]
  expect_identical(yaml$symbol, list(column = "square"))
  expect_identical(yaml$fill, list(sql = "'b'"))
})

test_that("mosaic_is_color() matches Mosaic's isColor() on the forms it accepts", {
  for (v in c("red", "RED", " Red ", "rebeccapurple", "transparent", "none", "currentColor",
              "#abc", "#abcd", "#aabbcc", "#aabbccdd", "rgb(1, 2, 3)", "hsla(1, 2%, 3%, 0.5)",
              "var(--x)", "url(#grad)")) {
    expect_true(mosaic_is_color(v), label = v)
  }
  for (v in c("species", "reddish", "#ab", "#abcde", "density", "grey0")) {
    expect_false(mosaic_is_color(v), label = v)
  }
})

test_that("the ported Mosaic lists match the bundled Mosaic/d3 sources", {
  js <- test_path("..", "..", "data-raw", "js", "node_modules")
  skip_if_not(dir.exists(js), "needs data-raw/js/node_modules")
  quoted <- function(file) {
    src <- paste(readLines(file.path(js, file), warn = FALSE), collapse = "\n")
    gsub("'", "", regmatches(src, gregexpr("'[A-Za-z0-9]+'", src))[[1]])
  }
  expect_setequal(.mosaic_constant_options, quoted("@uwdata/mosaic-plot/src/marks/util/is-constant-option.js"))
  expect_setequal(.mosaic_symbols, quoted("@uwdata/mosaic-plot/src/marks/util/is-symbol.js"))
  colors <- readLines(file.path(js, "d3-color/src/color.js"), warn = FALSE)
  colors <- sub(r"(^  ([a-z]+): 0x.*)", r"(\1)", grep(r"(^  [a-z]+: 0x)", colors, value = TRUE))
  expect_setequal(.css_color_names, colors)
})
