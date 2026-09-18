test_that("as_spec_payload() serializes a single mark with inline data", {
  df <- data.frame(Date = c("2020-01-01", "2020-02-01"), Close = c(296.24, 313.05))

  spec <- vg_create() |>
    vg_data(name = "aapl", data = df) |>
    vg_mark_line_y(data_from = "aapl", x = ~Date, y = ~Close) |>
    vg_attributes(width = 680, height = 200)

  payload <- as_spec_payload(spec)

  expect_equal(payload$tables$aapl, df)
  expect_null(payload$spec$data)

  expect_equal(payload$spec$plot[[1]]$mark, "lineY")
  expect_equal(payload$spec$plot[[1]]$data, list(from = "aapl"))
  expect_equal(payload$spec$plot[[1]]$x, "Date")
  expect_equal(payload$spec$plot[[1]]$y, "Close")

  expect_equal(payload$spec$width, 680)
  expect_equal(payload$spec$height, 200)
})

test_that("as_spec_payload() serializes an interactor with a param() reference", {
  df <- data.frame(t = 1:3, v = c(1, 2, 3))

  spec <- vg_create() |>
    vg_data(name = "walk", data = df) |>
    vg_mark_area_y(data_from = "walk", x = ~t, y = ~v, fill = "steelblue") |>
    vg_interval_x(as = param(brush))

  payload <- as_spec_payload(spec)

  expect_equal(payload$spec$plot[[1]]$fill, "steelblue")
  expect_equal(payload$spec$plot[[2]], list(select = "intervalX", as = "$brush"))
})

test_that("as_spec_payload() rejects formulas with more than a bare column name", {
  spec <- vg_create() |> vg_mark_dot(x = ~ log(a), y = ~b)
  expect_error(as_spec_payload(spec), "transform functions")
})

test_that("as_spec_payload() rejects a spec with no plots yet", {
  expect_error(as_spec_payload(vg_create()), "doesn't have any plots yet")
})

test_that("as_spec_payload() rejects an unrecognized layout object", {
  spec <- vg_create()
  spec$layout <- "not a valid layout"
  expect_error(as_spec_payload(spec), "Don't know how to serialize a layout")
})

test_that("vg_data() with `data` stores a data frame, otherwise stores file/query options", {
  df <- data.frame(x = 1)
  spec <- vg_create() |> vg_data(name = "a", data = df) |> vg_data(name = "b", file = "x.parquet")

  expect_equal(spec$data$a, list(data = df))
  expect_equal(spec$data$b, list(file = "x.parquet"))
})

test_that("an explicit NULL plot attribute (e.g., xAxis = NULL, hiding an axis) survives to the JSON payload", {
  spec <- vg_create() |>
    vg_data(name = "d", data = data.frame(a = 1, b = 2)) |>
    vg_mark_dot(data_from = "d", x = ~a, y = ~b) |>
    vg_plot(xAxis = NULL, width = 100)

  payload <- as_spec_payload(spec)
  expect_true("xAxis" %in% names(payload$spec))
  expect_null(payload$spec$xAxis)

  json <- to_json(spec)
  expect_match(as.character(json), '"xAxis":\\s*null', perl = TRUE)
})

test_that("vg_plot_defaults() with an explicit NULL survives per-plot serialization", {
  spec <- vg_create() |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_plot_defaults(xAxis = NULL, width = 680)

  payload <- as_spec_payload(spec)
  expect_true("xAxis" %in% names(payload$spec))
  expect_null(payload$spec$xAxis)
  expect_equal(payload$spec$width, 680)
})

test_that("data_from accepts a literal vector for mosaic's inline-data shorthand (not just a table name)", {
  # e.g., ruleY({data: [0]}) for a single reference line, rather than a
  # named data source.
  spec <- vg_mark_rule_y(data_from = c(0))
  payload <- serialize_item(spec$items[[1]])
  expect_equal(payload$data, list(0))

  spec2 <- vg_mark_rule_y(data_from = c(0, 10, 20))
  payload2 <- serialize_item(spec2$items[[1]])
  expect_equal(payload2$data, list(0, 10, 20))

  # A single-string data_from is still treated as a table name, not inline
  # data -- the two forms are told apart by type, not just length.
  spec3 <- vg_mark_dot(data_from = "aapl", x = ~a)
  payload3 <- serialize_item(spec3$items[[1]])
  expect_equal(payload3$data, list(from = "aapl"))
})

test_that("vg_data(name, query = ...) serializes as a bare SQL string, not an object", {
  # mosaic-spec's DataQuery type is a bare string ("name": "SELECT ..."),
  # unlike DataFile/DataTable/etc., which really are objects.
  spec <- vg_create() |> vg_data(name = "endpoint", query = "SELECT 1") |> vg_mark_dot(x = ~a)

  json <- to_json(spec)
  expect_match(as.character(json), '"endpoint":\\s*"SELECT 1"', perl = TRUE)

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$data$endpoint, "SELECT 1")
})

test_that("to_json()/to_yaml() serialize NA as a real null, not the string \"NA\" or a dropped field", {
  df <- data.frame(a = c(1L, 2L), b = c(1.5, NA_real_), c = c("x", NA_character_))
  spec <- vg_create() |> vg_data(name = "d", data = df) |> vg_mark_dot(data_from = "d", x = ~a, y = ~b)

  json <- as.character(to_json(spec, pretty = FALSE))
  expect_match(json, '"b":null', fixed = TRUE)
  expect_match(json, '"c":null', fixed = TRUE)
  expect_false(grepl('"NA"', json, fixed = TRUE))

  yaml_text <- as.character(to_yaml(spec))
  expect_match(yaml_text, "b: null", fixed = TRUE)
  expect_match(yaml_text, "c: null", fixed = TRUE)
  expect_false(grepl("\\.na", yaml_text))
})

test_that("warn_ordered_factor_cols() warns for an ordered factor column, naming a domain fix, and is silent otherwise", {
  df <- data.frame(g = factor(c("lo", "hi"), levels = c("lo", "hi"), ordered = TRUE), v = 1:2)
  expect_warning(warn_ordered_factor_cols(df, "d"), "ordered factor.*level order.*x_domain")

  plain <- data.frame(g = factor(c("lo", "hi")), v = 1:2)
  expect_no_warning(warn_ordered_factor_cols(plain, "d"))
})

test_that("to_json()/to_yaml() warn once on an ordered factor column (order isn't preserved)", {
  df <- data.frame(g = factor(c("lo", "hi"), levels = c("lo", "hi"), ordered = TRUE), v = 1:2)
  spec <- vg_create() |> vg_data(name = "d", data = df) |> vg_mark_dot(data_from = "d", x = ~g, y = ~v)

  expect_warning(to_json(spec), "ordered factor")
  expect_warning(to_yaml(spec), "ordered factor")
})

test_that("to_json()/to_yaml() serialize a POSIXct column as an ISO 8601 UTC string, not a naive local time or raw epoch seconds", {
  df <- data.frame(
    t = as.POSIXct("2024-01-15 07:00:00", tz = "America/Denver"),
    v = 1
  )
  spec <- vg_create() |> vg_data(name = "d", data = df) |> vg_mark_dot(data_from = "d", x = ~t, y = ~v)

  json <- as.character(to_json(spec, pretty = FALSE))
  expect_match(json, '"t":"2024-01-15T14:00:00Z"', fixed = TRUE)

  yaml_text <- as.character(to_yaml(spec))
  expect_match(yaml_text, "2024-01-15T14:00:00Z", fixed = TRUE)
})

test_that("vg_params() already supports Selections, not just plain Params", {
  # A Selection is mosaic-spec's `{"select": "intersect"}` shape (vs. a
  # plain Param's bare value like `1`) -- vg_params() needs no special
  # handling for this since it stores whatever list value it's given
  # as-is; a `list(select = ...)` value just passes straight through.
  spec <- vg_create() |> vg_params(query = list(select = "intersect")) |> vg_mark_dot(x = ~a)

  json <- to_json(spec)
  expect_match(as.character(json), '"select":\\s*"intersect"', perl = TRUE)

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$params$query, list(select = "intersect"))
})

test_that("a plot-level attribute value that's a param() reference serializes as \"$name\", not the raw object", {
  # e.g., vg_plot(projection_rotate = param(rotate)), for a slider-driven
  # globe rotation -- attrs (vg_plot()/vg_attributes()/vg_plot_defaults())
  # weren't run through serialize_value() at all before, so a vg_param
  # value reached jsonlite as a raw, unserializable object.
  rotate_p <- param(rotate)
  spec <- vg_create() |> vg_mark_dot(x = ~a) |> vg_plot(projection_rotate = rotate_p)

  json <- to_json(spec)
  expect_match(as.character(json), '"projectionRotate":\\s*"\\$rotate"', perl = TRUE)

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$projectionRotate, "$rotate")

  spec2 <- vg_create() |> vg_mark_dot(x = ~a) |> vg_plot_defaults(projection_rotate = rotate_p)
  expect_true(grepl("\\$rotate", to_yaml(spec2)))

  spec3 <- vg_create() |> vg_mark_dot(x = ~a) |> vg_attributes(projection_rotate = rotate_p)
  expect_equal(as_spec_payload(spec3)$spec$projectionRotate, "$rotate")
})

test_that("a param's value can combine other param references (e.g., mosaic's rotate: [$x, $y] pattern)", {
  lon <- param(longitude)
  lat <- param(latitude)
  spec <- vg_create() |> vg_params(longitude = -180, latitude = -30, rotate = list(lon, lat)) |> vg_mark_dot(x = ~a)

  json <- to_json(spec)
  expect_match(as.character(json), '"rotate":\\s*\\[\\s*"\\$longitude",\\s*"\\$latitude"', perl = TRUE)

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$params$rotate, list("$longitude", "$latitude"))
})

test_that("data_optimize sets the data object's optimize flag", {
  # mosaic-spec's data: {from:, optimize:} -- disables mark-specific query
  # optimizations (e.g., M4/LTTB line simplification) for this mark's data.
  spec <- vg_mark_line_y(data_from = "bls_unemp", data_optimize = FALSE, x = ~date, y = ~unemployment)
  payload <- serialize_item(spec$items[[1]])
  expect_equal(payload$data, list(from = "bls_unemp", optimize = FALSE))
})

test_that("data_from accepts a Param/Selection for a menu-driven dynamic data source", {
  # e.g., `data_from = param(data)`, for a menu that switches between
  # differently-sampled tables -- this is a table reference ("$data"),
  # not mosaic-spec's inline-data shorthand (which only applies to a
  # literal vector).
  data_p <- param(data)
  spec <- vg_mark_raster(data_from = data_p, x = ~time, y = ~delay)
  payload <- serialize_item(spec$items[[1]])
  expect_equal(payload$data, list(from = "$data"))
})

test_that("as_spec_payload() parses a JSON string spec directly, with no tables/files", {
  json <- '{"meta": {"title": "Hi"}, "plot": [{"mark": "dot", "x": "a", "y": "b"}]}'
  payload <- as_spec_payload(json)

  expect_equal(payload$spec$meta$title, "Hi")
  expect_equal(payload$spec$plot[[1]], list(mark = "dot", x = "a", y = "b"))
  expect_equal(payload$tables, list())
  expect_equal(payload$files, list())
})

test_that("as_spec_payload() parses a YAML string spec directly", {
  yml <- "
meta:
  title: Hi
plot:
  - mark: dot
    x: a
    y: b
"
  payload <- as_spec_payload(yml)

  expect_equal(payload$spec$meta$title, "Hi")
  expect_equal(payload$spec$plot[[1]], list(mark = "dot", x = "a", y = "b"))
})

test_that("as_spec_payload() doesn't mangle an unquoted YAML `y:` key into a boolean", {
  # YAML 1.1 (what the yaml package implements) resolves bare y/n/yes/no/
  # on/off as booleans -- `y` collides with the x/y encoding channel name,
  # so an unquoted `y:` key would otherwise come back named "TRUE".
  yml <- "
plot:
  - mark: dot
    x: a
    y: b
"
  payload <- as_spec_payload(yml)
  expect_named(payload$spec$plot[[1]], c("mark", "x", "y"))
  expect_equal(payload$spec$plot[[1]]$y, "b")
})

test_that("as_spec_payload() leaves real YAML booleans (true/false) as logicals, keys and values alike", {
  yml <- "
plot:
  - mark: dot
    x: a
    y: b
    tip: true
    no_border: false
    label: yes
"
  payload <- as_spec_payload(yml)
  mark <- payload$spec$plot[[1]]
  expect_identical(mark$tip, TRUE)
  expect_identical(mark$no_border, FALSE)
  expect_identical(mark$label, TRUE) # unquoted `yes` as a *value* is legitimately boolean
})

test_that("as_spec_payload() preserves a large YAML integer instead of silently NA-ing it", {
  # yaml::yaml.load()'s default implicit-integer resolution goes through
  # as.integer(), which silently overflows to NA past 32-bit range --
  # confirmed directly against a real mosaic example (docs/public/specs/
  # yaml/observable-latency.yaml's `xDomain: [1706227200000, 1706832000000]`,
  # a plain millisecond epoch timestamp pair). A custom "int" handler
  # (yaml_int_handler(), R/serialize.R) falls back to as.numeric() instead.
  yml <- "
plot:
  - mark: dot
    x: a
    y: b
xDomain: [1706227200000, 1706832000000]
"
  payload <- as_spec_payload(yml)
  expect_equal(payload$spec$xDomain, c(1706227200000, 1706832000000))
})

test_that("as_spec_payload() round-trips a vgspec through to_yaml()", {
  spec <- vg_create() |>
    vg_mark_dot(x = ~a, y = ~b)
  via_string <- as_spec_payload(to_yaml(spec))
  direct <- as_spec_payload(spec)
  expect_equal(via_string$spec, direct$spec)
})

test_that("as_spec_payload() gives a clear error on a non-length-1 string", {
  expect_error(as_spec_payload(character(0)), "single JSON/YAML string")
  expect_error(as_spec_payload(c("a", "b")), "single JSON/YAML string")
})

test_that("as_spec_payload() gives a clear error on malformed JSON/YAML text", {
  expect_error(as_spec_payload("{not valid"), "Couldn't parse `spec` as JSON")
  expect_error(as_spec_payload("not: [valid yaml: ["), "Couldn't parse `spec` as YAML")
})

test_that("as_spec_payload() rejects a string that doesn't parse to an object", {
  expect_error(as_spec_payload("[1, 2, 3]"), "must parse to a JSON/YAML object")
  expect_error(as_spec_payload("just a string"), "must parse to a JSON/YAML object")
})
