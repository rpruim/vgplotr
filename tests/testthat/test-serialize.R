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

test_that("a formula with more than a bare column name is rejected when the mark is built", {
  expect_error(vg_create() |> vg_mark_dot(x = ~ log(a), y = ~b), "transform functions")
})

test_that("as_spec_payload() still rejects such a formula (the safety net behind the early check)", {
  expect_error(serialize_unchecked(x = ~ log(a)), "transform functions")
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

test_that("as_spec_payload() leaves real YAML booleans (true/false, in all three YAML 1.2 casings) as logicals", {
  yml <- "
plot:
  - mark: dot
    x: a
    y: b
    tip: true
    no_border: false
    a: True
    b: FALSE
    c: TRUE
    d: False
"
  payload <- as_spec_payload(yml)
  mark <- payload$spec$plot[[1]]
  expect_identical(mark$tip, TRUE)
  expect_identical(mark$no_border, FALSE)
  expect_identical(mark$a, TRUE)
  expect_identical(mark$b, FALSE)
  expect_identical(mark$c, TRUE)
  expect_identical(mark$d, FALSE)
})

# The JavaScript YAML parsers a mosaic spec is written for (`yaml`, `js-yaml`
# -- both checked directly, and they agree) follow YAML 1.2, where only
# true/false are booleans. R's `yaml` package is YAML 1.1, where y/n/yes/no/
# on/off are too; parse_spec_string() undoes that with bool#yes/bool#no
# handlers (yaml_bool_handler(), R/serialize.R). The tests below cover every
# context a bare `y` can appear in, since each one used to fail separately.

parsed_mark <- function(mark_yaml) {
  parse_spec_string(paste0("plot:\n  - ", mark_yaml))$plot[[1]]
}

test_that("an unquoted `y` mapping key survives in every YAML context", {
  block <- parse_spec_string("plot:\n  - mark: dot\n    x: a\n    y: b\n")$plot[[1]]
  list_item <- parse_spec_string("plot:\n  - y: b\n")$plot[[1]]
  flow <- parsed_mark("{mark: dot, x: a, y: b}\n")
  flow_first <- parsed_mark("{y: b, mark: dot}\n")
  flow_nested <- parsed_mark("{mark: dot, data: {from: d}, y: b}\n")
  flow_multiline <- parsed_mark("{ mark: dot,\n      x: a, y: b }\n")
  flow_in_flow <- parsed_mark("{mark: dot, tip: {format: {x: a, y: b}}}\n")

  expect_named(block, c("mark", "x", "y"))
  expect_named(list_item, "y")
  expect_named(flow, c("mark", "x", "y"))
  expect_named(flow_first, c("y", "mark"))
  expect_named(flow_nested, c("mark", "data", "y"))
  expect_named(flow_multiline, c("mark", "x", "y"))
  expect_named(flow_in_flow$tip$format, c("x", "y"))
  for (m in list(block, flow, flow_nested, flow_multiline)) expect_equal(m$y, "b")
})

test_that("the other YAML 1.1 boolean words are keys too, not just `y`", {
  m <- parsed_mark("{mark: dot, n: 1, yes: 2, no: 3, on: 4, off: 5, Y: 6, N: 7}\n")
  expect_named(m, c("mark", "n", "yes", "no", "on", "off", "Y", "N"))
})

test_that("an unquoted y/n/yes/on as a *value* is a string, e.g., a column literally named y", {
  # `x: y` is a channel bound to a column called "y", not the boolean TRUE
  m <- parse_spec_string("plot:\n  - mark: dot\n    x: y\n    label: yes\n    z: n\n    w: on\n    v: Y\n")$plot[[1]]
  expect_identical(m$x, "y")
  expect_identical(m$label, "yes")
  expect_identical(m$z, "n")
  expect_identical(m$w, "on")
  expect_identical(m$v, "Y")   # e.g., a legend `label: Y` in mosaic's own symbols example
})

test_that("bool-like words inside a flow sequence stay strings", {
  m <- parsed_mark("{mark: dot, channels: [x, y, n]}\n")
  expect_identical(m$channels, list("x", "y", "n"))   # a sequence is a list (as from JSON)
})

test_that("prose inside a block scalar is never rewritten", {
  # a regex over the raw text used to turn `y: ...` at the start of a line
  # inside a `|` block into `'y': ...`
  spec <- parse_spec_string("meta:\n  description: |\n    Notes:\n    y: is the vertical axis\n    n: count\nplot:\n  - mark: dot\n")
  expect_identical(spec$meta$description, "Notes:\ny: is the vertical axis\nn: count\n")
})

test_that("quoted y/n stay exactly what they were written as, key or value", {
  m <- parse_spec_string("plot:\n  - mark: dot\n    'y': b\n    x: 'y'\n    z: \"n\"\n")$plot[[1]]
  expect_named(m, c("mark", "y", "x", "z"))
  expect_identical(m$x, "y")
  expect_identical(m$z, "n")
})

test_that("only the exact YAML 1.2 spellings are booleans; odd casings are strings", {
  m <- parsed_mark("{mark: dot, a: tRuE, b: fALSE, c: Yes}\n")
  expect_identical(m$a, "tRuE")
  expect_identical(m$b, "fALSE")
  expect_identical(m$c, "Yes")
})

test_that("yaml_bool_handler() resolves the six real booleans and leaves everything else alone", {
  for (x in c("true", "True", "TRUE")) expect_identical(yaml_bool_handler(x), TRUE)
  for (x in c("false", "False", "FALSE")) expect_identical(yaml_bool_handler(x), FALSE)
  for (x in c("y", "Y", "yes", "Yes", "YES", "n", "N", "no", "No", "NO", "on", "On", "ON", "off", "Off", "OFF", "tRuE")) {
    expect_identical(yaml_bool_handler(x), x)
  }
})

test_that("a JSON string spec is unaffected (JSON never had the problem)", {
  spec <- parse_spec_string('{"plot": [{"mark": "dot", "x": "y", "y": "b", "tip": true}]}')
  expect_identical(spec$plot[[1]]$x, "y")
  expect_identical(spec$plot[[1]]$y, "b")
  expect_identical(spec$plot[[1]]$tip, TRUE)
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
  expect_equal(unlist(payload$spec$xDomain), c(1706227200000, 1706832000000))
})

test_that("as_spec_payload() round-trips a vgspec through to_yaml()", {
  spec <- vg_create() |>
    vg_mark_dot(x = ~a, y = ~b)
  via_string <- as_spec_payload(to_yaml(spec))
  direct <- as_spec_payload(spec)
  expect_equal(via_string$spec, direct$spec)
})

test_that("as_spec_payload() lifts a string spec's inline row data out of `data:` into tables", {
  # The same shape to_json()/to_yaml() write a data frame as. Left in the
  # spec, mosaic's own declarative loading races the marks' first queries
  # (see as_spec_payload()'s header comment); it must be preloaded instead.
  df <- data.frame(a = 1:3, b = c(10, 20, 15), g = c("x", "y", "x"))
  spec <- vg_create() |> vg_data(name = "d", data = df) |> vg_mark_dot(data_from = "d", x = ~a, y = ~b)

  for (text in list(to_json(spec), to_yaml(spec))) {
    payload <- as_spec_payload(text)
    expect_named(payload$tables, "d")
    expect_length(payload$tables$d, 3)
    expect_equal(payload$tables$d[[2]], list(a = 2L, b = 20, g = "y"))
    expect_null(payload$spec$data)
    expect_equal(payload$spec$plot[[1]]$data, list(from = "d"))
    expect_equal(payload$files, list())
  }
})

test_that("as_spec_payload() lifts both inline shapes (bare array, `type: json` object) and leaves other sources alone", {
  yml <- "
data:
  bare:
    - {a: 1, b: 2}
    - {a: 3, b: 4}
  typed:
    type: json
    data:
      - {a: 5}
  remote:
    file: https://example.com/x.csv
  q: SELECT 1 AS a
plot:
  - mark: dot
    x: a
    y: b
"
  payload <- as_spec_payload(yml)
  expect_setequal(names(payload$tables), c("bare", "typed"))
  expect_equal(payload$tables$typed, list(list(a = 5L)))
  expect_named(payload$spec$data, c("remote", "q"))
  expect_equal(payload$spec$data$remote, list(file = "https://example.com/x.csv"))
  expect_equal(payload$spec$data$q, "SELECT 1 AS a")
})

test_that("as_spec_payload() leaves inline data with load-time options, or no usable rows, in the spec", {
  # loadTables() can't apply `where`/`select`, so silently lifting these
  # would change what the plot shows -- mosaic's own loader handles them.
  yml <- "
data:
  filtered:
    data:
      - {a: 1}
      - {a: 2}
    where: a > 1
  empty: []
  scalars: [1, 2, 3]
plot:
  - mark: dot
    x: a
    y: b
"
  payload <- as_spec_payload(yml)
  expect_equal(payload$tables, list())
  expect_named(payload$spec$data, c("filtered", "empty", "scalars"))
})

test_that("as_spec_payload() leaves a spec with no data block alone", {
  payload <- as_spec_payload('{"plot": [{"mark": "dot", "x": "a", "y": "b"}]}')
  expect_false("data" %in% names(payload$spec))
  expect_equal(payload$tables, list())
})

test_that("vg_widget() on a to_json()/to_yaml() string ships inline data as `tables`, same as the vgspec itself", {
  df <- data.frame(a = 1:3, b = c(10, 20, 15))
  spec <- vg_create() |> vg_data(name = "d", data = df) |> vg_mark_dot(data_from = "d", x = ~a, y = ~b)
  wasm <- vg_wasm_connector()

  from_object <- vg_widget(spec, connector = wasm, use_cache = FALSE)
  for (text in list(to_json(spec), to_yaml(spec))) {
    from_string <- vg_widget(text, connector = wasm, use_cache = FALSE)
    expect_equal(from_string$x$spec, from_object$x$spec)
    expect_named(from_string$x$tables, "d")

    # what actually reaches the browser: identical JSON rows either way
    rows <- function(w) jsonlite::fromJSON(
      do.call(jsonlite::toJSON, c(list(w$x$tables$d), list(auto_unbox = TRUE, dataframe = "rows"))),
      simplifyVector = FALSE
    )
    expect_equal(rows(from_string), rows(from_object))
  }
})

test_that("register_native_inline_tables() loads a string spec's lifted rows into a native DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  spec <- vg_create() |>
    vg_data(name = "d", data = data.frame(a = 1:3, g = c("x", "y", "x"))) |>
    vg_mark_dot(data_from = "d", x = ~a, y = ~a)
  payload <- as_spec_payload(to_json(spec))
  register_native_inline_tables(payload$tables, con)

  out <- DBI::dbGetQuery(con, "SELECT a, g FROM d ORDER BY a")
  expect_equal(out$a, 1:3)
  expect_equal(out$g, c("x", "y", "x"))
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

# --- the rest of the YAML 1.1 vs 1.2 differences (see yaml_handlers()) -------

json_of <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA))

test_that("a length-1 YAML sequence stays an array, as it does in JSON", {
  # `data: [0]` is mosaic's inline-data shorthand (a reference line); it used to
  # be read as the bare integer 0 and sent as `"data": 0`
  spec <- parse_spec_string("plot:\n  - mark: ruleY\n    data: [0]\n    channels: [id]\n")
  expect_identical(json_of(spec), '{"plot":[{"mark":"ruleY","data":[0],"channels":["id"]}]}')
  expect_identical(spec$plot[[1]]$data, list(0L))
  expect_identical(spec$plot[[1]]$channels, list("id"))
})

test_that("YAML sequences of every shape parse to lists, matching the JSON path", {
  yaml_spec <- parse_spec_string("a: [[1], [2]]\nb: [1, x]\nc: []\nd: [{k: 1}]\ne: [1, 2, 3]\nf: [a]\n")
  json_spec <- parse_spec_string('{"a": [[1], [2]], "b": [1, "x"], "c": [], "d": [{"k": 1}], "e": [1, 2, 3], "f": ["a"]}')
  expect_equal(yaml_spec, json_spec)
  expect_identical(json_of(yaml_spec), '{"a":[[1],[2]],"b":[1,"x"],"c":[],"d":[{"k":1}],"e":[1,2,3],"f":["a"]}')
})

test_that("the widget payload for a YAML spec is the same JSON as for the equivalent JSON spec", {
  yml <- "plot:\n  - mark: ruleY\n    data: [0]\n  - mark: dot\n    data: {from: d}\n    x: a\n    channels: [id]\n    xDomain: [9.75e5, 1.0e6]\n"
  jsn <- '{"plot":[{"mark":"ruleY","data":[0]},{"mark":"dot","data":{"from":"d"},"x":"a","channels":["id"],"xDomain":[975000,1000000]}]}'
  expect_identical(json_of(as_spec_payload(yml)$spec), json_of(as_spec_payload(jsn)$spec))
})

test_that("exponent floats are numbers, as in YAML 1.2", {
  # YAML 1.1 only recognises an exponent that has both a sign and a decimal point
  spec <- parse_spec_string("a: 9.75e5\nb: 1e5\nc: 1E3\nd: 1e-3\ne: .5e3\nf: -1.5e3\ng: 2.5e-3\nh: 2.5e+3\ni: 1.0e6\n")
  expect_equal(unlist(spec), c(a = 975000, b = 1e5, c = 1000, d = 0.001, e = 500, f = -1500, g = 0.0025, h = 2500, i = 1e6))
  expect_true(all(vapply(spec, is.numeric, logical(1))))
  expect_equal(unlist(parse_spec_string("xDomain: [9.75e5, 1.0e6]\n")$xDomain), c(975000, 1e6))
})

test_that("leading-zero integers are decimal, as in YAML 1.2 (not octal)", {
  spec <- parse_spec_string("a: 0755\nb: 007\nc: 089\nd: 02134\ne: 0\nf: 0x1F\ng: 42\n")
  expect_identical(spec$a, 755L)     # 1.1 octal would say 493
  expect_identical(spec$b, 7L)
  expect_identical(spec$c, 89L)      # invalid octal: used to stay the string "089"
  expect_identical(spec$d, 2134L)    # e.g., an unquoted zip code
  expect_identical(spec$e, 0L)
  expect_identical(spec$f, 31L)      # hex is unchanged
  expect_identical(spec$g, 42L)
})

test_that("a quoted number-like string stays a string", {
  spec <- parse_spec_string("a: \"9.75e5\"\nb: '1e5'\nc: '089'\nd: \"0755\"\ne: \"2020\"\nf: '12'\n")
  expect_identical(spec$a, "9.75e5")
  expect_identical(spec$b, "1e5")
  expect_identical(spec$c, "089")
  expect_identical(spec$d, "0755")
  expect_identical(spec$e, "2020")
  expect_identical(spec$f, "12")
})

test_that("a quoted number-like string inside a sequence or nested mapping stays a string", {
  spec <- parse_spec_string("a: [\"1e5\", '089']\nb: {k: \"9.75e5\"}\n")
  expect_identical(spec$a, list("1e5", "089"))
  expect_identical(spec$b$k, "9.75e5")
})

test_that("if a value is quoted anywhere, the same text unquoted elsewhere is kept as a string too (the safe direction)", {
  # the handler can't tell the two apart, so it errs toward the old behaviour
  spec <- parse_spec_string("a: \"1e5\"\nb: 1e5\n")
  expect_identical(spec$a, "1e5")
  expect_identical(spec$b, "1e5")
})

test_that("quoted_number_like_tokens() finds quoted number-like scalars only", {
  txt <- "a: \"1e5\"\nb: '089'\nc: \"hello\"\nd: 1e5\ne: \"a 1e5 b\"\nf: '2020'\n"
  expect_setequal(quoted_number_like_tokens(txt), c("1e5", "089", "2020"))
  expect_equal(quoted_number_like_tokens("no quotes here: 1e5"), character(0))
})

test_that("scalars YAML 1.1 and 1.2 already agree on are unchanged", {
  spec <- parse_spec_string(paste0(
    "a: 2001-12-14\nb: 1:30\nc: 1_000\nd: hello\ne: 3.14\nf: -7\ng: ~\nh: null\ni: true\nj: ''\nk: \"\"\n"
  ))
  expect_identical(spec$a, "2001-12-14")     # timestamps stay strings
  expect_identical(spec$b, "1:30")
  expect_identical(spec$c, "1_000")
  expect_identical(spec$d, "hello")
  expect_identical(spec$e, 3.14)
  expect_identical(spec$f, -7L)
  expect_true("g" %in% names(spec) && is.null(spec$g))
  expect_identical(spec$i, TRUE)
  expect_identical(spec$j, "")
  expect_identical(spec$k, "")
})

test_that("block scalars and long strings are never reinterpreted as numbers", {
  spec <- parse_spec_string("d: |\n  1e5\n  089\ne: >\n  9.75e5\n")
  expect_type(spec$d, "character")
  expect_type(spec$e, "character")
})

test_that("a large inline data array parses quickly (the quoted-token guard is one pass, not one search per string)", {
  rows <- vapply(seq_len(20000), function(i) sprintf("    - {a: %d, b: \"%d\", c: 1e%d}", i, i, i %% 10), character(1))
  yml <- paste(c("data:", "  d:", rows, "plot:", "  - {mark: dot, data: {from: d}, x: a, y: b}"), collapse = "\n")
  elapsed <- system.time(spec <- parse_spec_string(yml))[["elapsed"]]
  expect_length(spec$data$d, 20000)
  expect_lt(elapsed, 10)
  expect_identical(spec$data$d[[5]]$b, "5")   # quoted digits stay strings
  expect_true(is.numeric(spec$data$d[[5]]$c))
})
