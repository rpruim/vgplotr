test_that("vg_data() auto-generates a name when omitted", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3)
  spec <- vg_create() |> vg_data(data = df)
  expect_equal(names(spec$data), "data1")

  spec <- spec |> vg_data(data = df)
  expect_equal(names(spec$data), c("data1", "data2"))

  spec <- spec |> vg_data(data = df)
  expect_equal(names(spec$data), c("data1", "data2", "data3"))
})

test_that("vg_create(data =) registers a data frame as the spec's first data source", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3)
  spec <- vg_create(data = df)
  expect_equal(names(spec$data), "data1")
  expect_equal(spec$data$data1$data, df)
})

test_that("a data frame piped as spec into a mark auto-registers it and sets data_from", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |> vg_mark_dot(x = ~a, y = ~b)

  expect_true(is_vgspec(spec))
  expect_equal(names(spec$data), "data1")
  expect_equal(spec$data$data1$data, df)
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "data1")

  payload <- as_spec_payload(spec)
  expect_equal(payload$spec$plot[[1]]$data$from, "data1")
})

test_that("an explicit data_from string on the data-frame shorthand names the registered source", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |> vg_mark_dot(data_from = "my_df", x = ~a, y = ~b)

  expect_equal(names(spec$data), "my_df")
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "my_df")
})

test_that("a second mark can reference the auto-registered data source by name", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_mark_line_y(data_from = "data1", x = ~a, y = ~b)

  expect_equal(names(spec$data), "data1")
  expect_length(spec$layout$items, 2)
  expect_equal(spec$layout$items[[2]]$encodings$data_from, "data1")
})

test_that("the data-frame shorthand is equivalent to vg_create() |> vg_data(data = df) |> vg_mark_*()", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  via_shorthand <- df |> vg_mark_dot(x = ~a, y = ~b)
  reset_auto_data_names() # each construction starts fresh, so both land on "data1"
  via_explicit <- vg_create() |> vg_data(data = df) |> vg_mark_dot(data_from = "data1", x = ~a, y = ~b)

  expect_equal(via_shorthand, via_explicit)
})

test_that("a mark with no data_from defaults to the spec's first registered data source", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))

  # data-frame-piped first mark, then a second mark with no data_from at all
  spec <- df |> vg_mark_dot(x = ~a, y = ~b) |> vg_mark_line_y(x = ~a, y = ~b)
  expect_equal(spec$layout$items[[2]]$encodings$data_from, "data1")

  # same default via the explicit vg_create()/vg_data() path
  spec2 <- vg_create() |> vg_data(name = "d1", data = df) |> vg_mark_dot(x = ~a, y = ~b) |> vg_mark_line_y(x = ~a, y = ~b)
  expect_equal(spec2$layout$items[[1]]$encodings$data_from, "d1")
  expect_equal(spec2$layout$items[[2]]$encodings$data_from, "d1")

  # with more than one data source registered, it's the *first* one that wins
  spec3 <- vg_create() |>
    vg_data(name = "first", data = df) |>
    vg_data(name = "second", data = df) |>
    vg_mark_dot(x = ~a, y = ~b)
  expect_equal(spec3$layout$items[[1]]$encodings$data_from, "first")
})

test_that("an explicit data_from always wins over the first-data-source default", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- vg_create() |>
    vg_data(name = "first", data = df) |>
    vg_data(name = "second", data = df) |>
    vg_mark_dot(data_from = "second", x = ~a, y = ~b)
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "second")
})

test_that("marks with no `data` property (frame, sphere, hexgrid, axis/grid marks) never get a default data_from", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_mark_frame(stroke = "#ccc") |>
    vg_mark_sphere() |>
    vg_mark_hexgrid()

  expect_null(spec$layout$items[[2]]$encodings$data_from)
  expect_null(spec$layout$items[[3]]$encodings$data_from)
  expect_null(spec$layout$items[[4]]$encodings$data_from)

  payload <- as_spec_payload(spec)
  expect_null(payload$spec$plot[[2]]$data)
  expect_null(payload$spec$plot[[3]]$data)
  expect_null(payload$spec$plot[[4]]$data)
})

test_that("a mark with `data` support but only literal args doesn't get a default data_from", {
  reset_auto_data_names()
  # e.g., vg_mark_rule_x(x = 0, ...) as a plain reference line layered on a
  # data-bound plot -- unlike frame/sphere/etc. (which never have a `data`
  # property at all), ruleX *can* be data-bound, but nothing here
  # references a column, so attaching data_from would make mosaic try to
  # query the table with no selection list (confirmed: this used to
  # silently blank the whole plot).
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |>
    vg_mark_dot(x = ~a, y = ~b) |>
    vg_mark_rule_x(x = 0, stroke = "firebrick")

  expect_null(spec$layout$items[[2]]$encodings$data_from)
  payload <- as_spec_payload(spec)
  expect_null(payload$spec$plot[[2]]$data)
  expect_equal(payload$spec$plot[[2]]$x, 0)
})

test_that("a mark using sql()/agg() unwrapped still gets a default data_from", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |> vg_mark_dot(x = sql("a + 1"), y = ~b)
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "data1")
})

test_that("a mark using a transform function unwrapped still gets a default data_from", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  spec <- df |> vg_mark_rect_y(x = vg_bin(a, step = 1), y = vg_count())
  expect_equal(spec$layout$items[[1]]$encodings$data_from, "data1")
})

test_that("a mark using only param() references doesn't get a default data_from", {
  reset_auto_data_names()
  # a param is a reactive scalar, not a per-row data lookup
  spec <- vg_create() |>
    vg_data(name = "d1", data = data.frame(a = 1:3)) |>
    vg_mark_rule_x(x = param(threshold), stroke = "firebrick")
  expect_null(spec$layout$items[[1]]$encodings$data_from)
})

test_that("no default data_from is applied when the spec has no data source yet", {
  reset_auto_data_names()
  frag <- vg_mark_dot(x = ~a, y = ~b)
  expect_null(frag$items[[1]]$encodings$data_from)
})

test_that("data_from accepts a length-1 integer index into the spec's registered data sources", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3)
  spec <- vg_create() |>
    vg_data(name = "first", data = df) |>
    vg_data(name = "second", data = df) |>
    vg_data(name = "third", data = df)

  expect_equal((spec |> vg_mark_dot(data_from = 1L, x = ~a))$layout$items[[1]]$encodings$data_from, "first")
  expect_equal((spec |> vg_mark_dot(data_from = 2L, x = ~a))$layout$items[[1]]$encodings$data_from, "second")
  expect_equal((spec |> vg_mark_dot(data_from = 3L, x = ~a))$layout$items[[1]]$encodings$data_from, "third")
  expect_equal((spec |> vg_mark_dot(data_from = -1L, x = ~a))$layout$items[[1]]$encodings$data_from, "third")
  expect_equal((spec |> vg_mark_dot(data_from = -2L, x = ~a))$layout$items[[1]]$encodings$data_from, "second")
  expect_equal((spec |> vg_mark_dot(data_from = -3L, x = ~a))$layout$items[[1]]$encodings$data_from, "first")
})

test_that("an out-of-range integer data_from index errors clearly", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3)
  spec <- vg_create() |> vg_data(name = "only", data = df)

  expect_error(spec |> vg_mark_dot(data_from = 2L, x = ~a), "not a valid data source index")
  expect_error(spec |> vg_mark_dot(data_from = -2L, x = ~a), "not a valid data source index")
})

test_that("data_from = 0L is never an index -- it's literal inline data, like data_from = 0", {
  reset_auto_data_names()
  # 0 is never a valid 1-based index (there's no "0th" source), so 0L is
  # deliberately treated the same as the double 0 -- mosaic's own literal
  # inline-data shorthand -- rather than erroring.
  spec <- vg_mark_rule_y(data_from = 0L)
  expect_equal(serialize_item(spec$items[[1]])$data, list(0L))

  df <- data.frame(a = 1:3)
  spec2 <- vg_create() |> vg_data(name = "only", data = df) |> vg_mark_dot(data_from = 0L, x = ~a)
  expect_equal(spec2$layout$items[[1]]$encodings$data_from, 0L)
})

test_that("an integer data_from index with no data source registered yet errors clearly", {
  reset_auto_data_names()
  expect_error(vg_mark_dot(data_from = 1L, x = ~a), "needs at least one")
})

test_that("a bare (double) numeric data_from is still mosaic's literal inline-data shorthand, not an index", {
  reset_auto_data_names()
  # data_from = 0 (a double, not 0L) must keep meaning "inline data [0]",
  # e.g., for a single reference line -- only a strict R integer (1L, -1L,
  # ...) is treated as a data-source index. This is what data_from = c(0)
  # already relied on before the index feature existed.
  spec <- vg_mark_rule_y(data_from = c(0))
  expect_equal(serialize_item(spec$items[[1]])$data, list(0))

  spec2 <- vg_mark_rule_y(data_from = c(0, 10, 20))
  expect_equal(serialize_item(spec2$items[[1]])$data, list(0, 10, 20))
})

test_that("the default data_from (unset) is equivalent to data_from = 1L", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3)
  spec <- vg_create() |> vg_data(name = "first", data = df) |> vg_data(name = "second", data = df)

  via_default <- spec |> vg_mark_dot(x = ~a)
  via_explicit <- spec |> vg_mark_dot(data_from = 1L, x = ~a)
  expect_equal(via_default, via_explicit)
})

test_that("auto_data_name() never repeats a name across independent specs in the same session", {
  # The bug this guards against: build_mark()'s data-frame shortcut always
  # starts from a brand-new, empty vg_create(), so spec$data alone can
  # never tell two *separate* specs apart. Without a session-wide registry,
  # two unrelated `df |> vg_mark_dot(...)` shortcuts always both got named
  # "data1" -- harmless on their own, but once both specs are rendered onto
  # the same page (every vg_render() call shares one DuckDB instance), the
  # second one's table silently overwrites the first's, and whichever plot
  # queries afterward gets a confusing "column not found" error that
  # points nowhere near the real cause. Confirmed directly against the
  # getting-started vignette before this fix.
  reset_auto_data_names()
  df1 <- data.frame(a = 1:3, b = c(4, 5, 6))
  df2 <- data.frame(x = 1:3, y = c("p", "q", "r"))

  spec1 <- df1 |> vg_mark_dot(x = ~a, y = ~b)
  spec2 <- df2 |> vg_mark_dot(x = ~x, y = ~y)

  expect_false(identical(names(spec1$data), names(spec2$data)))
  expect_equal(names(spec1$data), "data1")
  expect_equal(names(spec2$data), "data2")
  # each spec's own mark still correctly points at its own (correctly
  # renamed) source, not the literal "data1" the shortcut always used to
  # produce
  expect_equal(spec2$layout$items[[1]]$encodings$data_from, "data2")
})

test_that("auto_data_name() session-wide uniqueness also covers vg_data(name = NULL) directly, not just the mark shortcut", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3)
  spec1 <- vg_create() |> vg_data(data = df)
  spec2 <- vg_create() |> vg_data(data = df) # a separate spec, also unnamed

  expect_equal(names(spec1$data), "data1")
  expect_equal(names(spec2$data), "data2")
})

test_that("reset_auto_data_names() clears the session-wide registry", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3)
  vg_create() |> vg_data(data = df) # claims "data1"

  reset_auto_data_names()
  spec <- vg_create() |> vg_data(data = df)
  expect_equal(names(spec$data), "data1") # "data1" is available again
})

test_that("vg_data_registry() is an empty 3-column data frame when nothing has been registered yet", {
  reset_auto_data_names()
  reg <- vg_data_registry()
  expect_equal(names(reg), c("name", "kind", "source"))
  expect_equal(nrow(reg), 0)
})

test_that("vg_data_registry() records name/kind/source for every source kind", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3, b = 4:6)
  vg_create() |> vg_data(name = "inline", data = df)
  vg_create() |> vg_data(name = "local_file", file = "data/stocks.csv")
  vg_create() |> vg_data(name = "remote", file = "https://example.com/data.parquet")
  vg_create() |> vg_data(name = "sql", query = "SELECT * FROM t")

  reg <- vg_data_registry()
  by_name <- setNames(split(reg[c("kind", "source")], seq_len(nrow(reg))), reg$name)

  expect_equal(by_name$inline$kind, "local data")
  expect_equal(by_name$inline$source, "data.frame [3 x 2]")
  expect_equal(by_name$local_file$kind, "file")
  expect_equal(by_name$local_file$source, "data/stocks.csv")
  expect_equal(by_name$remote$kind, "url")
  expect_equal(by_name$remote$source, "https://example.com/data.parquet")
  expect_equal(by_name$sql$kind, "query")
  expect_equal(by_name$sql$source, "SELECT * FROM t")
})

test_that("vg_data_registry() picks up both auto-generated and explicit names", {
  reset_auto_data_names()
  df <- data.frame(a = 1:3)
  df |> vg_mark_dot(x = ~a) # auto-named "data1"
  vg_create() |> vg_data(name = "explicit", data = df)

  expect_setequal(vg_data_registry()$name, c("data1", "explicit"))
})

test_that("vg_data_registry() reflects only the most recent registration when a name is reused", {
  reset_auto_data_names()
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 1:5, c = 1:5) # a different shape, registered under the same name
  vg_create() |> vg_data(name = "shared", data = df1)
  vg_create() |> vg_data(name = "shared", data = df2)

  reg <- vg_data_registry()
  expect_equal(nrow(reg), 1)
  expect_equal(reg$source, "data.frame [5 x 2]")
})
