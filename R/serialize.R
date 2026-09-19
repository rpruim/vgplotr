# Converts a vgspec into the shape needed to render it: a mosaic-spec JSON
# object (as a plain R list, ready for htmlwidgets/jsonlite), plus a set of
# tables and local files to load into DuckDB *before* that spec is parsed.
#
# Data frames are deliberately kept out of the spec's own `data:` block and
# loaded explicitly beforehand (see vg_render()/inst/htmlwidgets/vgplotr.js)
# rather than relying on mosaic-spec's own declarative data loading -- in
# testing, astToDOM() would sometimes query a mark's table before an inline
# `data: [...]` array had finished loading, and this preload avoids that
# race.
#
# Local files (a `file =` value that isn't a fully-qualified http(s) URL)
# are handled the same way, but for a different reason: duckdb-wasm can
# only *fetch* http(s) URLs, so any relative/local path referenced directly
# in the spec's `data:` block fails whenever the rendered page is opened as
# a plain file (`file://`, e.g., a Positron/RStudio viewer or just opening
# the .html directly) rather than served over HTTP. Reading the file here
# and embedding its bytes sidesteps that entirely -- it's loaded into
# DuckDB from memory, with no fetch involved, so it works the same way
# regardless of how the page itself was opened. A genuine http(s) URL is
# left in the spec's `data:` block, since mosaic's own loading already
# handles that reliably (tested directly) and fetching it eagerly here
# would be wrong for a large or possibly-updated-later remote file.
#
# A JSON/YAML *string* spec gets the same treatment for inline row data: an
# already-complete spec's own `data: {name: {data: [{...}, ...]}}` (or bare
# `name: [{...}, ...]`) block is exactly the racy path described above, so
# lift_inline_data_sources() pulls those out of the spec and into `tables`
# (as the plain row-object lists they already are). Every other kind of data
# source in a string spec (file=, query=, ...) is left exactly as given.
#' @noRd
as_spec_payload <- function(spec) {
  if (is.character(spec)) {
    parsed <- parse_spec_string(spec)
    lifted <- lift_inline_data_sources(parsed$data)
    parsed$data <- if (length(lifted$remaining)) lifted$remaining
    return(list(spec = parsed, tables = lifted$tables, files = list()))
  }
  stopifnot(is_vgspec(spec))
  if (is.null(spec$layout)) {
    stop("This vgspec doesn't have any plots yet.", call. = FALSE)
  }

  tables <- list()
  files <- list()
  data_entries <- list()
  for (nm in names(spec$data)) {
    src <- spec$data[[nm]]
    kind <- vg_data_source_kind(src)
    if (kind == "table") {
      warn_ordered_factor_cols(src$data, nm)
      tables[[nm]] <- src$data
    } else if (kind == "local_file") {
      file_info <- read_local_data_file(src$file)
      file_info$options <- src[setdiff(names(src), c("file", "type"))]
      files[[nm]] <- file_info
    } else {
      data_entries[[nm]] <- serialize_data_source(src, nm)
    }
  }

  out <- list()
  if (length(spec$meta)) out$meta <- spec$meta
  if (length(spec$config)) out$config <- spec$config
  if (length(data_entries)) out$data <- data_entries
  if (length(spec$params)) out$params <- serialize_params(spec$params)
  out <- c(out, serialize_layout(spec$layout, spec$plot_defaults))
  # spec$attrs (from vg_attributes()) are the spec's own top-level
  # attributes -- merged once here, at the very top, as opposed to
  # spec$plot_defaults just above, which serialize_layout() already
  # threaded into *every* plot in the tree.
  if (length(spec$attrs)) out <- merge_attrs(out, lapply(spec$attrs, serialize_value), context = "vg_attributes()")

  list(spec = out, tables = tables, files = files)
}

# Parses a length-1 JSON or YAML string -- an already-complete mosaic-spec
# (e.g., copied from mosaic's own example gallery, or from to_json()/
# to_yaml()) -- into a plain nested list in the same shape as_spec_payload()
# builds from a vgspec. Format is auto-detected: trimmed text starting with
# `{` or `[` is parsed as JSON, anything else as YAML (YAML has no such
# marker, but every JSON document is also valid YAML, so this only matters
# for picking error messages). `simplifyVector = FALSE` keeps
# jsonlite::fromJSON() from ever coercing an array of mark/plot objects into
# a data.frame, which would silently break re-serialization.
parse_spec_string <- function(text) {
  if (length(text) != 1 || is.na(text)) {
    stop(
      "`spec` must be a single JSON/YAML string (or a vgspec), not ",
      "length ", length(text), ".",
      call. = FALSE
    )
  }
  is_json <- grepl("^[[{]", trimws(text))
  parsed <- tryCatch(
    if (is_json) {
      jsonlite::fromJSON(text, simplifyVector = FALSE)
    } else {
      yaml::yaml.load(text, handlers = yaml_handlers(text))
    },
    error = function(e) {
      stop(
        "Couldn't parse `spec` as ", if (is_json) "JSON" else "YAML", ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  is_object <- is.list(parsed) && (length(parsed) == 0 || !is.null(names(parsed)))
  if (!is_object) {
    stop(
      "`spec` must parse to a JSON/YAML object (a mapping of keys like ",
      "`plot`/`meta`/`data`), not a bare ",
      if (is.list(parsed)) "array" else class(parsed)[1], ".",
      call. = FALSE
    )
  }
  parsed
}

# The `yaml` package implements YAML 1.1, which resolves a handful of short,
# unquoted words as booleans -- y/n/yes/no/on/off (any case) on top of
# true/false -- but the JavaScript YAML parsers a mosaic spec is written for
# (`yaml` and `js-yaml`, both checked directly on the same inputs, and both
# agreeing exactly) follow YAML 1.2, where *only* true/True/TRUE and false/
# False/FALSE are booleans and everything else is a plain string. That
# matters for the very common `y` (as in the x/y encoding channel): an
# unquoted `y` silently became the logical TRUE -- as a mapping key, R then
# coerces it to the list name "TRUE" -- and equally as a *value* (`x: y`, a
# column named y; a legend `label: Y`; `channels: [x, y]`). The two are the
# same bug, so this fixes both at the parser: yaml.load()'s `bool#yes`/
# `bool#no` handlers fire for every implicit boolean (keys and values, block
# and flow style alike) and are given the original text, so resolving it the
# 1.2 way is just: the six real spellings become logicals, and anything else
# stays the string it was written as.
#
# (An earlier version of this file rewrote the *text* with a regex to quote
# `y:` at the start of a line. That missed flow mappings (`{x: a, y: b}`) and
# every value, and, being blind to context, even rewrote prose inside a `|`
# block scalar. It had also concluded that handlers can't reach implicit
# booleans -- true only of the handler name it tried, `bool`; the actual
# names are `bool#yes` and `bool#no`.)
yaml_bool_handler <- function(x) {
  if (x %in% c("true", "True", "TRUE")) {
    TRUE
  } else if (x %in% c("false", "False", "FALSE")) {
    FALSE
  } else {
    x
  }
}

# The `yaml` package is YAML 1.1; the JavaScript parsers a mosaic spec is
# written for are YAML 1.2 (see yaml_bool_handler() above). Beyond booleans,
# that leaves four places where a pasted YAML spec would otherwise be read
# differently from how mosaic's own tooling reads it, all fixed here so the
# YAML path gives the same structure as the JSON path:
#
#  * `seq`: a sequence of same-typed scalars is normally *simplified* to an
#    atomic vector, and a length-1 vector is then written to JSON as a bare
#    scalar -- `data: [0]` (mosaic's inline-data shorthand, e.g. a reference
#    line) became `data: 0`, and `channels: [id]` became `"id"`. Supplying any
#    `seq` handler switches the simplification off (probed directly: the
#    handler receives the plain list), so sequences stay lists, exactly like
#    the JSON path's `simplifyVector = FALSE`.
#  * exponent floats (`9.75e5`, `1e5`, `1e-3`, `.5e3`): 1.1 only recognises an
#    exponent that has both a sign and a decimal point, so these stayed strings
#    (`xDomain: [9.75e5, 1.0e6]` was sent as two strings) where 1.2 has numbers.
#  * leading-zero integers: `0755` is 1.1 octal (493), but 1.2 reads it as
#    decimal 755 -- handled by the `int#oct` handler -- and `089`, invalid
#    octal, stayed a string.
#  * (Unchanged, and already agreeing with `yaml`: timestamps stay strings,
#    `1:30` stays a string, `0x1F` is 31.)
#
# The exponent-float and `089` cases can only be caught in the `str` handler
# (that's where an unrecognised plain scalar ends up), and that handler cannot
# tell a plain `1e5` from a deliberately *quoted* "1e5" -- both arrive as the
# same string. So it also refuses to convert any string whose quoted form
# ("1e5" or '1e5') appears in the source text: a guard, not a rewrite, and the
# safe direction to be wrong in (it keeps the old string behaviour). The set of
# quoted number-like tokens is collected once per parse, so the guard costs one
# pass over the text rather than one search per string.
yaml_handlers <- function(text) {
  quoted <- quoted_number_like_tokens(text)
  list(
    int = yaml_int_handler,
    "int#oct" = yaml_int_handler,
    "bool#yes" = yaml_bool_handler,
    "bool#no" = yaml_bool_handler,
    seq = function(x) x,
    str = function(x) yaml_str_handler(x, quoted)
  )
}

# Every quoted scalar in `text` made only of characters a number can contain,
# quotes stripped -- "1e5", '089', "2020", ... -- for yaml_str_handler()'s guard.
quoted_number_like_tokens <- function(text) {
  m <- gregexpr("\"[-+.0-9eE]+\"|'[-+.0-9eE]+'", text)
  tokens <- regmatches(text, m)[[1]]
  unique(substr(tokens, 2L, nchar(tokens) - 1L))
}

# Resolves an unrecognised plain scalar the YAML 1.2 way: an exponent float
# (`9.75e5`) or a leading-zero decimal integer (`089`) is a number. Anything
# else -- and any string that was quoted somewhere in the source, `quoted` --
# is left alone.
yaml_str_handler <- function(x, quoted) {
  is_exponent_float <- grepl("^[-+]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)[eE][-+]?[0-9]+$", x)
  is_decimal_int <- grepl("^[-+]?[0-9]+$", x)
  if ((is_exponent_float || is_decimal_int) && !(x %in% quoted)) {
    if (is_decimal_int) yaml_int_handler(x) else as.numeric(x)
  } else {
    x
  }
}

# yaml::yaml.load()'s default implicit-integer resolution parses a
# YAML-1.1 integer-looking scalar via R's as.integer(), which silently
# overflows to NA for anything past 32-bit range (confirmed directly:
# `yaml.load("x: 1706227200000")` -- a plain millisecond epoch timestamp,
# e.g. a real xDomain value copied from one of mosaic's own example
# specs -- returns `x = NA`, with only a warning easy to miss). Unlike
# the bool-word hazard above (where a custom handler is documented as
# never firing for *implicit* resolution), a custom "int" handler *does*
# fire here -- confirmed directly -- so falling back to as.numeric() when
# as.integer() overflows preserves the value instead of losing it.
yaml_int_handler <- function(x) {
  n <- suppressWarnings(as.integer(x))
  if (is.na(n) && !is.na(x)) as.numeric(x) else n
}

# Splits a parsed string spec's `data:` block into the sources vgplotr must
# preload itself -- inline arrays of row objects, in either of mosaic-spec's
# two shapes for them (a bare array, `DataArray`; or an object holding just
# `data:` and optionally `type: json`, `DataJSONObjects`) -- and everything
# else, which stays in the spec. Returns `tables` (name -> list of row
# objects, ready for loadTables() in inst/htmlwidgets/vgplotr.js) and
# `remaining` (the untouched other sources).
#
# An inline source that *also* carries load-time options (`where`, `select`,
# `temp`, ...) is deliberately left in the spec too: loadTables() has no
# way to apply them, and dropping them silently would change what the plot
# shows. Same for an empty array or rows that aren't all objects -- there's
# no reliable table to build from those, so mosaic's own loader is left to
# deal with (or reject) them.
lift_inline_data_sources <- function(data) {
  tables <- list()
  remaining <- list()
  for (nm in names(data)) {
    rows <- inline_data_rows(data[[nm]])
    if (is.null(rows)) {
      remaining[nm] <- list(data[[nm]])
    } else {
      tables[[nm]] <- rows
    }
  }
  list(tables = tables, remaining = remaining)
}

# The row objects of an inline data source (see lift_inline_data_sources()),
# or NULL if `src` isn't a plain one.
inline_data_rows <- function(src) {
  rows <- if (is.null(names(src))) {
    src
  } else if (all(names(src) %in% c("data", "type")) && !is.null(src$data) &&
             (is.null(src$type) || identical(src$type, "json"))) {
    src$data
  }
  is_row <- function(r) is.list(r) && length(r) > 0 && !is.null(names(r))
  if (is.list(rows) && is.null(names(rows)) && length(rows) > 0 && all(vapply(rows, is_row, logical(1)))) {
    rows
  }
}

# Classifies a spec's data source the way vg_render() needs to: "table" (an
# inline data frame) and "local_file" (a file= that isn't an http(s) URL)
# are the two kinds vgplotr must preload itself before mosaic's own runtime
# ever sees the spec (see as_spec_payload()'s own comment for why); "other"
# (query=, a genuine http(s) file=, ...) is already connector-agnostic and
# needs no special handling. Shared by as_spec_payload() (wasm: embeds
# tables/local files for the browser to load) and
# register_native_data_sources() (R/duckdb_server.R; native: registers them
# directly into a real DuckDB connection instead).
vg_data_source_kind <- function(src) {
  if (!is.null(src$data) && is.data.frame(src$data)) {
    "table"
  } else if (!is.null(src$file) && !grepl("^https?://", src$file, ignore.case = TRUE)) {
    "local_file"
  } else {
    "other"
  }
}

# Reads a local data file into a form embeddable in the htmlwidget payload:
# raw text for csv/json (read directly by DuckDB via registerFileText), or
# base64 for parquet (binary, via registerFileBuffer). Type is inferred
# from the file extension, mirroring mosaic-spec's own inference rule.
read_local_data_file <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Data file not found: '", path, "' (looked relative to the current ",
      "working directory, ", getwd(), ").",
      call. = FALSE
    )
  }
  ext <- tolower(tools::file_ext(path))
  bytes <- readBin(path, "raw", file.info(path)$size)
  if (ext == "parquet") {
    list(ext = ext, encoding = "base64", content = jsonlite::base64_enc(bytes))
  } else if (ext %in% c("csv", "json")) {
    list(ext = ext, encoding = "text", content = rawToChar(bytes))
  } else {
    stop(
      "Don't know how to load a local data file with extension `.", ext, "` ",
      "(expected .csv, .json, or .parquet).",
      call. = FALSE
    )
  }
}

# Recursively serializes a layout node -- a single plot, a vconcat/hconcat
# of further layout nodes, or a spacer. `plot_defaults` (mosaic-spec's real
# `plotDefaults`, set via vg_plot_defaults()) is threaded down and merged
# into *every* plot found in the tree (the plot's own attributes win on
# conflict) rather than emitted as a separate top-level `plotDefaults` key
# here -- simpler, and equivalent as long as plotDefaults is only ever used
# for static attributes like width/height. This is distinct from a spec's
# own top-level attrs (set via vg_attributes()), which the caller
# (as_spec_payload()/spec_to_list()) merges in exactly once, at the very
# top, rather than threading down into every plot.
serialize_layout <- function(layout, plot_defaults = list()) {
  if (is_vg_plot_fragment(layout)) {
    attrs <- override_attrs(plot_defaults, layout$attrs)
    c(list(plot = lapply(layout$items, serialize_item)), lapply(attrs, serialize_value))
  } else if (is_vg_concat(layout)) {
    children <- lapply(layout$children, serialize_layout, plot_defaults = plot_defaults)
    named_list(layout$direction, children)
  } else if (is_vg_space(layout)) {
    named_list(layout$direction, layout$amount)
  } else if (inherits(layout, "vg_input")) {
    serialize_input(layout)
  } else if (inherits(layout, "vg_legend")) {
    serialize_legend(layout)
  } else {
    stop(
      "Don't know how to serialize a layout of class ",
      paste(class(layout), collapse = "/"), ".",
      call. = FALSE
    )
  }
}

serialize_item <- function(item) {
  if (inherits(item, "vg_mark")) {
    c(list(mark = item$mark), serialize_encodings(item$encodings))
  } else if (inherits(item, "vg_interactor")) {
    c(list(select = item$type), serialize_encodings(item$options))
  } else if (inherits(item, "vg_legend")) {
    serialize_legend(item)
  } else {
    stop(
      "Don't know how to serialize an object of class ",
      paste(class(item), collapse = "/"), ".",
      call. = FALSE
    )
  }
}

serialize_input <- function(x) {
  c(list(input = x$type), serialize_encodings(x$options))
}

serialize_legend <- function(x) {
  out <- c(list(legend = x$type), serialize_encodings(x$options))
  if (!is.null(x$for_plot)) out[["for"]] <- x$for_plot
  out
}

# Pulls data_from/filter_by/data_optimize out of a mark's encodings into
# the nested `data: {from:, filterBy:, optimize:}` object mosaic-spec
# expects, and translates each remaining value (formulas, param()
# references) to plain JSON-able values.
#
# `data_from` doubles as a way to supply a literal inline data array (e.g.,
# `data_from = c(0)` for a single reference line, mosaic-spec's `"data":
# [0]` shorthand -- distinct from the `{"data": {"from": name}}` form used
# for a named data source): if it isn't a single string, it's treated as
# literal values rather than a table name. `as.list()` (not the bare
# vector) guarantees this serializes as a JSON array even when it has only
# one element -- jsonlite's `auto_unbox` would otherwise turn a length-1
# atomic vector into a bare scalar.
serialize_encodings <- function(enc) {
  data_from <- enc$data_from
  filter_by <- enc$filter_by
  data_optimize <- enc$data_optimize
  enc$data_from <- NULL
  enc$filter_by <- NULL
  enc$data_optimize <- NULL

  out <- lapply(enc, serialize_value)

  # A table reference is either a table-name string or a Param/Selection
  # (e.g., `data_from = param(data)`, for a menu-driven dynamic data source)
  # -- anything else (a literal vector) is mosaic-spec's inline-data
  # shorthand instead.
  is_table_ref <- (is.character(data_from) && length(data_from) == 1) || is_vg_param(data_from)

  if (!is.null(data_from) && !is_table_ref) {
    out <- c(list(data = as.list(data_from)), out)
  } else if (!is.null(data_from) || !is.null(filter_by) || !is.null(data_optimize)) {
    data_obj <- list()
    if (!is.null(data_from)) data_obj$from <- serialize_value(data_from)
    if (!is.null(filter_by)) data_obj$filterBy <- serialize_value(filter_by)
    if (!is.null(data_optimize)) data_obj$optimize <- data_optimize
    out <- c(list(data = data_obj), out)
  }
  out
}

# A param's own declared value (spec$params, from vg_params()) isn't just
# a plain scalar or Selection/ParamDate-shaped list ({select: ...}, {date:
# ...}) -- mosaic also allows one param to be built from *other* param
# references, e.g., `rotate = list(param(longitude), param(latitude))`
# (mosaic's own `rotate: [$longitude, $latitude]` pattern, combining two
# sliders into one projectionRotate value). Recurses into any nested list
# so a vg_param anywhere inside turns into its "$name" string; everything
# else (plain scalars, an ordinary Selection/ParamDate list) passes
# through unchanged.
serialize_param_value <- function(x) {
  if (is_vg_param(x)) {
    format(x)
  } else if (is.list(x)) {
    lapply(x, serialize_param_value)
  } else {
    x
  }
}

serialize_params <- function(params) {
  lapply(params, serialize_param_value)
}

serialize_value <- function(x) {
  if (inherits(x, "formula")) {
    serialize_formula(x)
  } else if (is_vg_param(x)) {
    format(x)
  } else if (is_vg_transform(x)) {
    serialize_transform(x)
  } else if (is_vg_sql_expr(x)) {
    serialize_sql_expr(x)
  } else {
    x
  }
}

serialize_sql_expr <- function(x) {
  out <- named_list(x$key, x$text)
  if (!is.null(x$label)) out$label <- x$label
  out
}

serialize_formula <- function(f) {
  serialize_expr(f[[2]], environment(f))
}

# Recognizes a mapping formula's RHS: a bare column-name symbol, a literal,
# a call to sql()/agg()/param() (evaluated directly -- safe, since unlike a
# transform's bare column-reference arguments, sql()/agg()'s own arguments
# are self-contained string/param() pieces, not free variables that would
# fail to evaluate, and param()'s own argument is just a bare name captured
# via rlang::ensym(), never looked up), or a call to one of the
# vg_transform_specs functions (vg_bin(), vg_count(), ...) -- recognized
# here purely syntactically, via match.call() against the real function's
# formals, without ever evaluating the call itself (which would fail, since
# e.g., `delay` in `~vg_bin(delay)` isn't a bound variable).
serialize_expr <- function(expr, env) {
  if (is.symbol(expr)) {
    # Usually a bare column-name reference that isn't a bound R variable at
    # all (e.g., `delay` in `~vg_bin(delay)`) -- but it can also be a
    # variable holding a param()/selection object (e.g., `xp <- param(x);
    # ~vg_column(xp)`), so try evaluating it first and use that value if
    # it resolves to one; otherwise fall back to the symbol's own name.
    val <- tryCatch(eval(expr, envir = env), error = function(e) NULL)
    if (is_vg_param(val)) return(serialize_value(val))
    as.character(expr)
  } else if (is.call(expr)) {
    # What a call inside a formula *is* -- sql()/agg()/param(), or which
    # transform, with its arguments matched -- is decided by
    # resolve_formula_call() (R/transforms.R), shared with the early check
    # check_formula_expr() that runs when a mark is built, so the two can't
    # drift apart. Only the evaluation happens here, at serialization.
    resolved <- resolve_formula_call(expr)
    if (resolved$kind == "eval") {
      return(serialize_value(eval(expr, envir = env)))
    }
    serialize_transform(build_vg_transform(resolved$spec, resolved$mc, env))
  } else if (is.numeric(expr) || is.character(expr) || is.logical(expr) || is.null(expr)) {
    expr
  } else {
    stop("Can't use this inside a mapping formula: ", deparse(expr), call. = FALSE)
  }
}

serialize_transform <- function(x) {
  field_json <- lapply(x$field, serialize_expr, env = x$env)
  value <- if (length(field_json) == 0) NULL
    else if (length(field_json) == 1) field_json[[1]]
    else field_json
  c(named_list(x$key, value), x$options)
}

# Converts a vgspec or bare layout fragment (a plot fragment, a
# vconcat()/hconcat(), an hspace()/vspace(), or a standalone legend/input)
# into mosaic-spec's own portable JSON/YAML shape, as a plain nested list --
# used by to_json()/to_yaml() (see R/export.R). Unlike as_spec_payload()
# (vg_render()'s own widget payload), this never rewrites data sources for
# the render pipeline's benefit: a data frame becomes mosaic-spec's own
# inline `{data: [...]}` array-of-row-objects form (its documented
# `DataJSONObjects` shape) instead of being pulled out into a separate
# `tables` list, and a `file =`/`query =` source is left exactly as given
# rather than having its content read and embedded -- there's no widget to
# embed it into, and the point is a spec a human could write by hand or
# feed to mosaic's own tools. `plotDefaults` is also emitted as its own
# top-level key here (mosaic-spec's real `SpecHead.plotDefaults`), rather
# than merged into every plot the way as_spec_payload() simplifies it --
# spec$attrs (the spec's own top-level attrs, from vg_attributes()) are a
# separate thing again, merged in once at the very top, exactly like
# as_spec_payload() does.
spec_to_list <- function(spec, suppress_data = FALSE) {
  if (is_vgspec(spec)) {
    if (is.null(spec$layout)) {
      stop("This vgspec doesn't have any plots yet.", call. = FALSE)
    }
    out <- list()
    if (length(spec$meta)) out$meta <- spec$meta
    if (length(spec$config)) out$config <- spec$config
    data_sources <- spec$data
    if (suppress_data) data_sources <- Filter(Negate(is_inline_data_source), data_sources)
    if (length(data_sources)) out$data <- Map(serialize_data_source, data_sources, names(data_sources))
    if (length(spec$params)) out$params <- serialize_params(spec$params)
    if (length(spec$plot_defaults)) out$plotDefaults <- lapply(spec$plot_defaults, serialize_value)
    out <- c(out, serialize_layout(spec$layout))
    if (length(spec$attrs)) out <- merge_attrs(out, lapply(spec$attrs, serialize_value), context = "vg_attributes()")
    out
  } else {
    serialize_layout(spec)
  }
}

serialize_data_source <- function(src, name = "?") {
  if (!is.null(src$data) && is.data.frame(src$data)) {
    # A plain `src$data <- ...; src` replacement, not utils::modifyList() --
    # modifyList() recurses into any element where *both* the old and new
    # values satisfy is.list(), and an R data.frame is itself a list, so
    # modifyList(src, list(data = <rows>)) tries to recursively merge the
    # original column-major data.frame with the new row-major list by name.
    # df_to_rows()'s row list is unnamed, so that merge matches nothing and
    # silently returns src$data completely unchanged (confirmed directly).
    src$data <- df_to_rows(src$data, name)
    src
  } else if (!is.null(src$query) && length(src) == 1) {
    # mosaic-spec's DataQuery is a bare SQL string ("name": "SELECT ..."),
    # not an object -- unlike DataFile/DataTable/etc., which really are
    # objects ({file: ..., where: ...}). vg_data(name, query = "...") is
    # the only vg_data() form this applies to (a lone `query` option).
    src$query
  } else {
    src
  }
}

# A data source defined *with* the spec (an inline data frame, vg_data(name,
# data = df) -- serialized as mosaic-spec's own {data: [...]} row-object
# array) as opposed to one specified *by name* elsewhere (file =, query =,
# type = "spatial", ...). Used by to_json()/to_yaml()'s `suppress_data =`.
is_inline_data_source <- function(src) {
  !is.null(src$data) && is.data.frame(src$data)
}

df_to_rows <- function(df, name = "?") {
  warn_ordered_factor_cols(df, name)
  df <- format_posixct_cols(df)
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

# to_json()/to_yaml() build their output by pre-converting a data frame to
# a plain (row-major) list via df_to_rows(), then handing that plain list
# to jsonlite::toJSON()/yaml::as.yaml() -- neither of which has any idea a
# given scalar used to be a POSIXct, so a raw POSIXct value serializes as
# whatever its own internal representation happens to print as (confirmed
# directly: a naive local-clock-time string with no timezone marker at all
# for JSON, a raw epoch-seconds number for YAML) -- silently the *wrong
# instant* once re-parsed, unlike vg_widget()'s own separate path, which
# goes through jsonlite's data.frame-aware serialization directly and
# already formats POSIXct correctly. Formatting to an explicit ISO 8601 UTC
# string here, before either serializer ever sees it, matches what that
# already-correct path produces, so all three rendering surfaces agree.
format_posixct_cols <- function(df) {
  is_posixct <- vapply(df, inherits, logical(1), "POSIXct")
  if (any(is_posixct)) {
    df[is_posixct] <- lapply(df[is_posixct], format, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
  }
  df
}
