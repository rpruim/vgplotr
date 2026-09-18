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
#' @noRd
as_spec_payload <- function(spec) {
  if (is.character(spec)) {
    return(list(spec = parse_spec_string(spec), tables = list(), files = list()))
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
      tables[[nm]] <- src$data
    } else if (kind == "local_file") {
      file_info <- read_local_data_file(src$file)
      file_info$options <- src[setdiff(names(src), c("file", "type"))]
      files[[nm]] <- file_info
    } else {
      data_entries[[nm]] <- serialize_data_source(src)
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
    if (is_json) jsonlite::fromJSON(text, simplifyVector = FALSE) else yaml::yaml.load(quote_yaml_bool_keys(text)),
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

# YAML 1.1 (what libyaml/the yaml package implements) resolves a handful of
# short, unquoted words as booleans: y/n/on/off/yes/no (any case), on top of
# the unambiguous true/false. That's a real hazard here specifically because
# `y` -- as in the *x*/*y* encoding channel, one of the most common property
# names in a mosaic spec -- silently becomes the logical TRUE if left
# unquoted, which R then coerces to the list name "TRUE" (confirmed
# directly: `yaml::yaml.load("y: b")` returns a list named "TRUE", not
# "y"). The yaml package's own `handlers` argument can't fix this: it's
# never invoked for *implicit* type resolution at all (confirmed directly
# -- a custom "bool" handler simply never fires), only for explicit `!!tag`
# annotations. So this quotes those specific bare words when they appear as
# a mapping key, before parsing, leaving true/false (unambiguous, and what
# to_yaml() itself always emits for real booleans) alone.
quote_yaml_bool_keys <- function(text) {
  bool_words <- c("y", "Y", "yes", "Yes", "YES", "n", "N", "no", "No", "NO",
                   "on", "On", "ON", "off", "Off", "OFF")
  pattern <- paste0(
    "^(\\s*(?:-\\s+)?)(", paste(bool_words, collapse = "|"), ")(\\s*:)(\\s|$)"
  )
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- vapply(
    lines,
    function(line) sub(pattern, "\\1'\\2'\\3\\4", line, perl = TRUE),
    character(1),
    USE.NAMES = FALSE
  )
  paste(lines, collapse = "\n")
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
    fn_name <- as.character(expr[[1]])
    if (fn_name %in% c("sql", "agg", "param")) {
      return(serialize_value(eval(expr, envir = env)))
    }
    spec <- vg_transform_specs[[fn_name]]
    if (is.null(spec)) {
      stop(
        "Only simple column references (e.g., ~Date), sql()/agg(), or known ",
        "transform functions (", paste(names(vg_transform_specs), collapse = "(), "), "()) ",
        "can be used inside a mapping formula. Got a call to `", fn_name, "()`.",
        call. = FALSE
      )
    }
    fn <- get(fn_name, mode = "function")
    mc <- match.call(definition = fn, call = expr)
    serialize_transform(build_vg_transform(spec, mc, env))
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
    if (length(data_sources)) out$data <- lapply(data_sources, serialize_data_source)
    if (length(spec$params)) out$params <- serialize_params(spec$params)
    if (length(spec$plot_defaults)) out$plotDefaults <- lapply(spec$plot_defaults, serialize_value)
    out <- c(out, serialize_layout(spec$layout))
    if (length(spec$attrs)) out <- merge_attrs(out, lapply(spec$attrs, serialize_value), context = "vg_attributes()")
    out
  } else {
    serialize_layout(spec)
  }
}

serialize_data_source <- function(src) {
  if (!is.null(src$data) && is.data.frame(src$data)) {
    utils::modifyList(src, list(data = df_to_rows(src$data)))
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

df_to_rows <- function(df) {
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}
