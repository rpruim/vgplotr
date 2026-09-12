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
# a plain file (`file://`, e.g. a Positron/RStudio viewer or just opening
# the .html directly) rather than served over HTTP. Reading the file here
# and embedding its bytes sidesteps that entirely -- it's loaded into
# DuckDB from memory, with no fetch involved, so it works the same way
# regardless of how the page itself was opened. A genuine http(s) URL is
# left in the spec's `data:` block, since mosaic's own loading already
# handles that reliably (tested directly) and fetching it eagerly here
# would be wrong for a large or possibly-updated-later remote file.
#' @noRd
as_spec_payload <- function(spec) {
  stopifnot(is_vgspec(spec))
  if (is.null(spec$layout)) {
    stop("This vgspec doesn't have any plots yet.", call. = FALSE)
  }

  tables <- list()
  files <- list()
  data_entries <- list()
  for (nm in names(spec$data)) {
    src <- spec$data[[nm]]
    if (!is.null(src$data) && is.data.frame(src$data)) {
      tables[[nm]] <- src$data
    } else if (!is.null(src$file) && !grepl("^https?://", src$file, ignore.case = TRUE)) {
      file_info <- read_local_data_file(src$file)
      file_info$options <- src[setdiff(names(src), c("file", "type"))]
      files[[nm]] <- file_info
    } else {
      data_entries[[nm]] <- src
    }
  }

  out <- list()
  if (length(spec$meta)) out$meta <- spec$meta
  if (length(data_entries)) out$data <- data_entries
  if (length(spec$params)) out$params <- spec$params
  out <- c(out, serialize_layout(spec$layout, spec$plot_defaults))

  list(spec = out, tables = tables, files = files)
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
# of further layout nodes, or a spacer. `plot_defaults` (mosaic-spec's
# `plotDefaults`) is threaded down and merged into *every* plot found in the
# tree (the plot's own attributes win on conflict) rather than emitted as a
# separate top-level `plotDefaults` key -- simpler, and equivalent as long as
# plotDefaults is only ever used for static attributes like width/height.
serialize_layout <- function(layout, plot_defaults = list()) {
  if (is_vg_plot_fragment(layout)) {
    attrs <- utils::modifyList(plot_defaults, layout$attrs)
    c(list(plot = lapply(layout$items, serialize_item)), attrs)
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

# Pulls data_from/filter_by out of a mark's encodings into the nested
# `data: {from:, filterBy:}` object mosaic-spec expects, and translates each
# remaining value (formulas, param() references) to plain JSON-able values.
serialize_encodings <- function(enc) {
  data_from <- enc$data_from
  filter_by <- enc$filter_by
  enc$data_from <- NULL
  enc$filter_by <- NULL

  out <- lapply(enc, serialize_value)

  if (!is.null(data_from) || !is.null(filter_by)) {
    data_obj <- list()
    if (!is.null(data_from)) data_obj$from <- data_from
    if (!is.null(filter_by)) data_obj$filterBy <- serialize_value(filter_by)
    out <- c(list(data = data_obj), out)
  }
  out
}

serialize_value <- function(x) {
  if (inherits(x, "formula")) {
    serialize_formula(x)
  } else if (is_vg_param(x)) {
    format(x)
  } else if (is_vg_transform(x)) {
    serialize_transform(x)
  } else {
    x
  }
}

serialize_formula <- function(f) {
  serialize_expr(f[[2]], environment(f))
}

# Recognizes a mapping formula's RHS: a bare column-name symbol, a literal,
# or a call to one of the vg_transform_specs functions (vg_bin(), vg_count(),
# ...) -- recognized here purely syntactically, via match.call() against the
# real function's formals, without ever evaluating the call itself (which
# would fail, since e.g. `delay` in `~vg_bin(delay)` isn't a bound variable).
serialize_expr <- function(expr, env) {
  if (is.symbol(expr)) {
    as.character(expr)
  } else if (is.call(expr)) {
    fn_name <- as.character(expr[[1]])
    spec <- vg_transform_specs[[fn_name]]
    if (is.null(spec)) {
      stop(
        "Only simple column references (e.g. ~Date) or known transform ",
        "functions (", paste(names(vg_transform_specs), collapse = "(), "), "()) ",
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
