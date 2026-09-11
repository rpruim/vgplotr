# Converts a vgspec into the shape needed to render it: a mosaic-spec JSON
# object (as a plain R list, ready for htmlwidgets/jsonlite), plus a separate
# set of tables to load into DuckDB *before* that spec is parsed.
#
# Data frames are deliberately kept out of the spec's own `data:` block and
# loaded explicitly beforehand (see vg_render()/inst/htmlwidgets/vgplotr.js)
# rather than relying on mosaic-spec's own declarative data loading -- in
# testing, astToDOM() would sometimes query a mark's table before an inline
# `data: [...]` array had finished loading, and this preload avoids that
# race. Data sources given as file/query specs (not a data frame) are passed
# through in the spec's `data:` block as before; that path hasn't been
# render-tested yet.
#' @noRd
as_spec_payload <- function(spec) {
  stopifnot(is_vgspec(spec))
  if (!is_vg_plot_fragment(spec$layout)) {
    stop(
      "Only specs with a single plot (no vconcat()/hconcat(), which aren't ",
      "implemented yet) can be rendered so far.",
      call. = FALSE
    )
  }

  tables <- list()
  data_entries <- list()
  for (nm in names(spec$data)) {
    src <- spec$data[[nm]]
    if (!is.null(src$data) && is.data.frame(src$data)) {
      tables[[nm]] <- src$data
    } else {
      data_entries[[nm]] <- src
    }
  }

  out <- list()
  if (length(spec$meta)) out$meta <- spec$meta
  if (length(data_entries)) out$data <- data_entries
  if (length(spec$params)) out$params <- spec$params
  out$plot <- lapply(spec$layout$items, serialize_item)

  # With only a single plot (no vconcat()/hconcat() yet), plotDefaults and
  # this plot's own attributes both just apply to that one plot; merge them
  # (the plot's own attrs win) rather than emitting a separate, currently
  # meaningless `plotDefaults` key. This will need revisiting once multiple
  # plots (and therefore a real plotDefaults use case) exist.
  out <- utils::modifyList(out, utils::modifyList(spec$plot_defaults, spec$layout$attrs))

  list(spec = out, tables = tables)
}

serialize_item <- function(item) {
  if (inherits(item, "vg_mark")) {
    c(list(mark = item$mark), serialize_encodings(item$encodings))
  } else if (inherits(item, "vg_interactor")) {
    c(list(select = item$type), serialize_encodings(item$options))
  } else {
    stop(
      "Don't know how to serialize an object of class ",
      paste(class(item), collapse = "/"), ".",
      call. = FALSE
    )
  }
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
  } else {
    x
  }
}

serialize_formula <- function(f) {
  rhs <- f[[2]]
  if (is.symbol(rhs)) {
    as.character(rhs)
  } else {
    stop(
      "Only simple column-reference formulas (e.g. ~Date) can be serialized ",
      "so far -- transform functions like bin()/count() aren't implemented ",
      "yet. Got: ", deparse(f),
      call. = FALSE
    )
  }
}
