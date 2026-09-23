#' Create a new mosaic vgplot specification
#'
#' `vg_create()` starts a new `vgspec`, the top-level object that other
#' `vg_*()` functions are piped through to add data sources, params, marks,
#' interactors, and layout. Marks and interactors can be piped directly onto
#' a freshly created spec (e.g., `vg_create() |> vg_mark_dot(x = ~a, y = ~b)`) as
#' long as the spec only needs a single plot; a spec with multiple plots, or
#' with layout-level inputs (e.g., [vg_menu()], [vg_table()]), needs an
#' explicit layout (see [vg_vconcat()]/[vg_hconcat()]).
#'
#' @param data An optional data frame to register as this spec's first data
#'   source (equivalent to following up with `vg_data(data = data)` -- see
#'   [vg_data()] for the name this gets, and [vg_mark()] for the analogous
#'   `some_data |> vg_mark_dot(...)` shorthand).
#' @param ... Named values routed automatically to wherever mosaic-spec
#'   allows them: a known plot-level attribute (e.g., `width =`, `height =`)
#'   goes to the spec's own top level (as if passed to [vg_attributes()]);
#'   anything else goes to `meta` (as if passed to [vg_meta()]), since
#'   mosaic-spec's metadata accepts arbitrary keys (e.g., `title =`). To set
#'   mosaic-spec's `plotDefaults` (applied to *every* plot, as opposed to
#'   just the spec's own top level) instead, call [vg_plot_defaults()]
#'   explicitly -- it's never chosen automatically, since every name valid
#'   there is also valid at the top level, which is preferred.
#' @family spec functions
#' @export
vg_create <- function(data = NULL, ...) {
  routed <- route_spec_args(list(...))
  spec <- structure(
    list(
      meta = routed$meta,
      data = list(),
      params = list(),
      config = list(),
      layout = NULL,
      attrs = routed$attrs,
      plot_defaults = list()
    ),
    class = "vgspec"
  )
  if (!is.null(data)) spec <- vg_data(spec, data = data)
  spec
}

is_vgspec <- function(x) inherits(x, "vgspec")

# Routes named arguments (from vg_create()'s `...`) to the top-level `attrs`
# bucket when the name is a known plot-level attribute (vg_plot_level_args()
# -- mosaic-spec's PlotAttributes, the same type Plot's own fields and
# plotDefaults both use; snake_case is accepted here too, translated via
# canonicalize_plot_attr_names()), or to `meta` otherwise (mosaic-spec's
# Meta type accepts arbitrary keys, so it's a safe catch-all for anything
# else, e.g., `title`). plotDefaults is deliberately not a third destination
# here: since it accepts exactly the same names as the top level, top level
# always wins under a "first match" priority -- plotDefaults is only
# reachable via an explicit call to vg_plot_defaults().
route_spec_args <- function(args) {
  args <- canonicalize_plot_attr_names(args)
  is_attr <- names(args) %in% vg_plot_level_args()
  list(attrs = args[is_attr], meta = args[!is_attr])
}

#' Set metadata (title, description) on a vgspec
#'
#' Mosaic-spec's own `meta` is inert as far as mosaic's JS runtime is
#' concerned -- it never reads `meta` when building the DOM, so this is
#' pure metadata (e.g., for a spec browser/gallery tool, or round-tripping
#' through [to_json()]/[to_yaml()]), not something that appears on the
#' rendered graphic by itself. As a vgplotr-level convenience, [vg_render()]
#' *does* render `meta$title` (as a caption above the widget); `description`/
#' `credit` remain inert for now.
#'
#' @param spec A `vgspec`.
#' @param ... Named metadata fields, e.g., `title =`, `description =`.
#' @family spec functions
#' @export
vg_meta <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  spec$meta <- override_attrs(spec$meta, list(...))
  spec
}

#' Set mosaic-spec `config` options on a vgspec
#'
#' Sets mosaic-spec's top-level `config` object: runtime configuration for
#' the database connection itself (currently just `extensions`, DuckDB
#' extensions to load before the spec runs, e.g., `"spatial"` for
#' geospatial data/marks) -- as opposed to `meta` (inert descriptive
#' metadata) or any of the spec's actual data/params/graphic content.
#'
#' @param spec A `vgspec`.
#' @param ... Named config options, e.g., `extensions = "spatial"` (or a
#'   character vector for more than one extension).
#' @family spec functions
#' @export
vg_config <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  spec$config <- override_attrs(spec$config, list(...))
  spec
}

#' Add a data source to a vgspec
#'
#' A data frame passed as `data` is loaded into DuckDB directly (see
#' [vg_render()]); other arguments (`file =`, `query =`, `where =`, ...)
#' describe a source for mosaic's own data loading instead.
#' @param spec A `vgspec`.
#' @param name The name other parts of the spec use to refer to this data
#'   (via `data_from =`/`filter_by =` on marks and interactors). Optional --
#'   if omitted, an unused name is generated (`"data1"`, `"data2"`, ...),
#'   e.g., for the shorthand described in [vg_mark()]. This is unique
#'   across the whole R session, not just within this spec: every
#'   `vg_render()` call on the same page shares one DuckDB instance (see
#'   `vignette("getting-started")`), so two *separate* specs that both
#'   omit `name` -- e.g., two `some_df |> vg_mark_dot(...)` shortcuts in one
#'   document -- still get distinct names instead of silently clobbering
#'   each other's table once rendered together.
#' @param data An optional data frame to use as this data source.
#' @param ... Data source options, e.g., `file =`, `query =`, `where =`.
#' @family spec functions
#' @export
vg_data <- function(spec, name = NULL, data = NULL, ...) {
  stopifnot(is_vgspec(spec))
  if (is.null(name)) name <- auto_data_name(spec)
  src <- if (!is.null(data)) list(data = data) else list(...)
  spec$data[[name]] <- src
  record_data_registry_entry(name, src)
  spec
}

# Session-wide log of every data-source name vg_data() has registered so
# far this R session -- auto-generated or explicit, to ANY spec, not just
# the current one -- keyed by name, each entry holding what that name most
# recently referred to (see data_registry_kind()/data_registry_source()).
# Backs both auto_data_name()'s collision avoidance (see its own comment
# for why that needs to span separate vg_create() chains) and the public
# vg_data_registry(). A single shared structure for both, rather than two
# parallel ones, since "every name handed out so far" and "what every name
# currently refers to" are the same underlying fact.
.vgplotr_data_registry <- new.env(parent = emptyenv())
.vgplotr_data_registry$entries <- list()

# Test-only: clears the registry so a test can assert the naming sequence
# restarts at "data1" without leaking state from a test that ran earlier in
# the same session. @noRd
reset_auto_data_names <- function() {
  assign("entries", list(), envir = .vgplotr_data_registry)
}

# The first name, from "data1", "data2", "data3", ..., that is unused both
# in `spec$data` (this spec's own already-registered sources -- what lets
# vg_data() called several times on one evolving spec still get "data1",
# "data2", "data3", ...) and in the session-wide registry (every name
# handed out so far this session, regardless of spec). Deliberately starts
# at "data1", not "data" -- every auto-generated name then has the exact
# same "data<N>" shape, with no unsuffixed "data" as a special case a
# caller could off-by-one on (e.g., assuming the *second* source is always
# "data1", which was only true when a spec's first two sources were both
# auto-named).
#
# The session-wide check is what actually matters for build_mark()'s
# `some_df |> vg_mark_dot(...)` shortcut (see vg_mark()): that path always
# starts from a brand-new, empty vg_create(), so spec$data alone is always
# empty there -- without a session-wide registry too, every such shortcut
# would always get named "data1", no matter how many others already exist.
# That's harmless for one plot on a page, but once a *different* data
# frame's shortcut plot ends up on the same page (mosaic-spec's `data:`
# names are shared across every `vg_render()` call on a page -- see
# vg_data()'s own `name` docs), the two tables collide: whichever finishes
# loading last silently overwrites the other's, and any plot that queries
# afterward gets a confusing DuckDB error about a missing column instead
# of anything pointing at the real cause.
auto_data_name <- function(spec) {
  existing <- names(spec$data)
  used <- names(.vgplotr_data_registry$entries)
  i <- 1
  repeat {
    candidate <- paste0("data", i)
    if (!(candidate %in% existing) && !(candidate %in% used)) break
    i <- i + 1
  }
  candidate
}

# Human-facing classification of a data source for vg_data_registry() --
# distinct from vg_data_source_kind() (R/serialize.R), which classifies
# for a different purpose (what rendering needs to preload) and doesn't
# tell a URL apart from a query the way this needs to.
data_registry_kind <- function(src) {
  if (!is.null(src$data) && is.data.frame(src$data)) {
    "local data"
  } else if (!is.null(src$file)) {
    if (grepl("^https?://", src$file, ignore.case = TRUE)) "url" else "file"
  } else if (!is.null(src$query)) {
    "query"
  } else {
    "other"
  }
}

# A short, human-readable description of where a data source's rows
# actually come from -- the data frame's dimensions, the file path/URL, or
# the query text -- for vg_data_registry().
data_registry_source <- function(src) {
  if (!is.null(src$data) && is.data.frame(src$data)) {
    sprintf("data.frame [%d x %d]", nrow(src$data), ncol(src$data))
  } else if (!is.null(src$file)) {
    src$file
  } else if (!is.null(src$query)) {
    src$query
  } else {
    NA_character_
  }
}

# Records (or overwrites, if `name` was already registered -- e.g., the
# same source re-registered under the same name across several chunks)
# what `name` currently refers to, for vg_data_registry().
record_data_registry_entry <- function(name, src) {
  .vgplotr_data_registry$entries[[name]] <- list(
    kind = data_registry_kind(src),
    source = data_registry_source(src)
  )
}

#' List every data source name used this session, and what it currently refers to
#'
#' A session-wide log of every name [vg_data()] has registered so far --
#' whether given explicitly or auto-generated (see its `name` argument) --
#' meant as a diagnostic aid. Since every `vg_render()` call on the same
#' page shares one DuckDB instance, two different specs that end up using
#' the same name for different data silently clobber each other once
#' rendered together, with nothing in the R console to say so (see
#' `vignette("getting-started")`'s "One DuckDB instance for an entire
#' page" callout). Checking `vg_data_registry()` before rendering several
#' specs onto the same page -- to confirm each name's `source` is really
#' the data meant for it -- is a quick way to catch that kind of mistake
#' before it happens instead of after.
#'
#' Reflects only the *most recent* registration for each name (matching
#' what would actually be live in the shared DuckDB instance if every spec
#' built so far were rendered onto one page right now), not a full history
#' of every past registration. Only covers sources registered through
#' [vg_data()]/[vg_create(data = )][vg_create()]/the `some_df |>
#' vg_mark_dot(...)` shorthand -- a name that only appears inside a raw
#' JSON/YAML string passed straight to [vg_render()]/[vg_widget()] isn't
#' tracked here, since that path never goes through [vg_data()].
#'
#' @return A data frame with one row per distinct name: `name`; `kind`
#'   (`"local data"`, `"file"`, `"url"`, `"query"`, or `"other"`); and
#'   `source`, a short description (the data frame's dimensions, the file
#'   path/URL, or the query text).
#' @family spec functions
#' @export
vg_data_registry <- function() {
  entries <- .vgplotr_data_registry$entries
  if (!length(entries)) {
    return(data.frame(
      name = character(0), kind = character(0), source = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    name = names(entries),
    kind = vapply(entries, `[[`, character(1), "kind"),
    source = vapply(entries, `[[`, character(1), "source"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Resolves an integer data_from (a 1-based index into `names_vec`, the
# spec's registered data source names in registration order; negative
# counts from the end, e.g., -1 is the most recently registered one) to
# the data-source name it refers to. Used by vg_mark() so `data_from = 1L`
# works the same as naming that source explicitly -- the *integer* type
# (1L, not 1) is what distinguishes this from mosaic's own literal
# inline-data convention (data_from = c(0), always a double vector).
resolve_data_from_index <- function(i, names_vec) {
  n <- length(names_vec)
  if (length(i) != 1 || i == 0 || abs(i) > n) {
    stop(
      "`data_from = ", i, "L` is not a valid data source index -- this spec has ",
      n, " registered data source", if (n != 1) "s", " (", paste(names_vec, collapse = ", "),
      "). Use a whole number from 1 to ", n,
      if (n > 0) paste0(" (or -1 to -", n, ", counting from the end)"),
      ".",
      call. = FALSE
    )
  }
  if (i > 0) names_vec[[i]] else names_vec[[n + i + 1]]
}

#' Declare Params/Selections on a vgspec
#' @param spec A `vgspec`.
#' @param ... Named params, e.g., `point = 0`, or selections, e.g., `query =
#'   list(select = "intersect")` (mosaic-spec's `Selection` shape --
#'   `select` is one of `"crossfilter"`/`"intersect"`/`"single"`/`"union"`,
#'   with optional `cross`/`empty`/`include` fields).
#' @family spec functions
#' @export
vg_params <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  spec$params <- override_attrs(spec$params, list(...))
  spec
}

#' Set top-level attributes on a vgspec
#'
#' Sets attributes directly on the spec's own top level -- e.g., for a
#' single-plot spec, sibling keys to `plot:` such as `width`/`height`,
#' exactly like mosaic's own example specs write them. This is distinct
#' from [vg_plot_defaults()] (mosaic-spec's `plotDefaults`, applied to
#' *every* plot in the spec, including ones nested in a
#' [vg_vconcat()]/[vg_hconcat()]), from a single plot's own attributes (set
#' via arguments to [vg_plot()], possibly accumulated from several
#' marks/interactors -- see [vg_plot()] for that merge policy), and from a
#' single mark's encodings.
#'
#' A name that isn't one of mosaic's own plot attributes triggers a warning,
#' since it won't do anything to the rendered graphic (there's nowhere else
#' it could still take effect) -- e.g., `title`, which belongs in [vg_meta()]
#' instead.
#'
#' @param spec A `vgspec`.
#' @param ... Named top-level attributes, e.g., `width =`, `height =`,
#'   `x_domain =` (snake_case -- translated to mosaic's own camelCase key,
#'   e.g., `xDomain`).
#' @family spec functions
#' @export
vg_attributes <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  new <- canonicalize_plot_attr_names(list(...))
  warn_unknown_attrs(names(new), "vg_attributes()")
  spec$attrs <- merge_attrs(spec$attrs, new, context = "vg_attributes()")
  spec
}

#' Set mosaic-spec `plotDefaults` on a vgspec
#'
#' Sets mosaic-spec's top-level `plotDefaults`: attributes applied to *every*
#' plot in the spec, including ones nested inside a
#' [vg_vconcat()]/[vg_hconcat()] layout -- not just the spec's own top level
#' (see [vg_attributes()] for that). This is distinct from a single plot's
#' own attributes (set via arguments to [vg_plot()], possibly accumulated
#' from several marks/interactors -- see [vg_plot()] for that merge policy)
#' and from a single mark's encodings. If per-plot or per-mark
#' attribute-setting is needed later, that should get its own function
#' rather than overloading this one.
#'
#' A name that isn't one of mosaic's own plot attributes triggers a warning,
#' since it won't do anything to the rendered graphic -- see [vg_attributes()].
#'
#' @param spec A `vgspec`.
#' @param ... Named plot-default attributes, e.g., `width =`, `height =`,
#'   `x_domain =` (snake_case -- translated to mosaic's own camelCase key,
#'   e.g., `xDomain`).
#' @family spec functions
#' @export
vg_plot_defaults <- function(spec, ...) {
  stopifnot(is_vgspec(spec))
  new <- canonicalize_plot_attr_names(list(...))
  warn_unknown_attrs(names(new), "vg_plot_defaults()")
  spec$plot_defaults <- merge_attrs(spec$plot_defaults, new, context = "vg_plot_defaults()")
  spec
}

#' @export
print.vgspec <- function(x, ...) {
  cat("<vgspec>\n")
  if (length(x$meta)) cat("  meta:", paste(names(x$meta), collapse = ", "), "\n")
  if (length(x$data)) cat("  data:", paste(names(x$data), collapse = ", "), "\n")
  if (length(x$params)) cat("  params:", paste(names(x$params), collapse = ", "), "\n")
  if (length(x$attrs)) {
    str_at <- vapply(x$attrs, deparse_short, character(1))
    cat("  attrs:", paste0(names(x$attrs), " = ", str_at, collapse = ", "), "\n")
  }
  if (length(x$plot_defaults)) {
    str_pd <- vapply(x$plot_defaults, deparse_short, character(1))
    cat("  plot_defaults:", paste0(names(x$plot_defaults), " = ", str_pd, collapse = ", "), "\n")
  }
  if (is.null(x$layout)) {
    cat("  layout: (empty)\n")
  } else {
    cat("  layout:\n")
    print(x$layout)
  }
  invisible(x)
}
