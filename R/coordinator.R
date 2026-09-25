#' Options for mosaic's client-side coordinator
#'
#' Every plot on a page talks to the database through one mosaic
#' *coordinator*, which caches query results, merges queries that can share a
#' result, and speeds up interactive filtering by pre-aggregating. The defaults
#' suit most graphics; `vg_coordinator()` collects the options for the times
#' they don't: to turn a feature off while tracking down a problem, to keep
#' mosaic's console output quiet, or to keep pre-aggregation out of a database
#' you care about.
#'
#' Pass the result to [vg_widget()]/[vg_render()] as `coordinator =`, or set
#' `options(vgplotr.coordinator = vg_coordinator(...))` once for a whole
#' document (like `vgplotr.use_cache`; see [vg_widget()]).
#'
#' The coordinator is shared by every widget on a page, and the first widget
#' to render creates it. Its options are the ones that count: a later widget
#' that asks for different ones is told so in the browser console and
#' otherwise ignored. To use a setting, give it to every widget (the
#' `vgplotr.coordinator` option does that).
#'
#' @param cache Whether to keep query results in the browser and reuse them
#'   when the same query is asked again (default `TRUE`). Turn off to see every
#'   query run against the database.
#' @param consolidate Whether to combine queries that differ only in what
#'   they select into one query (default `TRUE`).
#' @param preaggregate Whether to speed up interactive filtering of
#'   aggregated marks (histograms, heatmaps, ...) by materializing data cubes
#'   in the database (default `TRUE`). Those tables are written to the
#'   database's `preaggregate_schema`, so set this to `FALSE` when the database
#'   is a real one, e.g., from [vg_duckdb_connector()], that shouldn't get a
#'   schema of mosaic's making.
#' @param preaggregate_schema The database schema, a simple name (letters,
#'   digits and underscores), that holds the pre-aggregated tables (default
#'   `"mosaic"`).
#' @param logging How much mosaic writes to the browser console: `"all"` (the
#'   default; per-query timing among other messages), `"errors"` (warnings and
#'   errors only), or `"none"`.
#' @return A `vg_coordinator` object.
#' @family rendering functions
#' @export
#' @examples
#' vg_coordinator(logging = "errors")
#' vg_coordinator(cache = FALSE, preaggregate = FALSE)
vg_coordinator <- function(cache = TRUE,
                           consolidate = TRUE,
                           preaggregate = TRUE,
                           preaggregate_schema = "mosaic",
                           logging = c("all", "errors", "none")) {
  check_coordinator_flag(cache, "cache")
  check_coordinator_flag(consolidate, "consolidate")
  check_coordinator_flag(preaggregate, "preaggregate")
  if (!(is.character(preaggregate_schema) && length(preaggregate_schema) == 1 &&
        !is.na(preaggregate_schema) && grepl("^[A-Za-z_][A-Za-z0-9_]*$", preaggregate_schema))) {
    stop(
      "`preaggregate_schema` must be a simple name: letters, digits and ",
      "underscores, not starting with a digit.",
      call. = FALSE
    )
  }
  logging <- rlang::arg_match(logging)

  structure(
    list(
      cache = cache,
      consolidate = consolidate,
      preaggregate = preaggregate,
      preaggregate_schema = preaggregate_schema,
      logging = logging
    ),
    class = "vg_coordinator"
  )
}

check_coordinator_flag <- function(x, name) {
  if (!(is.logical(x) && length(x) == 1 && !is.na(x))) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

is_vg_coordinator <- function(x) inherits(x, "vg_coordinator")

# What vgplotr.js reads (`x.coordinator`): the names mosaic's own Coordinator
# takes, with the pre-aggregation options grouped under `preagg`.
coordinator_payload <- function(x) {
  list(
    cache = x$cache,
    consolidate = x$consolidate,
    preagg = list(enabled = x$preaggregate, schema = x$preaggregate_schema),
    logging = x$logging
  )
}

#' @export
print.vg_coordinator <- function(x, ...) {
  cat("<vg_coordinator>\n")
  print_fields(unclass(x))
  invisible(x)
}
