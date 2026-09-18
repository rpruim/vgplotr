# Keep in sync with MOSAIC_VERSION in data-raw/update-schema.R and
# data-raw/js/build.js -- see tests/testthat/test-info.R for an automated
# check that this (and .vg_duckdb_wasm_version, R/duckdb_cache.R) actually
# matches what got bundled into inst/htmlwidgets/lib/mosaic-bundle.min.js,
# rather than relying on this comment alone.
.vg_mosaic_version <- "0.31.0"

#' Report the versions of Mosaic and DuckDB-Wasm this vgplotr bundles
#'
#' The mosaic JS runtime and DuckDB-Wasm's JS API are vendored into the
#' package at a fixed version, chosen when this vgplotr version was built
#' (not resolved at install time or view time) -- see `vignette("using-databases")`
#' and `data-raw/js/build.js`. `vg_info()` reports exactly which versions
#' that is, along with whether the (separately-cached, since it's too large
#' to vendor) DuckDB-Wasm engine binary itself is currently cached locally,
#' and -- since these aren't vendored at all, but come from whatever the
#' user has installed -- the `duckdb`/`nanoarrow` package versions used by
#' [vg_duckdb_connector()]'s native rendering path, if installed.
#'
#' @return An object of class `vg_info`, with a `print()` method, containing:
#'   \describe{
#'     \item{`vgplotr`}{This vgplotr version.}
#'     \item{`mosaic`}{The bundled mosaic version.}
#'     \item{`duckdb_wasm`}{The pinned DuckDB-Wasm version (see
#'       [vg_duckdb_cache_status()] for whether it's currently cached
#'       locally or fetched from a CDN at view time).}
#'     \item{`duckdb_cache`}{The result of [vg_duckdb_cache_status()].}
#'     \item{`native_packages`}{A named character vector, the installed
#'       version of each of `duckdb`/`nanoarrow`/`DBI` (used by
#'       [vg_duckdb_connector()]), or `NA` for any not installed.}
#'   }
#' @family info functions
#' @export
vg_info <- function() {
  native_packages <- vapply(
    c("duckdb", "nanoarrow", "DBI"),
    function(pkg) {
      if (requireNamespace(pkg, quietly = TRUE)) {
        as.character(utils::packageVersion(pkg))
      } else {
        NA_character_
      }
    },
    character(1)
  )

  structure(
    list(
      vgplotr = as.character(utils::packageVersion("vgplotr")),
      mosaic = .vg_mosaic_version,
      duckdb_wasm = .vg_duckdb_wasm_version,
      duckdb_cache = vg_duckdb_cache_status(),
      native_packages = native_packages
    ),
    class = "vg_info"
  )
}

#' @export
print.vg_info <- function(x, ...) {
  cat("vgplotr ", x$vgplotr, "\n", sep = "")
  cat("  Mosaic:      ", x$mosaic, " (bundled)\n", sep = "")
  cat(
    "  DuckDB-Wasm: ", x$duckdb_wasm,
    if (x$duckdb_cache$cached) " (bundled; cached locally)" else " (bundled; fetched from CDN when not cached)",
    "\n",
    sep = ""
  )
  cat("  Native DuckDB (optional, for vg_duckdb_connector()):\n")
  labels <- formatC(paste0(names(x$native_packages), ":"), width = -max(nchar(names(x$native_packages)) + 1))
  for (i in seq_along(x$native_packages)) {
    version <- x$native_packages[[i]]
    cat("    ", labels[i], " ", if (is.na(version)) "not installed" else version, "\n", sep = "")
  }
  invisible(x)
}
