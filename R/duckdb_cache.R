# Keep in sync with DUCKDB_WASM_VERSION in inst/htmlwidgets/vgplotr.js and
# data-raw/js/build.js.
.vg_duckdb_wasm_version <- "1.29.0"

vg_duckdb_cache_dir <- function() {
  file.path(tools::R_user_dir("vgplotr", "cache"), paste0("duckdb-wasm-", .vg_duckdb_wasm_version))
}

vg_duckdb_cache_pref_file <- function() {
  file.path(tools::R_user_dir("vgplotr", "config"), "duckdb_cache_declined")
}

vg_duckdb_cache_declined <- function() file.exists(vg_duckdb_cache_pref_file())

vg_duckdb_cache_decline <- function() {
  f <- vg_duckdb_cache_pref_file()
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  file.create(f)
  invisible(NULL)
}

#' Reset the "don't ask again" preference set by declining to cache the
#' DuckDB WASM engine
#' @export
vg_duckdb_cache_reset_preference <- function() {
  f <- vg_duckdb_cache_pref_file()
  if (file.exists(f)) unlink(f)
  invisible(NULL)
}

vg_duckdb_cache_files <- function() {
  dir <- vg_duckdb_cache_dir()
  c(wasm = file.path(dir, "duckdb-eh.wasm"), worker = file.path(dir, "duckdb-browser-eh.worker.js"))
}

vg_duckdb_cache_exists <- function() all(file.exists(vg_duckdb_cache_files()))

#' Check whether the DuckDB WASM engine is cached locally
#'
#' See [vg_cache_duckdb()].
#' @export
vg_duckdb_cache_status <- function() {
  files <- vg_duckdb_cache_files()
  cached <- all(file.exists(files))
  structure(
    list(
      cached = cached,
      dir = vg_duckdb_cache_dir(),
      version = .vg_duckdb_wasm_version,
      size = if (cached) sum(file.size(files)) else NA_real_
    ),
    class = "vg_duckdb_cache_status"
  )
}

#' @export
print.vg_duckdb_cache_status <- function(x, ...) {
  if (x$cached) {
    cat(
      "DuckDB WASM engine v", x$version, " is cached at ", x$dir,
      " (", round(x$size / 1e6, 1), " MB).\n",
      sep = ""
    )
  } else {
    cat(
      "DuckDB WASM engine v", x$version, " is not cached ",
      "(vg_render() fetches it from a CDN each time instead). ",
      "Call vg_cache_duckdb() to cache it locally (~35 MB, one-time download).\n",
      sep = ""
    )
  }
  invisible(x)
}

#' Cache the DuckDB WASM engine locally for offline/reproducible rendering
#'
#' [vg_render()] needs a client-side DuckDB to query data in the browser.
#' Most of the JS runtime it uses ships with vgplotr, but the database
#' engine itself is a compiled WebAssembly binary too large to include in
#' the package (about 35 MB) -- by default, `vg_render()` fetches it from a
#' CDN each time a plot is *viewed*. Calling `vg_cache_duckdb()` once
#' downloads it into a local, version-pinned cache
#' (`tools::R_user_dir("vgplotr", "cache")`) instead; every `vg_render()`
#' after that uses the cached copy, with no CDN involved at all -- including
#' when the rendered page is viewed completely offline, and without the
#' cached version ever silently changing later.
#'
#' This is entirely opt-in: without calling it, vgplotr behaves exactly as
#' before (fetching from a CDN at view time). [vg_render()] will offer to
#' run this for you interactively (at most once per session, and never
#' again if you decline permanently -- see [vg_duckdb_cache_reset_preference()])
#' when the cache doesn't already exist.
#'
#' @param force Re-download even if already cached.
#' @return Invisibly, the result of [vg_duckdb_cache_status()] after caching.
#' @export
vg_cache_duckdb <- function(force = FALSE) {
  status <- vg_duckdb_cache_status()
  if (status$cached && !force) {
    message("Already cached at ", status$dir, ". Use force = TRUE to re-download.")
    return(invisible(status))
  }

  dir <- vg_duckdb_cache_dir()
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  base_url <- sprintf(
    "https://cdn.jsdelivr.net/npm/@duckdb/duckdb-wasm@%s/dist",
    .vg_duckdb_wasm_version
  )
  files <- vg_duckdb_cache_files()
  message("Downloading DuckDB WASM engine v", .vg_duckdb_wasm_version, " (~35 MB)...")
  for (nm in names(files)) {
    src_name <- if (nm == "wasm") "duckdb-eh.wasm" else "duckdb-browser-eh.worker.js"
    utils::download.file(paste0(base_url, "/", src_name), files[[nm]], mode = "wb", quiet = FALSE)
  }
  message("Cached at ", dir, ".")

  invisible(vg_duckdb_cache_status())
}

#' Remove the locally cached DuckDB WASM engine
#'
#' See [vg_cache_duckdb()]. This does not affect the "don't ask again"
#' preference; see [vg_duckdb_cache_reset_preference()] for that.
#' @export
vg_uncache_duckdb <- function() {
  dir <- vg_duckdb_cache_dir()
  if (dir.exists(dir)) unlink(dir, recursive = TRUE)
  invisible(NULL)
}

# Tracks whether we've already offered to cache in *this* R session, so
# vg_render() asks at most once per session even across many plots -- reset
# naturally on every new session, independent of the persistent
# "don't ask again" preference file.
.vgplotr_session <- new.env(parent = emptyenv())
.vgplotr_session$asked_duckdb_cache <- FALSE

# Called from vg_render(). Only ever prompts in an interactive session
# (never during a knit/render, which runs non-interactively even when
# started by a human at a console -- so this can't block an automated
# build), and only when there's something to offer: no cache yet, not
# permanently declined, not already asked this session.
maybe_offer_duckdb_cache <- function() {
  if (vg_duckdb_cache_exists()) return(invisible(NULL))
  if (!interactive()) return(invisible(NULL))
  if (vg_duckdb_cache_declined()) return(invisible(NULL))
  if (isTRUE(.vgplotr_session$asked_duckdb_cache)) return(invisible(NULL))
  .vgplotr_session$asked_duckdb_cache <- TRUE

  choice <- utils::menu(
    choices = c(
      "Yes, cache it now (~35 MB one-time download)",
      "Not now (ask me again next session)",
      "No, don't ask again"
    ),
    title = paste(
      "vgplotr can cache the DuckDB WASM engine locally, so rendering works",
      "offline and doesn't depend on a CDN. Cache it now?"
    )
  )
  if (choice == 1) {
    vg_cache_duckdb()
  } else if (choice == 3) {
    vg_duckdb_cache_decline()
    message("OK, won't ask again. Run vg_cache_duckdb() any time to cache it later.")
  }
  invisible(NULL)
}

# The htmltools dependency vg_render() attaches when the cache exists, so
# the rendered page carries a URL to the local files (via `attachment`,
# which just links to the file -- see htmlDependency()'s docs -- rather
# than embedding the ~35MB engine's content in the document itself) instead
# of leaving the JS to fetch it from a CDN.
vg_duckdb_cache_dependency <- function() {
  if (!vg_duckdb_cache_exists()) return(NULL)
  htmltools::htmlDependency(
    name = "vgplotr-duckdb-wasm",
    version = .vg_duckdb_wasm_version,
    src = c(file = vg_duckdb_cache_dir()),
    attachment = list(wasm = "duckdb-eh.wasm", worker = "duckdb-browser-eh.worker.js")
  )
}
