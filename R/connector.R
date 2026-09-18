#' Render via DuckDB-Wasm in the browser (the default)
#'
#' The default rendering backend for [vg_widget()]/[vg_render()]: mosaic
#' runs entirely client-side against a DuckDB compiled to WebAssembly, so
#' the result is a fully self-contained page needing no server -- see
#' [vg_widget()] for details. You won't normally construct this directly;
#' it's the implicit default. Use [vg_duckdb_connector()] instead when you
#' need a real (native) DuckDB, e.g. for data too large for the browser, or
#' to reach an external database via DuckDB's own extensions
#' (`postgres_scanner`, `mysql_scanner`, `sqlite_scanner`, ...).
#'
#' @return A `vg_connector` object.
#' @family connector functions
#' @export
vg_wasm_connector <- function() {
  structure(list(), class = c("vg_wasm_connector", "vg_connector"))
}

#' Render via a native DuckDB connection instead of DuckDB-Wasm
#'
#' An opt-in alternative to the default [vg_wasm_connector()]: instead of
#' running DuckDB-Wasm inside the browser, the browser talks over a local
#' HTTP server (started automatically, see [vg_duckdb_server_stop()]) to a
#' *real* DuckDB process running in this R session. This removes
#' DuckDB-Wasm's size/extension limits -- in particular, it's the only way
#' to visualize data that actually lives in another database, via DuckDB's
#' own federation extensions (e.g. `ATTACH '...' AS pg (TYPE postgres)`,
#' `mysql_scanner`, `sqlite_scanner`): set those up yourself on `con` however
#' you like, vgplotr just proxies queries to it.
#'
#' Only one native connection is served per R session (starting a second,
#' different `con` while one is already active is an error -- call
#' [vg_duckdb_server_stop()] first if you need to switch).
#'
#' **Security note:** the local server executes whatever SQL the page sends
#' it, with no authentication -- mitigated by binding to `127.0.0.1` only,
#' never a public interface, but this means (1) don't point `con` at a
#' connection with write access to anything you wouldn't want arbitrary SQL
#' run against from that browser tab (prefer a read-only role for real
#' external databases), and (2) unlike DuckDB-Wasm output, a page rendered
#' this way is only live on the machine/session that rendered it -- sharing
#' the HTML file (email, hosting it, opening it later) gives a graphic that
#' can't connect to anything, since there's no server running for it to
#' reach on the recipient's machine.
#'
#' @param con An existing DBI connection to a DuckDB database (e.g. from
#'   `DBI::dbConnect(duckdb::duckdb())`), already set up with any extensions
#'   or `ATTACH`ed external databases you want reachable. Left `NULL` (the
#'   default), vgplotr creates and manages a private in-process connection
#'   for you, closed when you call [vg_duckdb_server_stop()]. A `con` you
#'   supply yourself is never closed by vgplotr -- you own its lifecycle.
#' @return A `vg_connector` object, for use as [vg_widget()]/[vg_render()]'s
#'   `connector =` argument.
#' @family connector functions
#' @export
vg_duckdb_connector <- function(con = NULL) {
  if (!is.null(con) && !inherits(con, "DBIConnection")) {
    stop(
      "`con` must be a DBI connection, e.g. from DBI::dbConnect(duckdb::duckdb()).",
      call. = FALSE
    )
  }
  structure(
    list(con = con, owns_con = is.null(con)),
    class = c("vg_duckdb_connector", "vg_connector")
  )
}

is_vg_connector <- function(x) inherits(x, "vg_connector")
