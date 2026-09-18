# Backs vg_duckdb_connector() (R/connector.R): a persistent, session-scoped
# local HTTP server speaking mosaic's own REST connector wire protocol
# (@uwdata/mosaic-core/src/connectors/rest.ts) -- one POST endpoint, a JSON
# body `{type: "exec"|"json"|"arrow", sql}` in, and either an empty 200
# ("exec"), a JSON array of row objects ("json"), or raw Arrow IPC bytes
# ("arrow") out. Deliberately not modeled on vg_snapshot()'s httpuv server
# (R/snapshot.R) -- that one is throwaway/static-file-only, torn down via
# on.exit() before the function that started it even returns; this one has
# to survive across many separate vg_widget() calls in the same session and
# handle queries dynamically, so it lives in package-level state instead.
.vgplotr_native <- new.env(parent = emptyenv())

# Only one native connection is served per session (see vg_duckdb_connector()
# docs) -- reuses the running server if `connector` refers to the same
# connection (implicit-and-already-owns-one, or the identical explicit
# `con`), errors if asked to switch to a different one without stopping
# the current server first.
ensure_vg_duckdb_server <- function(connector) {
  needed <- c("duckdb", "nanoarrow", "DBI", "httpuv")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "vg_duckdb_connector() needs the ", paste(missing, collapse = ", "),
      " package(s) (install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))).",
      call. = FALSE
    )
  }

  if (!is.null(.vgplotr_native$server)) {
    same_con <- if (is.null(connector$con)) {
      .vgplotr_native$owns_con
    } else {
      identical(connector$con, .vgplotr_native$con)
    }
    if (!same_con) {
      stop(
        "A native DuckDB server is already running for a different ",
        "connection in this session. Call vg_duckdb_server_stop() first ",
        "if you want to switch connections.",
        call. = FALSE
      )
    }
    return(list(con = .vgplotr_native$con, uri = .vgplotr_native$uri))
  }

  con <- connector$con
  owns_con <- is.null(con)
  if (owns_con) con <- DBI::dbConnect(duckdb::duckdb())

  port <- httpuv::randomPort()
  server <- httpuv::startServer("127.0.0.1", port, vg_duckdb_app(con))

  .vgplotr_native$server <- server
  .vgplotr_native$con <- con
  .vgplotr_native$owns_con <- owns_con
  .vgplotr_native$uri <- sprintf("http://127.0.0.1:%d/", port)

  list(con = con, uri = .vgplotr_native$uri)
}

#' Stop the local native-DuckDB rendering server
#'
#' Stops the persistent local HTTP server started automatically by
#' [vg_duckdb_connector()] and, if vgplotr created its own private DuckDB
#' connection for it (i.e., `con` was left `NULL`), disconnects it too. A
#' `con` you supplied yourself is left connected -- you own its lifecycle.
#' Any already-rendered widgets using the server stop working once it's
#' stopped. A no-op if no native server is currently running.
#'
#' @return Invisibly, `TRUE` if a server was actually stopped, `FALSE` if
#'   none was running.
#' @family connector functions
#' @export
vg_duckdb_server_stop <- function() {
  if (is.null(.vgplotr_native$server)) {
    return(invisible(FALSE))
  }
  httpuv::stopServer(.vgplotr_native$server)
  if (isTRUE(.vgplotr_native$owns_con)) {
    DBI::dbDisconnect(.vgplotr_native$con, shutdown = TRUE)
  }
  rm(list = ls(.vgplotr_native), envir = .vgplotr_native)
  invisible(TRUE)
}

# The httpuv app for ensure_vg_duckdb_server(): a single call() handler
# implementing mosaic's REST connector protocol against `con`. CORS headers
# are required since the page (served from wherever the widget itself is
# opened from) and this server (its own random 127.0.0.1 port) are
# different origins from the browser's point of view; a POST with a JSON
# body triggers a preflight OPTIONS request, handled separately below.
vg_duckdb_app <- function(con) {
  cors <- list("Access-Control-Allow-Origin" = "*")
  list(
    call = function(req) {
      if (identical(req$REQUEST_METHOD, "OPTIONS")) {
        return(list(
          status = 204L,
          headers = c(cors, list(
            "Access-Control-Allow-Methods" = "POST, OPTIONS",
            "Access-Control-Allow-Headers" = "Content-Type",
            # Chrome's Private Network Access policy requires this on the
            # preflight before it'll let a fetch from a non-local page
            # origin (e.g. a file:// page, or eventually any non-private
            # origin) reach a private address like 127.0.0.1 -- without it,
            # the request never even leaves the browser and surfaces to JS
            # as a generic "TypeError: Failed to fetch", indistinguishable
            # from the server simply not running.
            "Access-Control-Allow-Private-Network" = "true"
          )),
          body = NULL
        ))
      }

      query <- tryCatch(
        jsonlite::fromJSON(rawToChar(req$rook.input$read()), simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (is.null(query) || is.null(query$sql)) {
        return(list(status = 400L, headers = cors, body = "Malformed query request."))
      }
      type <- query$type
      if (is.null(type)) type <- "arrow"

      tryCatch(
        vg_duckdb_query_response(con, type, query$sql, cors),
        error = function(e) list(status = 500L, headers = cors, body = conditionMessage(e))
      )
    }
  )
}

vg_duckdb_query_response <- function(con, type, sql, cors) {
  if (type == "exec") {
    DBI::dbExecute(con, sql)
    return(list(status = 200L, headers = cors, body = ""))
  }
  if (type == "json") {
    rows <- jsonlite::toJSON(DBI::dbGetQuery(con, sql), dataframe = "rows", na = "null", auto_unbox = FALSE)
    return(list(
      status = 200L,
      headers = c(cors, list("Content-Type" = "application/json")),
      body = as.character(rows)
    ))
  }
  if (type == "arrow") {
    stream <- duckdb::dbFetchArrow(duckdb::dbSendQueryArrow(con, sql))
    rc <- rawConnection(raw(0), "w+b")
    on.exit(close(rc), add = TRUE)
    nanoarrow::write_nanoarrow(stream, rc)
    return(list(
      status = 200L,
      headers = c(cors, list("Content-Type" = "application/vnd.apache.arrow.stream")),
      body = rawConnectionValue(rc)
    ))
  }
  stop("Unknown mosaic query type: `", type, "`.", call. = FALSE)
}

# duckdb_register() maps an R factor to DuckDB's own ENUM type, which
# DuckDB's Arrow exporter then dictionary-encodes -- and DuckDB's Arrow IPC
# writer can't serialize a dictionary-encoded array at all ("ArrowIpcWriter
# WriteArrayStream() failed: Cannot encode dictionary arrays", confirmed
# directly). Converting factor columns to plain character first sidesteps
# it entirely, and also matches what the wasm/JSON-embedding path already
# does to a factor column (df_to_rows()/jsonlite::toJSON() has no concept
# of "factor" either -- it's already just a string there), so native mode
# doesn't newly diverge in how a factor column renders.
drop_factors <- function(df) {
  is_factor_col <- vapply(df, is.factor, logical(1))
  if (any(is_factor_col)) df[is_factor_col] <- lapply(df[is_factor_col], as.character)
  df
}

# duckdb_register() has no idea a double-classed column is actually a
# bit64::integer64 (which stores each value as the raw bit pattern of a
# 64-bit integer, reusing a plain R double vector as the container) -- it
# registers the column as an ordinary DOUBLE and reinterprets those bits as
# an IEEE-754 float, silently corrupting every value with no error
# (confirmed directly: a real integer64 like 9223372036854775800 comes back
# as NaN, and 1 comes back as 4.94e-324). A plain as.double() conversion
# fixes the corruption, but -- unlike drop_factors()'s conversion, which is
# lossless -- it's still lossy past 2^53, so this one warns too. Gated on
# bit64 actually being installed/loaded: it's not a vgplotr dependency, and
# a data.frame can't contain a real integer64 column without it anyway.
convert_integer64_cols <- function(df, name) {
  if (!requireNamespace("bit64", quietly = TRUE)) return(df)
  is_int64 <- vapply(df, bit64::is.integer64, logical(1))
  if (any(is_int64)) {
    warning(
      "Data source '", name, "' has integer64 column(s) (",
      paste(names(df)[is_int64], collapse = ", "), ") -- converting to a ",
      "plain double for the native DuckDB connector, which loses exact ",
      "precision past 2^53. Convert to a regular integer or double column ",
      "yourself first if exact large values matter.",
      call. = FALSE
    )
    # bit64::as.double.integer64() emits its own "precision lost" warning on
    # every call where it applies -- suppressed here since the warning()
    # above already tells the caller the same thing, just once and by name.
    df[is_int64] <- suppressWarnings(lapply(df[is_int64], as.double))
  }
  df
}

# The native-mode counterpart of loadTables() (inst/htmlwidgets/vgplotr.js)
# for a JSON/YAML *string* spec: as_spec_payload() lifts such a spec's inline
# row-object arrays out of its `data:` block into `tables` (see
# lift_inline_data_sources(), R/serialize.R), so under a native connector --
# which never ships `tables` to the browser -- they'd otherwise just vanish.
# Rows are turned into a data.frame and registered directly, exactly like a
# vgspec's own data-frame sources (register_native_data_sources()), rather
# than round-tripped through DuckDB's `read_json_auto()` the way the wasm
# side does: that needs DuckDB's `json` extension, which a native R DuckDB
# may not have and can't always fetch (confirmed directly -- autoload fails
# offline). One consequence: an ISO-8601 timestamp *string* stays a plain
# VARCHAR here, where wasm's `read_json_auto()` would infer TIMESTAMP.
register_native_inline_tables <- function(tables, con) {
  for (nm in names(tables)) {
    json <- jsonlite::toJSON(tables[[nm]], auto_unbox = TRUE, null = "null", digits = NA)
    df <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)
    if (!is.data.frame(df)) {
      stop(
        "Can't load inline data '", nm, "' into a native DuckDB: its rows ",
        "aren't flat (every value needs to be a single scalar).",
        call. = FALSE
      )
    }
    duckdb::duckdb_register(con, nm, df, overwrite = TRUE)
  }
  invisible(NULL)
}

# Registers a spec's own data.frame/local-file sources (vg_data_source_kind(),
# R/serialize.R) directly into a native DuckDB connection -- the native-mode
# counterpart of as_spec_payload()'s embed-for-the-browser handling.
# data.frame sources are a genuine zero-copy registration
# (duckdb::duckdb_register(), "no data is copied"); local files are read by
# DuckDB itself server-side instead of being read+base64-embedded by R and
# shipped to the browser. Anything else (query=, a real http(s) file=) is
# already connector-agnostic and needs no handling here at all.
register_native_data_sources <- function(spec, con) {
  reader_fn <- c(parquet = "read_parquet", csv = "read_csv_auto", json = "read_json_auto")
  for (nm in names(spec$data)) {
    src <- spec$data[[nm]]
    kind <- vg_data_source_kind(src)
    if (kind == "table") {
      # No warn_ordered_factor_cols() call here: vg_widget() always calls
      # as_spec_payload() first, connector-independent, and its own "table"
      # branch already fires this same warning once -- adding it here too
      # would double-warn on every native-connector render.
      data <- convert_integer64_cols(drop_factors(src$data), nm)
      duckdb::duckdb_register(con, nm, data, overwrite = TRUE)
    } else if (kind == "local_file") {
      if (!file.exists(src$file)) {
        stop(
          "Data file not found: '", src$file, "' (looked relative to the ",
          "current working directory, ", getwd(), ").",
          call. = FALSE
        )
      }
      ext <- tolower(tools::file_ext(src$file))
      reader <- reader_fn[[ext]]
      if (is.null(reader)) {
        stop(
          "Don't know how to load a local data file with extension `.", ext, "` ",
          "(expected .csv, .json, or .parquet).",
          call. = FALSE
        )
      }
      DBI::dbExecute(con, sprintf(
        "CREATE OR REPLACE VIEW %s AS SELECT * FROM %s(%s)",
        DBI::dbQuoteIdentifier(con, nm), reader, DBI::dbQuoteString(con, normalizePath(src$file))
      ))
    }
  }
  invisible(NULL)
}
