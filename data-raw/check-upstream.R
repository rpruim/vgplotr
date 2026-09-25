# Checks whether upstream mosaic bugs vgplotr works around are still there.
#
#   Rscript data-raw/check-upstream.R            # the latest published release
#   Rscript data-raw/check-upstream.R 0.31.0     # a specific version
#
# data-raw/update-schema.R also runs this for the pinned MOSAIC_VERSION, so a
# version bump reports on it too. See "Upstream issues to watch" in AGENTS.md.

# mosaic-spec 0.31.0's parseWindowFrame() (src/ast/WindowFrameNode.js) builds
# each literal frame offset with `new LiteralNode(v)`, imported from
# @uwdata/mosaic-sql -- a SQL AST node with no instantiate(), which
# WindowFrameNode.instantiate() then calls, so a frame with a plain number or
# null throws. The bug is present exactly when that file still takes LiteralNode
# from mosaic-sql; a fixed version would import spec's own ./LiteralNode.js.
# vgplotr.js's patchWindowFrames() is the workaround.
window_frame_bug_present <- function(src) {
  src <- paste(src, collapse = "\n")
  imports_sql_literal <- grepl(
    "import\\s*\\{[^}]*\\bLiteralNode\\b[^}]*\\}\\s*from\\s*['\"]@uwdata/mosaic-sql['\"]", src
  )
  imports_sql_literal && grepl("new\\s+LiteralNode\\(", src)
}

# TRUE/FALSE, or NA if it couldn't be fetched (offline, file moved, ...).
mosaic_window_frame_bug <- function(version = "latest") {
  tryCatch({
    if (identical(version, "latest")) {
      info <- jsonlite::fromJSON(
        "https://data.jsdelivr.com/v1/packages/npm/@uwdata/mosaic-spec/resolved?specifier=latest"
      )
      version <- info$version
    }
    url <- sprintf(
      "https://cdn.jsdelivr.net/npm/@uwdata/mosaic-spec@%s/src/ast/WindowFrameNode.js", version
    )
    structure(window_frame_bug_present(readLines(url, warn = FALSE)), version = version)
  }, error = function(e) structure(NA, version = version, why = conditionMessage(e)))
}

report_window_frame_bug <- function(version = "latest") {
  present <- mosaic_window_frame_bug(version)
  v <- attr(present, "version")
  if (is.na(present)) {
    message("[check-upstream] mosaic-spec@", v, ": couldn't check the window-frame bug (",
            attr(present, "why"), ").")
  } else if (present) {
    message("[check-upstream] mosaic-spec@", v, ": window-frame bug still present; ",
            "keep patchWindowFrames() in inst/htmlwidgets/vgplotr.js.")
  } else {
    message(
      "[check-upstream] mosaic-spec@", v, ": the window-frame bug looks FIXED upstream.\n",
      "  Once the bundle is rebuilt on this version and a frame such as\n",
      "  rows = c(6, 0) renders without patchWindowFrames(), remove it: delete the\n",
      "  function and its call in inst/htmlwidgets/vgplotr.js, drop the caveat in\n",
      "  ?vg_intervals (R/intervals.R) and ?vg_transforms (R/transforms.R), and remove\n",
      "  the 'Upstream issues to watch' entry in AGENTS.md."
    )
  }
  invisible(present)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  report_window_frame_bug(if (length(args)) args[[1]] else "latest")
}
