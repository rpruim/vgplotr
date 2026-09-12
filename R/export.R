#' Convert a vgspec (or plot fragment) to mosaic-spec JSON
#'
#' Produces the same kind of JSON mosaic's own spec files use (e.g. the
#' examples under `docs/public/specs/json/` in uwdata/mosaic) -- useful for
#' inspecting exactly what a spec built with vgplotr's R API translates to,
#' or for saving/sharing it independent of R. Works on a full `vgspec` (from
#' [vg_create()]) or a bare fragment that hasn't been wrapped in one -- a
#' chain of piped marks/interactors, a [vg_vconcat()]/[vg_hconcat()] layout,
#' or a standalone [vg_legend()]/[vg_slider()]/etc.
#'
#' Unlike [vg_render()]'s own internal widget payload, this always keeps
#' data sources exactly as mosaic-spec itself represents them: a data frame
#' becomes an inline array of row objects (mosaic-spec's own
#' `{data: [...]}` form), and a `file =`/`query =` source is left as a
#' plain reference rather than having its content read and embedded.
#'
#' @param spec A `vgspec`, or a bare layout fragment.
#' @param pretty Whether to indent the JSON for readability (default `TRUE`).
#' @param ... Additional arguments passed on to [jsonlite::toJSON()].
#' @return A `json` object (see [jsonlite::toJSON()]); printing it shows the
#'   raw JSON text.
#' @family spec export function
#' @export
to_json <- function(spec, pretty = TRUE, ...) {
  # Built via modifyList()/do.call() rather than passed as literal named
  # arguments alongside `...`, so a caller who explicitly supplies one of
  # these defaults (e.g. `auto_unbox = FALSE`) overrides it cleanly instead
  # of colliding ("formal argument matched by multiple actual arguments").
  args <- utils::modifyList(
    list(x = spec_to_list(spec), auto_unbox = TRUE, dataframe = "rows", null = "null", pretty = pretty),
    list(...)
  )
  do.call(jsonlite::toJSON, args)
}

#' Convert a vgspec (or plot fragment) to mosaic-spec YAML
#'
#' The YAML counterpart to [to_json()] -- see its documentation for what
#' gets converted and how. Produces the same kind of YAML mosaic's own spec
#' files use (e.g. the examples under `docs/public/specs/yaml/` in
#' uwdata/mosaic).
#'
#' @param spec A `vgspec`, or a bare layout fragment.
#' @param ... Additional arguments passed on to [yaml::as.yaml()].
#' @return A `vg_yaml` object (a character string with a `print()` method
#'   that writes it out unquoted/unescaped); use [writeLines()] or
#'   `cat(..., file = ...)` to save it to disk.
#' @family spec export function
#' @export
to_yaml <- function(spec, ...) {
  # yaml::as.yaml() defaults to YAML 1.1's `yes`/`no` for logicals, which a
  # YAML-1.2-core-schema parser (e.g. the JS `js-yaml` library mosaic itself
  # is likely to use) reads back as the plain strings "yes"/"no", not
  # booleans -- so `view: true` would silently stop meaning what it says.
  # Rendering logicals as literal, unquoted "true"/"false" avoids that.
  handlers <- list(
    logical = function(x) structure(if (isTRUE(x)) "true" else "false", class = "verbatim")
  )
  # column.major = FALSE: yaml::as.yaml() otherwise auto-detects a list of
  # same-shaped row objects (e.g. our inline data-frame rows) as "tabular"
  # and silently transposes it back into column-major form -- exactly
  # undoing df_to_rows() -- so this has to be turned off explicitly. Built
  # via modifyList()/do.call() (see to_json()) so a caller-supplied
  # `handlers =`/`column.major =` in `...` overrides these defaults instead
  # of colliding with them.
  args <- utils::modifyList(
    list(x = spec_to_list(spec), handlers = handlers, column.major = FALSE),
    list(...)
  )
  new_vg_yaml(do.call(yaml::as.yaml, args))
}

new_vg_yaml <- function(text) structure(text, class = "vg_yaml")

#' @export
print.vg_yaml <- function(x, ...) {
  cat(x)
  invisible(x)
}
