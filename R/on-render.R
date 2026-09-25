#' Run JavaScript after a graphic renders
#'
#' Some behavior can't be written in a mosaic spec: a custom *client* that runs
#' its own query and reacts to a selection (a live count of the selected rows,
#' say), or a *clause* published to a selection from your own controls.
#' `vg_on_render()` adds a JavaScript function that vgplotr calls in the browser
#' once the graphic has rendered, with what it needs to do such things. Like
#' [js()], of which it is a use, the code runs with the page's own privileges,
#' so only use code you wrote or trust.
#'
#' The function receives one object, with these properties:
#'
#' * `element`: the rendered graphic's root DOM element. Append your own
#'   elements to it (or read from it).
#' * `params`: a `Map` from each param/selection name (see [vg_params()]) to
#'   its mosaic `Param` or `Selection`. A name that is only referred to, such
#'   as an interactor's `as = param(brush)`, is included.
#' * `coordinator`: the graphic's mosaic coordinator, which runs its queries.
#' * `vg`: the vgplot API bound to that coordinator, including the SQL builders
#'   (`vg.Query`, `vg.sql`, `vg.count()`, ...).
#' * `mosaic`: the `mosaic-core` module, for `MosaicClient`, `Selection`,
#'   `Priority` and the clause helpers (`mosaic.clausePoint()`,
#'   `mosaic.clauseInterval()`, `mosaic.clauseMatch()`, ...).
#' * `makeClient(options)`: mosaic's `makeClient()`, already connected to this
#'   graphic's coordinator. Prefer it to `mosaic.makeClient()`, which defaults to
#'   mosaic's global coordinator, a different one.
#'
#' The function may be `async`, and may be added more than once (they run in
#' order). If it throws, the error is shown under the graphic.
#'
#' Hooks are not part of mosaic-spec, so [to_json()]/[to_yaml()] leave them
#' out (with a warning), and they only apply to a spec built with
#' [vg_create()], not to a JSON/YAML string given to [vg_widget()].
#'
#' See `vignette("custom-javascript")` for a worked example.
#'
#' @param spec A `vgspec`.
#' @param code JavaScript, written with [js()], that evaluates to a function.
#' @return The `vgspec`, with the hook added.
#' @family spec functions
#' @export
#' @examples
#' # a live count of the rows inside a brush
#' set.seed(1)
#' pts <- data.frame(x = runif(100), y = runif(100))
#' vg_create() |>
#'   vg_data("pts", pts) |>
#'   vg_mark_dot(x = ~x, y = ~y, data_from = "pts") |>
#'   vg_interval_x(as = param(brush)) |>
#'   vg_on_render(js("({ element, params, makeClient, vg }) => {
#'     const label = element.appendChild(document.createElement('div'));
#'     makeClient({
#'       selection: params.get('brush'),
#'       query: filter => vg.Query.from('pts').select({ n: vg.count() }).where(filter),
#'       queryResult: data => { label.textContent = [...data][0].n + ' selected'; }
#'     });
#'   }"))
vg_on_render <- function(spec, code) {
  if (!is_vgspec(spec)) {
    stop("`spec` must be a vgspec, e.g., from vg_create().", call. = FALSE)
  }
  if (!is_vg_js(code)) {
    stop(
      "`code` must be JavaScript written with js(), e.g., ",
      "js(\"({ element }) => { ... }\").",
      call. = FALSE
    )
  }
  spec$on_render <- c(spec$on_render, list(code))
  spec
}

# The hooks as what vgplotr.js reads (`x.onRender`): the code strings.
on_render_payload <- function(spec) {
  if (!is_vgspec(spec) || length(spec$on_render) == 0) return(NULL)
  vapply(spec$on_render, function(hook) hook$code, character(1))
}

# Hooks are code for vgplotr's own renderer; a mosaic-spec has nowhere to put them.
warn_on_render_dropped <- function(spec) {
  if (is_vgspec(spec) && length(spec$on_render)) {
    warning(
      "vg_on_render() hooks are not part of mosaic-spec, so they are left out ",
      "of the exported spec.",
      call. = FALSE
    )
  }
  invisible(NULL)
}
