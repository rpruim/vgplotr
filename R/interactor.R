# Which vg_interactor() types are embedded inside a plot's mark list
# (mosaic calls these "interactors"/selections: intervalX, toggle, pan, ...)
# vs. which live in the surrounding layout, as standalone widgets (mosaic
# calls these "inputs": slider, menu, table, search).
#
# .vg_interactor_types/.vg_input_types come from mosaic's own JSON schema
# (R/interactors-generated.R, produced by data-raw/update-schema.R).
vg_interactor_placement <- function(interactor) {
  if (interactor %in% .vg_interactor_types) "plot"
  else if (interactor %in% .vg_input_types) "layout"
  else stop(
    "Unknown interactor type `", interactor, "`. If this is a valid mosaic ",
    "interactor/input type, rerun data-raw/update-schema.R (it may need ",
    "MOSAIC_VERSION bumped first).",
    call. = FALSE
  )
}

#' Add an interactor (a selection like `intervalX`, or a standalone input
#' widget like a slider) to a spec
#'
#' Mosaic distinguishes interactors that live inside a plot, alongside its
#' marks (e.g., `intervalX`, which brushes a Selection), from inputs that live
#' in the surrounding layout as standalone widgets (e.g., `slider`, `menu`).
#' `vg_interactor()` covers both; which kind a given `interactor` is gets
#' looked up internally (see `vg_interactor_placement()`). One convenience
#' wrapper per type -- `vg_toggle()`, `vg_menu()`, etc. -- is generated from
#' mosaic's own JSON schema into `R/interactors-generated.R`.
#'
#' A plot-embedded interactor selects on the mark just before it in its plot
#' -- the columns mapped to that mark's `x`, `y`, `fill`, ... channels -- so
#' add it right after the mark it's for (`vg_mark_dot(...) |> vg_interval_x(...)`,
#' then any other marks). Adding one where the mark before it doesn't map the
#' channel it needs to a data column (or where there is no mark before it)
#' warns, since the interactor would have nothing to select on.
#'
#' The second argument is named `interactor`, not `type`, on purpose: some
#' interactor/input types have their own unrelated option that mosaic calls
#' `type` (e.g., `vg_search()`'s `type = "prefix"`, its query mode). Naming
#' this parameter `type` would collide with that whenever both are supplied
#' -- `interactor` can never collide with a real mosaic-spec property name.
#'
#' @param spec For a plot-embedded interactor: a plot fragment or `vgspec`
#'   to add it to, or `NULL` to start a new plot with just this interactor.
#'   For a layout-level input (e.g., `"slider"`): a `vgspec` with no layout
#'   yet, to make the input its whole layout, or `NULL` to return the input
#'   on its own -- combine it with plots using [vg_vconcat()]/[vg_hconcat()].
#' @param interactor The interactor/input type, e.g., `"intervalX"`, `"slider"`.
#' @param ... Options for the interactor/input (e.g., `as = param(brush)`,
#'   `label = "Bias"`, `min = 0`, `max = 100`), and/or, for plot-embedded
#'   interactors, plot-level attributes. Like [vg_mark()], `vg_interactor()`
#'   itself takes each option under mosaic-spec's own camelCase name
#'   (`filterBy =`), where the generated wrappers take snake_case
#'   (`filter_by =`). An argument that is neither an option of this
#'   interactor/input type in mosaic-spec nor (for a plot-embedded
#'   interactor) a plot-level attribute warns, since mosaic would silently
#'   ignore it.
#' @family interactor functions
#' @export
vg_interactor <- function(spec = NULL, interactor, ...) {
  build_interactor(spec, interactor, list(...), style = "camel")
}

# The body of vg_interactor(), shared with the generated wrappers (see
# vg_interactor_() below); `style` is as for build_mark() (R/mark.R).
build_interactor <- function(spec, interactor, args, style) {
  placement <- vg_interactor_placement(interactor)

  if (placement == "plot") {
    split <- split_plot_args(args, protect = .vg_interactor_own_props[[interactor]])
    warn_unrecognized_interactor_args(split$local_args, interactor, style = style)
    warn_unrecognized_enum_values(args, style)
    check_transform_calls(args, paste0("interactor `", interactor, "`"), style)
    interactor_obj <- structure(
      list(type = interactor, options = split$local_args),
      class = "vg_interactor"
    )
    fragment <- as_vg_plot_fragment(spec)
    warn_interactor_without_mark(fragment, interactor, split$local_args, style)
    fragment$items <- c(fragment$items, list(interactor_obj))
    fragment$attrs <- merge_attrs(fragment$attrs, split$plot_attrs, context = paste0("interactor `", interactor, "`"))
    update_layout(spec, fragment)
  } else {
    # A layout-level input is either returned on its own (spec = NULL), to go
    # into vg_vconcat()/vg_hconcat(), or becomes the whole layout of a spec
    # that doesn't have one yet -- Mosaic's own form for a spec that is just,
    # say, one table (`{"input": "table", ...}` at the top level).
    fn <- paste0("vg_", camel_to_snake(interactor))
    if (!is.null(spec) && !is_vgspec(spec)) {
      stop(
        "`", interactor, "` is a layout-level input; it doesn't take a spec/plot ",
        "to extend -- it can't go inside a plot. Combine it with plots using ",
        "vg_vconcat()/vg_hconcat() instead, e.g., vg_vconcat(", fn, "(...), your_plot).",
        call. = FALSE
      )
    }
    if (is_vgspec(spec) && !is.null(spec$layout)) {
      stop(
        "This spec already has a layout, so the `", interactor, "` input can't ",
        "become its whole layout. Add it to the existing layout with ",
        "vg_vconcat()/vg_hconcat() instead, e.g., spec |> vg_vconcat(", fn, "(...)).",
        call. = FALSE
      )
    }
    warn_unrecognized_interactor_args(args, interactor, kind = "input", style = style)
    warn_unrecognized_enum_values(args, style)
    check_transform_calls(args, paste0("input `", interactor, "`"), style)
    input <- structure(list(type = interactor, options = args), class = "vg_input")
    if (is_vgspec(spec)) {
      spec$layout <- input
      spec
    } else {
      input
    }
  }
}

# Shared by every generated vg_<type>() wrapper (R/interactors-generated.R):
# drops whichever named arguments the caller left at their vg_unset default
# (i.e., didn't actually supply) before dispatching to vg_interactor(). The
# discriminant is always passed positionally by the generated wrappers, so
# naming this parameter `interactor` (matching vg_interactor()'s own,
# collision-safe name -- see its documentation) is enough on its own: a
# same-named real property (e.g., vg_search()'s `type`) now flows through
# `...` untouched instead of being intercepted by exact-name matching.
vg_interactor_ <- function(spec, interactor, ...) {
  build_interactor(spec, interactor, drop_unset(list(...)), style = "snake")
}

#' @export
print.vg_interactor <- function(x, ...) {
  cat("<vg_interactor:", x$type, ">\n")
  print_fields(x$options)
  invisible(x)
}

#' @export
print.vg_input <- function(x, ...) {
  cat("<vg_input:", x$type, ">\n")
  print_fields(x$options)
  invisible(x)
}

# Mosaic attaches a plot-embedded interactor to the mark just before it in
# its plot (vgplot's `plot.marks[plot.marks.length - 1]`, when the interactor
# is added), and reads the data column(s) it selects on from that mark's
# channels. If that mark doesn't map the needed channel to data, an interval
# or nearest interactor silently gets no column -- its selection filters
# with `NULL BETWEEN ...`, matching nothing -- and a toggle or region fails
# when rendered ("Missing channel"). This warns when the interactor is added.

# The channels `interactor` reads from its mark, given its own options -- each
# a list(channel =, match =): "prefix" (any channel whose name starts with
# it, as Mark.channelField() does for intervals and nearest) or "exact" (one
# of a fixed set, as Toggle/Region do). Empty when an option supplies the
# column directly.
interactor_needed_channels <- function(interactor, opts) {
  need <- function(channels, match) lapply(channels, function(ch) list(channel = ch, match = match))
  given_channels <- if (is.character(opts$channels)) opts$channels
  switch(interactor,
    intervalX = if (is.null(opts$field)) need("x", "prefix"),
    intervalY = if (is.null(opts$field)) need("y", "prefix"),
    intervalXY = c(if (is.null(opts$xfield)) need("x", "prefix"), if (is.null(opts$yfield)) need("y", "prefix")),
    nearest = , nearestX = , nearestY = if (is.null(opts$fields)) {
      default <- switch(interactor, nearestX = "x", nearestY = "y", c("x", "y"))
      need(if (!is.null(given_channels)) given_channels else default, "prefix")
    },
    toggleX = need("x", "exact"),
    toggleY = need("y", "exact"),
    toggleZ = need("z", "exact"),
    toggleColor = need("color", "exact"),
    toggle = , region = need(given_channels, "exact"),
    list()
  )
}

# The names of a mark's channels that Mosaic maps to data (a column, SQL
# expression or transform), as opposed to constants and params -- the
# channels an interactor can select on. Includes the extra ones under
# `channels = list(...)`.
mark_data_channels <- function(mark_obj) {
  enc <- mark_obj$encodings
  enc$data_from <- NULL
  enc$filter_by <- NULL
  enc$data_optimize <- NULL
  extra <- enc$channels
  enc$channels <- NULL
  if (is.list(extra) && !is.null(names(extra))) enc <- c(enc, extra)
  is_data <- function(name, v) {
    if (inherits(v, "formula") || is_vg_transform(v) || is_vg_sql_expr(v)) return(TRUE)
    if (is.character(v) && length(v) == 1) return(!mosaic_reads_as_constant(name, v))
    is.list(v) && !is.null(names(v)) && !inherits(v, "vg_param")
  }
  names(enc)[mapply(is_data, names(enc), enc)]
}

has_channel <- function(channels, need) {
  if (need$match == "prefix") return(any(startsWith(channels, need$channel)))
  candidates <- switch(need$channel,
    color = c("color", "fill", "stroke"),
    x = c("x", "x1", "x2"),
    y = c("y", "y1", "y2"),
    need$channel
  )
  any(candidates %in% channels)
}

warn_interactor_without_mark <- function(fragment, interactor, opts, style) {
  needed <- interactor_needed_channels(interactor, opts)
  if (length(needed) == 0) return(invisible())
  fn_name <- function(prefix, type) {
    if (identical(style, "snake")) paste0(prefix, camel_to_snake(type), "()")
    else sprintf('vg_%s("%s")', if (prefix == "vg_") "interactor" else "mark", type)
  }
  this <- fn_name("vg_", interactor)
  marks <- Filter(function(item) inherits(item, "vg_mark"), fragment$items)
  if (length(marks) == 0) {
    warning(
      this, " has no mark before it in its plot to select from. An interactor ",
      "uses the mark just before it, so put it right after that mark, e.g. ",
      "`vg_mark_dot(...) |> ", this, "`.",
      call. = FALSE
    )
    return(invisible())
  }
  prev <- marks[[length(marks)]]
  missing <- Filter(function(n) !has_channel(mark_data_channels(prev), n), needed)
  if (length(missing) == 0) return(invisible())
  chans <- paste0("`", vapply(missing, `[[`, "", "channel"), "`", collapse = " or ")
  better <- Filter(function(m) all(vapply(needed, has_channel, logical(1), channels = mark_data_channels(m))),
                   marks[-length(marks)])
  hint <- if (length(better)) {
    paste0(" (e.g., right after ", fn_name("vg_mark_", better[[length(better)]]$mark), ")")
  } else ""
  warning(
    this, " uses the mark just before it in its plot, ", fn_name("vg_mark_", prev$mark),
    ", which doesn't map ", chans, " to a data column, so it would have nothing ",
    "to select on. Put ", this, " right after the mark it's for", hint, ".",
    call. = FALSE
  )
  invisible()
}
