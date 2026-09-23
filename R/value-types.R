#' What the `<...>` notation in vgplotr's argument docs means
#'
#' Every generated `vg_mark_*()`/`vg_*()` (interactor/input) argument, and
#' every `vg_scale_*()`/`vg_guide_*()` argument, is documented with a short
#' `<option1 | option2 | ...>` notation right after its name, listing what
#' mosaic-spec actually accepts there -- derived directly from mosaic's own
#' JSON schema (`data-raw/update-schema.R`), not hand-written, so it can't
#' drift from what the running mosaic version really supports. This page is
#' the shared vocabulary those notations are built from.
#'
#' @section Channel values:
#' A handful of tokens describe a *channel value* -- something bound to
#' data, computed, or otherwise resolved per row -- rather than one fixed R
#' type. Most mark encodings (`x =`, `y =`, `fill =`, ...) accept all of
#' these; see `vignette("getting-started")` for worked examples of each.
#'
#' \describe{
#'   \item{`column`}{A one-sided formula naming a data column, e.g.
#'     `x = ~mpg`.}
#'   \item{`literal`}{A constant value applied to every row, e.g.
#'     `fill = "steelblue"` or `opacity = 0.5`.}
#'   \item{`transform()`}{A vgplotr transform function, e.g. `vg_bin()`,
#'     `vg_count()`, `vg_avg()` -- computed by DuckDB, not R. See
#'     [vg_transforms] for the full set.}
#'   \item{`sql()`/`agg()`}{A raw SQL expression (`sql()`) or one wrapping
#'     an aggregate function (`agg()`), for anything a column reference or
#'     transform can't express. See [sql()].}
#'   \item{`list(value=, ...)`}{The `{value:, scale:, label:}` object form
#'     mosaic-spec allows for overriding a channel's scale/label directly,
#'     e.g. `x = list(value = ~mpg, scale = "shared")`. Rarely needed --
#'     the plain `column`/`literal`/`transform()`/`sql()` forms above cover
#'     the vast majority of cases.}
#' }
#'
#' A property whose notation includes `list(value=, ...)` (not just
#' `list`) is one of these `{value:, scale:, label:}` channels; a bare
#' `list` elsewhere means an ordinary named list of sub-options specific to
#' that property (its own `@param` text says what goes in it).
#'
#' @section Reactive values:
#' \describe{
#'   \item{`param()`}{A reactive Param or Selection, e.g.
#'     `param(brush)` -- see [param()]. Appears on almost every argument:
#'     mosaic-spec lets nearly any scalar option be driven by a live
#'     `param()` instead of a fixed value (a slider-controlled radius, a
#'     menu-controlled color scheme, ...), not just data-bound channels.}
#' }
#'
#' @section Plain R types:
#' \describe{
#'   \item{`number`}{A single R number, e.g. `2`, `0.5`.}
#'   \item{`string`}{A single character string, e.g. `"steelblue"`.}
#'   \item{`boolean`}{`TRUE` or `FALSE`.}
#'   \item{`NULL`}{Literal `NULL` -- explicitly unsets/disables the
#'     property (e.g. `label = NULL` shows no axis label), distinct from
#'     just leaving the argument unset.}
#'   \item{numeric vector / character vector / vector}{A plain R vector,
#'     e.g. `range = c(0, 20)`; `vector` when mosaic accepts elements of
#'     more than one type (or doesn't constrain them further).}
#'   \item{`list`}{A named list of sub-options specific to that property
#'     (see its own `@param` text) -- not a `{value:, scale:, label:}`
#'     channel (that's `list(value=, ...)` above).}
#'   \item{`any`}{Genuinely unconstrained by mosaic's own schema -- e.g. a
#'     [vg_menu()]'s initial `value`, which can be whatever type its
#'     `options` are.}
#' }
#'
#' @section Fixed sets of literal values:
#' Quoted options (`"basis" | "bundle" | ...`) are the literal strings (or,
#' occasionally, `TRUE`/`FALSE`/`NULL` mixed into the same set) mosaic
#' actually recognizes there -- pass exactly one of them, quoted, e.g.
#' `curve = "basis"`. A set with more than 10 values is abbreviated to the
#' first 10 plus `...`; see the property's own description (same line) or
#' [mosaic's documentation](https://idl.uw.edu/mosaic/) for the rest --
#' most commonly [Observable Plot's color
#' schemes](https://observablehq.com/plot/features/scales#color-scales) for
#' a `scheme =` argument.
#'
#' One special literal, `"Fixed"`, appears on domain-like arguments
#' (`x_domain =`, `color_domain =`, ...): it freezes a domain that was
#' initially computed from the data so it no longer changes under
#' subsequent interactive filtering, for stable before/after comparisons.
#'
#' Two more appear together, `"day"/"week"/"month"/...` and `number`, on
#' interval-like arguments (`interval =`, `ticks =`, ...): either a named
#' time interval (`"day"`, `"week"`, `"3 months"`, ...) or a plain number,
#' defining intervals at that many underlying units.
#'
#' @name vg_value_types
NULL
