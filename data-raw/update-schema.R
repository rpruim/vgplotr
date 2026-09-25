# Regenerates R/marks-generated.R, R/interactors-generated.R, and
# R/attrs-generated.R from mosaic's own published JSON schema -- the same
# source of truth uwdata/mosaic's own bin/generate-python-api.js uses to
# generate the Python API's marks/attributes.
#
# Run interactively (source this file, or `Rscript data-raw/update-schema.R`)
# whenever MOSAIC_VERSION here is bumped (keep it in sync with the version
# pinned in inst/htmlwidgets/vgplotr.js, so the functions this generates
# match what the runtime the browser actually loads supports) or whenever
# vgplotr seems to be missing a mark/interactor/attribute mosaic supports.
#
# Every vg_mark_<mark>()/vg_<type>() wrapper gets a real, named formal
# argument per property the schema defines for it (rather than just `...`),
# each documented from the schema's own description -- so tab completion
# and `?vg_mark_dot` show the actual options, not just "...". The
# lower-level vg_mark()/vg_interactor() they're built on stay `...`-based
# on purpose, since they take an arbitrary/dynamic mark or interactor type.
#
# Each formal's R-facing name is snake_case (mosaic's `strokeWidth` becomes
# `stroke_width`), matching the rest of the package (vg_scale_*()/
# vg_guide_*() are snake_case too) -- but it's forwarded to vg_mark()/
# vg_interactor() under its exact original schema name, since that's the
# key mosaic's JSON spec actually needs. This is a clean-break rename with
# no camelCase compatibility alias: existing code calling e.g.,
# `vg_mark_dot(strokeWidth = 2)` needs updating to `stroke_width = 2`.
#
# Mark wrappers are prefixed vg_mark_ (not just vg_<mark>()) so every mark
# constructor can be found by listing functions starting "vg_mark_", and so
# a mark name never collides with an unrelated vg_<name>() function
# elsewhere in the package -- this happened for real with mosaic's axisX/
# axisY/axisFx/axisFy marks, which collided with the plot-attribute guide
# setters in R/guide.R (vg_guide_x()/etc., not vg_axis_x()/etc., because of
# this exact collision) before this prefix was introduced. Interactor/input
# wrappers keep the plain vg_<type>() form since no such collision has
# arisen for them.
#
# What this does NOT cover (hand-written, not schema-driven): transforms
# (R/transforms.R -- a different schema file, Transform.ts, without a
# corresponding section in the JSON schema's mark/attribute defs), legends
# (only 3 types, small enough to maintain by hand in R/legend.R), and
# data-source options (R/create.R).

MOSAIC_VERSION <- "0.31.0"

# Reports whether the upstream bug behind vgplotr.js's patchWindowFrames() is
# fixed in this version (never blocks generation: it only prints).
source("data-raw/check-upstream.R", local = TRUE)
report_window_frame_bug(MOSAIC_VERSION)

schema_url <- sprintf(
  "https://cdn.jsdelivr.net/npm/@uwdata/mosaic-spec@%s/dist/mosaic-schema.json",
  MOSAIC_VERSION
)
schema <- jsonlite::fromJSON(schema_url, simplifyVector = FALSE)
defs <- schema$definitions

camel_to_snake <- function(name) {
  tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", name))
}

ref_name <- function(ref) sub("^#/definitions/", "", ref$`$ref`)

or_else <- function(x, default) if (is.null(x)) default else x

# First sentence of a schema description, markdown links/footnotes
# stripped, collapsed to one line -- mirrors mosaic's own docline() in
# bin/generate-python-api.js.
docline <- function(desc, fallback) {
  text <- desc
  if (is.null(text) || !nzchar(trimws(text))) text <- fallback
  text <- gsub(r"(\[([^]]+)\]\([^)]*\))", "\\1", text) # [text](url)
  text <- gsub(r"(\[([^]]+)\]\[[^]]*\])", "\\1", text) # [text][ref]
  text <- gsub(r"(\[(\d+)\])", "", text)                # bare footnote [1]
  text <- gsub(r"(\[([^]]+)\])", "\\1", text)           # [text] shortcut
  text <- gsub("[\r\n]+", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  first <- strsplit(text, "(?<=\\.)\\s", perl = TRUE)[[1]][1]
  if (is.na(first) || !nzchar(first)) first <- fallback
  # A literal backslash in the schema text (e.g., "line breaks (\n, \r\n, or
  # \r)", describing escape sequences as a documentation topic, not actual
  # newlines) would otherwise reach the .Rd file unescaped, where Rd's own
  # macro processor tries to interpret \n/\r as macro calls ("unknown macro
  # '\n'"). Doubling it is the markdown-level escape for a literal
  # backslash, which roxygen2 (Roxygen: list(markdown = TRUE)) then turns
  # into a properly Rd-escaped backslash. Raw strings make the intent
  # legible here: r"(\)" is one literal backslash, r"(\\)" is two, vs. the
  # equivalent normal-string forms "\\" and "\\\\".
  gsub(r"(\)", r"(\\)", first, fixed = TRUE)
}

# A mark definition either has `properties` directly, or is a union
# (`anyOf`/`allOf`) whose branches all agree on the same
# `properties.mark.const` (e.g., densityX/densityY) -- mirrors mosaic's own
# markInfo() in bin/generate-python-api.js. Returns NULL for a non-mark def.
#
# `required` collects the schema's own required-property lists too (beyond
# `mark`/`data`, stripped below along with the properties themselves) --
# these become real, default-less R arguments instead of `= vg_unset`, so
# e.g., omitting ErrorBarX's `x` fails immediately in R with a clear message
# instead of silently producing a spec that errors in the browser. A
# property that's required in every branch but with a *different* `const`
# per branch (e.g., densityX's `type`: "areaX"/"lineX"/"dotX"/"textX") is a
# branch discriminant, not a true requirement -- mosaic applies its own
# default for it (see its description), so it's excluded here and stays
# `vg_unset` like everything else.
mark_info <- function(def) {
  if (!is.null(def$properties$mark$const)) {
    return(list(
      mark = def$properties$mark$const, description = def$description,
      properties = def$properties, required = unlist(or_else(def$required, character()))
    ))
  }
  branches <- or_else(def$anyOf, def$allOf)
  consts <- unique(unlist(lapply(branches, function(b) b$properties$mark$const)))
  if (length(consts) != 1) return(NULL)
  props <- list()
  required <- character()
  for (b in branches) {
    props <- utils::modifyList(props, or_else(b$properties, list()))
    required <- union(required, unlist(or_else(b$required, character())))
  }
  for (p in required) {
    consts_p <- unique(unlist(lapply(branches, function(b) b$properties[[p]]$const)))
    if (length(consts_p) > 1) required <- setdiff(required, p)
  }
  list(mark = consts, description = def$description, properties = props, required = required)
}

mark_defs <- Filter(Negate(is.null), lapply(defs, mark_info))
names(mark_defs) <- vapply(mark_defs, function(m) m$mark, character(1))

# Captured before "data" is stripped below: a handful of purely-decorative
# marks (frame, sphere, hexgrid, graticule, the axis/grid marks) have no
# `data` property in the schema at all -- they compute their own geometry
# and never take a backing table. vg_mark()'s "default data_from to the
# spec's first data source" convenience (R/mark.R) must never apply to
# these, or it would add an invalid `data` key these marks don't declare.
mark_has_data <- vapply(mark_defs, function(m) "data" %in% names(m$properties), logical(1))

for (nm in names(mark_defs)) {
  mark_defs[[nm]]$properties <- mark_defs[[nm]]$properties[!names(mark_defs[[nm]]$properties) %in% c("mark", "data")]
  mark_defs[[nm]]$required <- setdiff(mark_defs[[nm]]$required, c("mark", "data"))
}

interactor_types <- vapply(
  schema$definitions$PlotInteractor$anyOf,
  function(r) defs[[ref_name(r)]]$properties$select$const,
  character(1)
)
input_types <- vapply(
  c("Menu", "Search", "Slider", "Table"),
  function(nm) defs[[nm]]$properties$input$const,
  character(1)
)
interactor_defs <- list()
for (r in schema$definitions$PlotInteractor$anyOf) {
  d <- defs[[ref_name(r)]]
  interactor_defs[[d$properties$select$const]] <- list(
    description = d$description,
    properties = d$properties[setdiff(names(d$properties), "select")]
  )
}
for (nm in c("Menu", "Search", "Slider", "Table")) {
  d <- defs[[nm]]
  interactor_defs[[d$properties$input$const]] <- list(
    description = d$description,
    properties = d$properties[setdiff(names(d$properties), "input")]
  )
}

# Interactors mosaic's runtime parser accepts but its JSON schema omits.
# parseSpec() recognizes every directive vgplot exports (plotInteractorNames()
# is Object.keys(interactorDirectives)), but PlotInteractor lists only
# NearestX/NearestY and Toggle/ToggleX/ToggleY: `nearest` (both x and y; its
# Nearest interface exists in mosaic-spec's Nearest.ts but isn't in the
# PlotInteractor union) and `toggleZ` (toggle over the z channel) never made
# it in. Each takes exactly the options of a sibling schema type -- confirmed
# in vgplot's interactors.js, where nearest()/toggleZ() differ from
# nearestX()/toggleY() only in the `pointer`/`channels` they fix -- so clone
# that sibling's properties instead of hand-writing them. A future schema
# that lists one of these itself is skipped: it is already generated.
runtime_only_interactors <- c(nearest = "NearestX", toggleZ = "ToggleY")
for (nm in names(runtime_only_interactors)) {
  if (nm %in% interactor_types) next
  sibling <- interactor_defs[[defs[[runtime_only_interactors[[nm]]]]$properties$select$const]]
  stopifnot(!is.null(sibling))
  interactor_defs[[nm]] <- list(
    description = paste0("A ", nm, " interactor."),
    properties = sibling$properties
  )
  interactor_types <- c(interactor_types, nm)
}

plot_attrs <- sort(names(defs$PlotAttributes$properties))

# Some property names exist in *both* a mark/interactor's own schema (e.g.,
# RectY's own `inset`, shrinking just that mark's rects) and
# PlotAttributes (e.g., a plot-wide `inset` default for every mark's scale)
# -- same name, different scope, because mosaic-spec disambiguates by
# *where* the key appears (inside a mark's own object vs. the plot's own
# attrs), a distinction split_plot_args() (R/utils.R) can't make from a
# flat `...` by name alone. These per-type "own property" lists let it
# special-case names a specific mark/interactor actually declares, so
# e.g., `vg_mark_rect_y(inset = 1)` keeps `inset` on the mark instead of
# always bubbling same-named properties up to the plot.
mark_own_props <- lapply(mark_defs, function(m) sort(names(m$properties)))
interactor_own_props <- lapply(interactor_defs, function(m) sort(names(m$properties)))

# A legend's properties (PlotLegend -- the same set for all three legend
# types, color/opacity/symbol, which differ only in the `legend` value
# itself). Excludes the `legend` discriminant, which vg_legend() takes as
# its own `type` argument; the standalone form's extra `for` property (the
# separate `Legend` def) is likewise vg_legend()'s own `for_plot` argument,
# not an option. Used by warn_unrecognized_legend_args() (R/utils.R) to
# catch an option mosaic would silently ignore. Legend *constructors* stay
# hand-written (R/legend.R) -- only this property list is schema-derived,
# so it can't go stale when MOSAIC_VERSION is bumped.
stopifnot(!is.null(defs$PlotLegend$properties))
legend_props <- sort(setdiff(names(defs$PlotLegend$properties), "legend"))

cat(length(mark_defs), "marks,", length(interactor_types), "interactors,",
    length(input_types), "inputs,", length(plot_attrs), "plot attributes\n")

# vg_mark_()/vg_interactor_() (R/mark.R, R/interactor.R) pass a mark/
# interactor's own properties through to vg_mark()/vg_interactor() using
# their *exact* schema names, so that e.g., Search's own "type" option (its
# query mode) can be set under its real name. That only works because
# vg_mark()'s/vg_interactor()'s own discriminant parameters are named
# `mark`/`interactor`, not (say) `type` -- R matches named arguments by
# exact name before position, so a same-named forwarded property would
# otherwise silently hijack the discriminant slot instead of landing in
# `...` (this happened for real with Search's "type" during development).
# Fail loudly here if a future schema version ever adds a mark property
# literally called "mark" or an interactor/input property literally called
# "interactor", rather than letting that ship as a silent, hard-to-spot bug.
stopifnot(
  "a mark now has a property literally named `mark`, which would collide with vg_mark()'s discriminant parameter -- rename it there too" =
    !any(vapply(mark_defs, function(m) "mark" %in% names(m$properties), logical(1))),
  "an interactor/input now has a property literally named `interactor`, which would collide with vg_interactor()'s discriminant parameter -- rename it there too" =
    !any(vapply(interactor_defs, function(m) "interactor" %in% names(m$properties), logical(1)))
)

# camel_to_snake() is not invertible (fooBAR and fooBar both become
# foo_bar), so a future schema version could in principle introduce two
# differently-camelCased properties on the *same* mark/interactor that
# collide once translated. Fail loudly here instead of silently merging
# two properties into one R argument -- checked empirically against
# v0.31.0 with zero collisions (design/design-reflections.qmd question 2c).
check_snake_collisions <- function(type_defs, kind) {
  for (nm in names(type_defs)) {
    props <- names(type_defs[[nm]]$properties)
    dupes <- unique(props[duplicated(camel_to_snake(props))])
    if (length(dupes)) {
      stop(
        "camel_to_snake() collision on ", kind, " `", nm, "`: ",
        paste(dupes, collapse = ", "),
        " -- add a manual rename before regenerating.",
        call. = FALSE
      )
    }
  }
}
check_snake_collisions(mark_defs, "mark")
check_snake_collisions(interactor_defs, "interactor/input")

# Same check for PlotAttributes itself -- vg_plot()/vg_plot_defaults()/
# vg_attributes()/vg_create() accept these under a snake_case translation
# too (.vg_plot_attrs_snake below), which depends on camel_to_snake()
# being collision-free across all 215 names. Checked empirically against
# v0.31.0 with zero collisions.
{
  plot_attr_dupes <- unique(plot_attrs[duplicated(camel_to_snake(plot_attrs))])
  if (length(plot_attr_dupes)) {
    stop(
      "camel_to_snake() collision among PlotAttributes properties: ",
      paste(plot_attr_dupes, collapse = ", "),
      " -- add a manual rename before regenerating.",
      call. = FALSE
    )
  }
}

# Same check for legend properties -- vg_legend()/vg_legend_color()/etc.
# accept these under a snake_case translation too (.vg_legend_props_snake
# below), which depends on camel_to_snake() being collision-free across
# all of them.
{
  legend_prop_dupes <- unique(legend_props[duplicated(camel_to_snake(legend_props))])
  if (length(legend_prop_dupes)) {
    stop(
      "camel_to_snake() collision among PlotLegend properties: ",
      paste(legend_prop_dupes, collapse = ", "),
      " -- add a manual rename before regenerating.",
      call. = FALSE
    )
  }
}

# First-seen description for a given property name, reused across every
# mark/interactor/input that has a property of that name (these mean the
# same thing everywhere in mosaic's grammar, so one description per name is
# both accurate and far less repetitive than re-deriving one per type).
property_docs <- function(type_defs) {
  docs <- list()
  for (nm in names(type_defs)) {
    props <- type_defs[[nm]]$properties
    for (p in names(props)) {
      if (is.null(docs[[p]])) docs[[p]] <- docline(props[[p]]$description, p)
    }
  }
  docs
}
mark_prop_docs <- property_docs(mark_defs)
interactor_prop_docs <- property_docs(interactor_defs)

# --- Value-type notation (e.g. "<number | param()>") -----------------------
#
# Prepended to each generated @param line, right after the argument name,
# so `?vg_mark_dot` shows what a property actually *accepts* -- not just a
# prose description -- without anyone having to read mosaic-spec's own
# TypeScript. The handful of short tokens this produces (column, literal,
# transform(), sql()/agg(), param(), list, any, ...) are shared across
# every mark/interactor/attribute and documented once in ?vg_value_types
# rather than re-explained per property.

# The literal value of a JSON const/enum entry, ready to display: a
# quoted string, bare TRUE/FALSE/NULL for a boolean/null literal (mosaic's
# schema really does mix these into one enum sometimes, e.g. LabelArrow's
# `["auto", ..., true, false, null]` -- naively coercing the whole enum
# through as.character() before inspecting each element, the way
# collect_enum_values() below deliberately doesn't need to, would
# silently turn TRUE into the misleading string "TRUE").
literal_token <- function(v) {
  if (is.character(v)) sprintf('"%s"', v)
  else if (is.logical(v) && !is.na(v)) if (v) "TRUE" else "FALSE"
  else if (is.null(v)) "NULL"
  else as.character(v)
}

# A sibling of collect_enum_values() below, for the same const/enum/$ref/
# anyOf/oneOf/allOf shape, but collecting *every* literal (including
# boolean/null, not just strings) as a display-ready token -- this one
# feeds the type notation, where a non-string literal still needs to be
# shown; collect_enum_values() feeds typo-suggestion matching, where only
# strings are ever a plausible typo target.
collect_enum_tokens <- function(node, defs, seen = character(), depth = 0) {
  if (depth > 8 || is.null(node)) return(character())
  vals <- character()
  if (!is.null(node$const)) vals <- c(vals, literal_token(node$const))
  if (!is.null(node$enum)) for (v in node$enum) vals <- c(vals, literal_token(v))
  if (!is.null(node$`$ref`)) {
    rn <- ref_name(node)
    if (!(rn %in% seen)) vals <- c(vals, collect_enum_tokens(defs[[rn]], defs, c(seen, rn), depth + 1))
  }
  for (key in c("anyOf", "oneOf", "allOf")) {
    for (b in node[[key]]) vals <- c(vals, collect_enum_tokens(b, defs, seen, depth + 1))
  }
  vals
}

# A property schema node with none of these keys is JSON Schema's `{}` --
# "any value", unconstrained -- e.g. Menu's own `value` (its initial
# selection, whatever type the menu's own options happen to be) or
# PlotAttributes' `colorPivot` (compared against arbitrary domain values).
is_empty_schema <- function(node) {
  keys <- c("type", "const", "enum", "$ref", "anyOf", "oneOf", "allOf", "items", "properties")
  !any(keys %in% names(node))
}

# More than this many literal options and the notation abbreviates to a
# handful + "..." instead of spelling all of them out (ColorScheme alone
# has 51) -- the property's own description (already part of the same
# @param line) and mosaic's own docs cover the rest.
ENUM_INLINE_LIMIT <- 10

# The notation token(s) for a schema property node -- recurses the same
# const/enum/$ref/anyOf/oneOf/allOf shape collect_enum_values() does, but
# returns a vocabulary meant to be *read*, not validated against. A
# handful of named schema types get a fixed, hand-picked token set instead
# of being recursed into (ChannelValue, ChannelValueSpec, ParamRef, Fixed,
# Interval) -- either because their real shape is far more detail than is
# useful inline (ChannelValueSpec's full object form), or because the
# R-facing spelling differs from the JSON one (ParamRef -> `param()`, the
# actual vgplotr constructor, not the `{type: "string"}` it serializes
# to).
type_tokens <- function(node, defs, depth = 0) {
  if (depth > 8 || is.null(node)) return(character())
  if (is_empty_schema(node)) return("any")
  if (!is.null(node$`$ref`)) {
    rn <- ref_name(node)
    if (rn == "ParamRef") return("param()")
    if (rn == "ChannelValue") return(c("column", "literal", "transform()", "sql()/agg()"))
    if (rn %in% c("ChannelValueSpec", "ChannelValueIntervalSpec")) {
      return(c("column", "literal", "transform()", "sql()/agg()", "list(value=, ...)"))
    }
    if (rn == "Fixed") return('"Fixed"')
    if (rn %in% c("Interval", "LiteralTimeInterval")) return(c('"day"/"week"/"month"/...', "number"))
    vals <- unique(collect_enum_tokens(defs[[rn]], defs))
    if (length(vals)) {
      return(if (length(vals) > ENUM_INLINE_LIMIT) c(vals[seq_len(ENUM_INLINE_LIMIT)], "...") else vals)
    }
    return(type_tokens(defs[[rn]], defs, depth + 1))
  }
  if (!is.null(node$const)) return(literal_token(node$const))
  if (!is.null(node$enum)) {
    vals <- unique(vapply(node$enum, literal_token, character(1)))
    return(if (length(vals) > ENUM_INLINE_LIMIT) c(vals[seq_len(ENUM_INLINE_LIMIT)], "...") else vals)
  }
  for (key in c("anyOf", "oneOf", "allOf")) {
    if (!is.null(node[[key]])) {
      return(unique(unlist(lapply(node[[key]], type_tokens, defs = defs, depth = depth + 1))))
    }
  }
  ty <- node$type
  if (is.list(ty)) ty <- unlist(ty)
  if (length(ty) > 1) {
    return(unique(unlist(lapply(ty, function(t) type_tokens(list(type = t), defs, depth + 1)))))
  }
  switch(or_else(ty, "unknown"),
    number = "number",
    integer = "number",
    string = "string",
    boolean = "boolean",
    "null" = "NULL",
    array = {
      items <- node$items
      if (is.null(items) || is.null(items$type)) "vector"
      else if (identical(items$type, "number")) "numeric vector"
      else if (identical(items$type, "string")) "character vector"
      else "vector"
    },
    object = "list",
    "unknown"
  )
}

# "`<option1 | option2 | ...>`" for one schema property node -- "" if the
# node contributes no tokens at all (shouldn't happen for a real
# property, but a defensive fallback rather than an empty `<>` in the
# docs). Backtick-wrapped (-> \verb{} in the generated .Rd, not raw text)
# deliberately: roxygen2's markdown parser treats a *single bare word*
# between angle brackets (no space/pipe inside, e.g. the unadorned
# "<string>" a plain string-only property produces) as a raw HTML tag and
# silently drops it from plain-text help (`?vg_mark_dot` at the console)
# -- it only survives in HTML output, wrapped in \if{html}{\out{...}}.
# Confirmed directly: every multi-token notation ("<number | param()>")
# rendered fine either way, but a bare one vanished in Rd2txt() output.
# \verb{} renders identically (and correctly) in both text and HTML.
type_notation <- function(node, defs) {
  toks <- unique(type_tokens(node, defs))
  if (!length(toks)) return("")
  paste0("`<", paste(toks, collapse = " | "), ">`")
}

# First-seen type notation for a given property name -- same "global by
# name, not per-mark" reasoning as property_docs()/property_enums() below.
property_types <- function(type_defs, defs) {
  types <- list()
  for (nm in names(type_defs)) {
    props <- type_defs[[nm]]$properties
    for (p in names(props)) {
      if (is.null(types[[p]])) types[[p]] <- type_notation(props[[p]], defs)
    }
  }
  types
}
mark_prop_types <- property_types(mark_defs, defs)
interactor_prop_types <- property_types(interactor_defs, defs)

# `<...>` value-type notation (see ?vg_value_types) for each of
# legend_props above -- used by vg_legend()'s `@eval`'d roxygen
# (legend_options_doc(), R/legend.R) so its per-option documentation stays
# schema-derived instead of a hand-copied snapshot that drifts, like every
# other constructor family's argument notation.
legend_prop_types <- vapply(
  legend_props,
  function(nm) type_notation(defs$PlotLegend$properties[[nm]], defs),
  character(1)
)
names(legend_prop_types) <- legend_props

# Every string literal a property schema node accepts, found by walking
# enum/const/$ref/anyOf/oneOf/allOf recursively -- covers both of mosaic's
# two enum encodings (a real `enum: [...]` array, e.g. CurveName, and an
# anyOf of `{const: "x"}` branches, e.g. textAnchor's start/middle/end).
# Deliberately does NOT recurse into `properties`/`items` -- those describe
# a nested object/array's own shape, not another alternative value for
# *this* property -- so an open-ended ChannelValueSpec-style property (a
# column reference, sql()/agg(), etc.) simply contributes no literals here
# instead of pulling in unrelated schema fragments. A ParamRef branch
# (`{type: "string"}`, no enum/const) also naturally contributes nothing,
# so it doesn't need special-casing either.
collect_enum_values <- function(node, defs, seen = character(), depth = 0) {
  if (depth > 8 || is.null(node)) return(character())
  vals <- character()
  if (is.character(node$const)) vals <- c(vals, node$const)
  if (!is.null(node$enum)) {
    ev <- unlist(node$enum)
    if (is.character(ev)) vals <- c(vals, ev)
  }
  if (!is.null(node$`$ref`)) {
    rn <- ref_name(node)
    if (!(rn %in% seen)) vals <- c(vals, collect_enum_values(defs[[rn]], defs, c(seen, rn), depth + 1))
  }
  for (key in c("anyOf", "oneOf", "allOf")) {
    for (b in node[[key]]) vals <- c(vals, collect_enum_values(b, defs, seen, depth + 1))
  }
  vals
}

# Enums are global by property name, not per-mark (confirmed empirically:
# curve/frameAnchor/interpolate/etc. resolve to the identical shared
# definition everywhere they appear) -- so this unions every mark/
# interactor's allowed values into one lookup per property name. Slightly
# more permissive than strictly correct in the few cases where one mark
# allows an extra literal a sibling mark doesn't (e.g. curve: "auto" on
# Line/Density* but not Area* marks), which matches the "warn, don't
# over-restrict" philosophy call-time validation is for.
property_enums <- function(type_defs, defs) {
  out <- list()
  for (nm in names(type_defs)) {
    props <- type_defs[[nm]]$properties
    for (p in names(props)) {
      vals <- unique(collect_enum_values(props[[p]], defs))
      if (length(vals)) out[[p]] <- union(or_else(out[[p]], character()), vals)
    }
  }
  out
}
enum_props <- property_enums(c(mark_defs, interactor_defs), defs)

# Builds one vg_<name>() function + its roxygen block. `helper` is the
# shared runtime function (vg_mark_()/vg_interactor_()) that drops any
# still-`vg_unset` argument before dispatching to the generic constructor.
# `required` properties (see mark_info()) get no default at all -- a real,
# required R argument that errors immediately if omitted -- and are moved
# to the front of the formals (right after `spec`) since they're the ones
# a caller must supply.
#
# Each property's R-facing formal name is its snake_case translation
# (`strokeWidth` -> `stroke_width`), but it's still forwarded to `helper`
# under its *exact* schema name (`strokeWidth = stroke_width`) -- that's
# the key mosaic's JSON spec actually needs, and the only thing
# `vg_mark()`/`vg_interactor()` (and ultimately `as_spec_payload()`) ever
# see. `camel_to_snake()` is confirmed collision-free across every
# property name in the schema (see design/design-reflections.qmd question
# 2c) -- if a future schema version ever introduces a colliding pair, the
# `stopifnot()` below catches it.
#' The `formula` shorthand (design/vg_formula.qmd) is mark-only, so it's
#' injected here as its own dedicated 2nd formal (right after `spec`, before
#' the schema-derived props) rather than reusing `extra_formals` -- that
#' mechanism appends *after* `...` (data_from/filter_by/data_optimize),
#' which wouldn't let `formula` be used positionally the way
#' `vg_mark_dot(iris, Sepal.Length ~ Sepal.Width, ...)` needs.
.vg_formula_doc <- paste(
  "A shorthand for this mark's position channels (`x`, `y`, `fx`, `fy`, and",
  "paired `x1`/`x2` or `y1`/`y2`), e.g.,",
  "`Sepal.Length ~ Sepal.Width | ~ Species` for",
  "`y = ~Sepal.Length, x = ~Sepal.Width, fx = ~Species`. See [vg_mark()]",
  "for the full grammar."
)

generate_wrapper <- function(fn, helper, type_arg, properties, prop_docs, prop_types, extra_formals = character(),
                              extra_docs = character(), title, spec_doc, family, required = character(),
                              formula_arg = FALSE) {
  props <- names(properties)
  props <- c(intersect(required, props), setdiff(props, required))
  snake_props <- camel_to_snake(props)
  has_spec <- !is.null(spec_doc)
  formals_str <- paste(c(
    if (has_spec) "spec = NULL",
    if (formula_arg) "formula = vg_unset",
    ifelse(props %in% required, snake_props, paste0(snake_props, " = vg_unset")),
    "...",
    if (length(extra_formals)) paste0(extra_formals, " = vg_unset")
  ), collapse = ", ")
  call_args <- paste(c(
    if (has_spec) "spec" else "NULL",
    sprintf('"%s"', type_arg),
    if (formula_arg) "formula = formula",
    paste0(props, " = ", snake_props),
    "...",
    if (length(extra_formals)) paste0(extra_formals, " = ", extra_formals)
  ), collapse = ", ")

  c(
    paste0("#' ", title),
    "#'",
    if (length(props)) "#' See [vg_value_types] for what the `<...>` notation below (`column`, `param()`, ...) means.",
    "#'",
    if (!is.null(spec_doc)) sprintf("#' @param spec %s", spec_doc),
    if (formula_arg) sprintf("#' @param formula %s", .vg_formula_doc),
    sprintf("#' @param %s %s %s", snake_props, unlist(prop_types[props]), unlist(prop_docs[props])),
    "#' @param ... Additional options or plot-level attributes.",
    extra_docs,
    sprintf("#' @family %s", family),
    "#' @export",
    sprintf("%s <- function(%s) {", fn, formals_str),
    sprintf("  %s(%s)", helper, call_args),
    "}",
    ""
  )
}

# --- R/marks-generated.R -------------------------------------------------

mark_lines <- c(
  "# Generated by data-raw/update-schema.R from mosaic's JSON schema.",
  "# DO NOT EDIT BY HAND -- rerun that script instead.",
  "#",
  "# One vg_mark_<mark>() wrapper per mark type in the mosaic-spec schema,",
  "# each with a real named argument per property that mark accepts (so tab",
  "# completion and ?vg_mark_dot show the actual options) -- e.g.,",
  "# vg_mark_dot(spec, x = ~a, y = ~b) is vg_mark(spec, \"dot\", x = ~a, y =",
  "# ~b) with x/y (and every other dot property) as real, documented",
  "# arguments instead of an opaque `...`. Every mark constructor starts",
  "# with vg_mark_, so they can all be found by listing functions with that",
  "# prefix -- this also keeps a mark name from ever colliding with an",
  "# unrelated vg_<name>() function elsewhere in the package.",
  ""
)
for (name in sort(names(mark_defs))) {
  fn <- paste0("vg_mark_", camel_to_snake(name))
  mark_lines <- c(mark_lines, generate_wrapper(
    fn = fn,
    helper = "vg_mark_",
    type_arg = name,
    properties = mark_defs[[name]]$properties,
    prop_docs = mark_prop_docs,
    prop_types = mark_prop_types,
    extra_formals = c("data_from", "filter_by", "data_optimize"),
    extra_docs = c(
      "#' @param data_from The name of the data source this mark reads from (see [vg_data()]); a length-1 nonzero R integer (`1L`, `-1L`, ...; note the `L`) giving a 1-based index into the spec's registered data sources instead, negative counting from the end (e.g., `-1L` for the most recently registered one); or a literal vector of values to use as inline data directly (mosaic-spec's `\"data\": [...]` shorthand, e.g., for a single reference line -- a bare double like `0`/`c(0)`, or `0L` itself (never a valid index), means this, not an index). Left unset, defaults to the first registered data source (equivalent to `data_from = 1L`) for any mark type that takes data at all.",
      "#' @param filter_by A Param/Selection (e.g., from [param()]) to filter this mark's data by.",
      "#' @param data_optimize A flag (default `TRUE`) to enable mark-specific query optimizations for this mark's data; set `FALSE` to disable them (mosaic-spec's `data: {optimize: false}`)."
    ),
    title = docline(mark_defs[[name]]$description, paste0("The `", name, "` mark.")),
    spec_doc = "A plot fragment or `vgspec` to add this mark to, or `NULL` to start a new plot with just this mark.",
    family = "mark functions",
    required = mark_defs[[name]]$required,
    formula_arg = TRUE
  ))
}
writeLines(mark_lines, "R/marks-generated.R")

# --- R/interactors-generated.R --------------------------------------------

inter_lines <- c(
  "# Generated by data-raw/update-schema.R from mosaic's JSON schema.",
  "# DO NOT EDIT BY HAND -- rerun that script instead.",
  "",
  "# Which vg_interactor()/vg_input() types are embedded inside a plot's",
  "# mark list vs. which live in the surrounding layout as standalone",
  "# widgets -- see vg_interactor_placement() in R/interactor.R.",
  ".vg_interactor_types <- c(",
  paste0('  "', interactor_types, '"', collapse = ",\n"),
  ")",
  "",
  ".vg_input_types <- c(",
  paste0('  "', input_types, '"', collapse = ",\n"),
  ")",
  "",
  "# One vg_<type>() wrapper per interactor type (embedded in a plot, e.g.,",
  "# vg_pan_zoom()) and input type (a standalone layout widget, e.g.,",
  "# vg_menu()) in the mosaic-spec schema, each with a real named argument",
  "# per option that type accepts -- e.g., vg_toggle(spec, as = param(sel))",
  "# is vg_interactor(spec, \"toggle\", as = param(sel)) with `as` (and every",
  "# other toggle option) as a real, documented argument.",
  ""
)
for (type in interactor_types) {
  fn <- paste0("vg_", camel_to_snake(type))
  inter_lines <- c(inter_lines, generate_wrapper(
    fn = fn,
    helper = "vg_interactor_",
    type_arg = type,
    properties = interactor_defs[[type]]$properties,
    prop_docs = interactor_prop_docs,
    prop_types = interactor_prop_types,
    title = docline(interactor_defs[[type]]$description, paste0("A `", type, "` interactor.")),
    spec_doc = "A plot fragment or `vgspec` to add this interactor to, or `NULL` to start a new plot with just this interactor.",
    family = "interactor functions"
  ))
}
for (type in input_types) {
  fn <- paste0("vg_", camel_to_snake(type))
  inter_lines <- c(inter_lines, generate_wrapper(
    fn = fn,
    helper = "vg_interactor_",
    type_arg = type,
    properties = interactor_defs[[type]]$properties,
    prop_docs = interactor_prop_docs,
    prop_types = interactor_prop_types,
    title = docline(interactor_defs[[type]]$description, paste0("A `", type, "` input.")),
    spec_doc = NULL,
    family = "interactor functions"
  ))
}
writeLines(inter_lines, "R/interactors-generated.R")

# --- R/attrs-generated.R ---------------------------------------------------

# Formats a named list of character vectors, e.g., list(dot = c("r", "x"),
# lineY = c("curve", "x", "y")), as R source -- one entry per line, list
# names double-quoted (never assumed to be syntactic R names).
format_named_char_list <- function(named_list) {
  entries <- vapply(names(named_list), function(nm) {
    vals <- named_list[[nm]]
    vals_str <- if (length(vals) == 0) "character()" else paste0('"', vals, '"', collapse = ", ")
    sprintf('  "%s" = c(%s)', nm, vals_str)
  }, character(1))
  paste0(entries, collapse = ",\n")
}

attr_lines <- c(
  "# Generated by data-raw/update-schema.R from mosaic's JSON schema.",
  "# DO NOT EDIT BY HAND -- rerun that script instead.",
  "",
  "# All plot-level attribute names (mosaic-spec's PlotAttributes), used by",
  "# split_plot_args() in R/utils.R to tell a plot-level attribute (width,",
  "# xDomain, colorScheme, ...) apart from a mark/interactor's own",
  "# encodings/options when both arrive together via one `...`.",
  ".vg_plot_attrs <- c(",
  paste0('  "', plot_attrs, '"', collapse = ",\n"),
  ")",
  "",
  "# Per-mark-type/interactor-type property names, used by split_plot_args()",
  "# to protect a mark/interactor's own property (e.g., RectY's own `inset`)",
  "# from being mistaken for a same-named PlotAttributes property (a",
  "# plot-wide `inset` default) and bubbled up to the enclosing plot instead",
  "# of staying on the mark/interactor that actually declared it.",
  ".vg_mark_own_props <- list(",
  format_named_char_list(mark_own_props),
  ")",
  "",
  ".vg_interactor_own_props <- list(",
  format_named_char_list(interactor_own_props),
  ")",
  "",
  "# Every option a legend (vg_legend()/vg_legend_color()/etc.) accepts --",
  "# the same set for all three legend types. Used by",
  "# warn_unrecognized_legend_args() (R/utils.R).",
  ".vg_legend_props <- c(",
  paste0('  "', legend_props, '"', collapse = ",\n"),
  ")",
  "",
  "# camelCase legend-property name -> `<...>` value-type notation, for the",
  "# same properties as .vg_legend_props above. Used by legend_options_doc()",
  "# (R/legend.R) to build vg_legend()'s `@eval`'d @param ... documentation.",
  ".vg_legend_prop_types <- c(",
  paste0("  ", names(legend_prop_types), " = ", vapply(legend_prop_types, deparse, character(1)), collapse = ",\n"),
  ")",
  "",
  "# snake_case -> exact camelCase mosaic-spec key, for the same properties",
  "# as .vg_legend_props above (e.g. tick_size -> tickSize) -- the legend",
  "# equivalent of .vg_plot_attrs_snake below. vg_legend()/vg_legend_color()/",
  "# etc. accept snake_case (matching marks/interactors) or the exact",
  "# camelCase key; canonicalize_legend_prop_names() (R/utils.R) does the",
  "# translation.",
  ".vg_legend_props_snake <- c(",
  paste0('  ', camel_to_snake(legend_props), ' = "', legend_props, '"', collapse = ",\n"),
  ")",
  "",
  "# snake_case -> exact camelCase mosaic-spec key for every plot attribute,",
  "# e.g., x_domain -> xDomain. vg_plot()/vg_plot_defaults()/vg_attributes()/",
  "# vg_create() (and the mark/interactor \"attribute riding along\" path in",
  "# split_plot_args()) accept the snake_case form -- matching the rest of",
  "# the package -- and translate it to this exact key before storing or",
  "# checking it against vg_plot_level_args(); an already-camelCase or",
  "# unrecognized name simply doesn't match any entry here and passes",
  "# through untouched (canonicalize_plot_attr_names(), R/utils.R).",
  ".vg_plot_attrs_snake <- c(",
  paste0('  ', camel_to_snake(plot_attrs), ' = "', plot_attrs, '"', collapse = ",\n"),
  ")",
  "",
  "# Which mark types have a `data` property at all -- a handful of purely",
  "# decorative marks (frame, sphere, hexgrid, graticule, the axis/grid",
  "# marks) compute their own geometry and never take a backing table.",
  "# vg_mark() (R/mark.R) uses this to know which marks it's safe to",
  "# default `data_from` on (to the spec's first registered data source)",
  "# when the caller didn't supply one.",
  ".vg_mark_has_data <- c(",
  paste0('  ', names(mark_has_data), ' = ', ifelse(mark_has_data, "TRUE", "FALSE"), collapse = ",\n"),
  ")",
  "",
  "# Every recognized string literal for a given mark/interactor property",
  "# name (e.g. curve -> \"basis\", \"bundle\", ..., \"step-before\"), unioned",
  "# across every mark/interactor that declares it (enums are global by",
  "# property name in mosaic's schema, not truly per-mark -- see",
  "# data-raw/update-schema.R). Used by warn_unrecognized_enum_values()",
  "# (R/utils.R) to catch a likely-mistyped literal value at call time.",
  ".vg_enum_props <- list(",
  format_named_char_list(enum_props),
  ")"
)
writeLines(attr_lines, "R/attrs-generated.R")

# --- R/scale-generated.R & R/guide-generated.R ----------------------------
#
# mosaic's PlotAttributes (`.vg_plot_attrs` above) has no schema-level
# structure separating scale properties from guide (axis) properties, or
# grouping them by channel (x/y/fx/fy/color/opacity/r/length/symbol) -- it's
# one flat 215-property object. This classifies purely by name: every
# property is <prefix><Suffix> (or, for the handful of global defaults,
# just <suffix> with an empty prefix), and each Suffix is looked up in
# exactly one of the two canonical tables below to decide (a) whether it's
# a scale or guide property and (b) its snake_case argument name. These
# tables are the single source of truth for every vg_scale_*()/
# vg_guide_*() constructor -- add a new suffix here (never in a
# per-channel copy) if a future mosaic version adds one.
#
# `projection*` and `facet{Grid,Label,Margin*}` don't fit this per-channel
# pattern and are left as raw vg_plot()/vg_attributes() calls, same as
# always (see design/completing-the-package.qmd's "Projection and
# Geographic Scales" section, not yet implemented).

plot_attr_props <- defs$PlotAttributes$properties
plot_attr_docs <- vapply(
  names(plot_attr_props),
  function(nm) docline(plot_attr_props[[nm]]$description, nm),
  character(1)
)
names(plot_attr_docs) <- names(plot_attr_props)
plot_attr_types <- vapply(
  names(plot_attr_props),
  function(nm) type_notation(plot_attr_props[[nm]], defs),
  character(1)
)
names(plot_attr_types) <- names(plot_attr_props)

# Deliberate semantic renames where a literal snake_case translation of the
# suffix would be confusing: `Scale` -> `type` (not `scale`, an argument to
# a function already named vg_scale_*()) and `Axis` -> `position` (not
# `axis`, when "which axis" is the whole point of vg_guide_*()). See
# design/design-reflections.qmd question 2b.
suffix_arg_overrides <- c(Scale = "type", Axis = "position")
suffix_arg_name <- function(suffix) {
  override <- unname(suffix_arg_overrides[suffix])
  ifelse(is.na(override), camel_to_snake(suffix), override)
}

# Canonical suffix order -- also the order arguments appear in every
# generated vg_scale_*()/vg_guide_*() signature (only the suffixes a given
# channel actually has are included).
scale_suffixes <- c(
  "Scale", "Domain", "Range", "Scheme", "Interpolate", "Pivot", "Symmetric",
  "Nice", "Zero", "Reverse", "Clamp", "Round",
  "Padding", "PaddingInner", "PaddingOuter", "Align",
  "Inset", "InsetLeft", "InsetRight", "InsetTop", "InsetBottom",
  "Base", "Exponent", "Constant", "Percent", "N"
)
guide_suffixes <- c(
  "Axis", "Ticks", "TickSpacing", "TickSize", "TickPadding", "TickFormat",
  "TickRotate", "Grid", "Line", "Label", "LabelAnchor", "LabelOffset",
  "LabelArrow", "FontVariant", "AriaLabel", "AriaDescription"
)
inset_suffixes <- c("InsetLeft", "InsetRight", "InsetTop", "InsetBottom")

# `prefix` is "" for the bare global-default channel (e.g., "align", not
# "xAlign").
plot_attr_name <- function(prefix, suffix) {
  if (nchar(prefix) == 0) paste0(tolower(substr(suffix, 1, 1)), substring(suffix, 2)) else paste0(prefix, suffix)
}
has_plot_attr <- function(prefix, suffix) plot_attr_name(prefix, suffix) %in% names(plot_attr_props)

# The suffixes (in canonical order) that actually exist for `prefix`,
# restricted to `suffixes` (scale_suffixes or guide_suffixes).
channel_suffixes <- function(prefix, suffixes) Filter(function(s) has_plot_attr(prefix, s), suffixes)

attr_names_doc <- function(which_values, suffix) {
  paste(sprintf("`%s`", vapply(which_values, plot_attr_name, character(1), suffix = suffix)), collapse = "/")
}

# Builds one generated vg_scale_*()/vg_guide_*() function (plus its roxygen
# block and, for a multi-prefix `which_values`, its wrapper_function()-built
# aliases -- e.g., vg_scale_x()/vg_scale_y() for vg_scale_position()).
# `which_values` is a single prefix (e.g., "color", or "" for the bare
# global-default channel) for a standalone function with no `which`
# argument, or two prefixes sharing one generic function (e.g., c("x", "y"))
# with `which` selecting between them. `has_inset` enables the
# position/facet-scale-only left/right-vs-top/bottom inset handling (see
# add_inset_attrs(), R/utils.R) -- never needed for guide functions, or for
# the color/opacity/r/length/symbol channels (none of which have any Inset
# property).
generate_scale_guide <- function(fn, which_values, suffixes, has_inset, family, family_prefix,
                                  which_noun = NULL, spec_noun, title, description, examples,
                                  alias = NULL) {
  has_which <- length(which_values) > 1
  common <- setdiff(channel_suffixes(which_values[1], suffixes), if (has_inset) inset_suffixes else character())
  args <- vapply(common, suffix_arg_name, character(1))

  spec_doc <- c(
    sprintf(
      "#' @param spec A plot fragment or `vgspec` to set this%s on, or `NULL` to",
      if (nzchar(spec_noun)) paste0(" ", spec_noun) else ""
    ),
    "#'   start a new plot fragment with just these attributes."
  )

  which_doc <- if (has_which) {
    sprintf(
      "#' @param which Which %s this sets: %s.", which_noun,
      paste(sprintf('`"%s"`', which_values), collapse = " or ")
    )
  } else {
    character()
  }

  arg_docs <- if (length(common)) {
    sprintf(
      "#' @param %s %s %s (%s).", args,
      plot_attr_types[plot_attr_name(which_values[1], common)],
      plot_attr_docs[plot_attr_name(which_values[1], common)],
      vapply(common, function(s) attr_names_doc(which_values, s), character(1))
    )
  } else {
    character()
  }

  inset_formals <- character()
  inset_docs <- character()
  if (has_inset) {
    lr_which <- which_values[vapply(which_values, has_plot_attr, logical(1), suffix = "InsetLeft")]
    tb_which <- which_values[vapply(which_values, has_plot_attr, logical(1), suffix = "InsetTop")]
    inset_formals <- c("inset_left = vg_unset", "inset_right = vg_unset", "inset_top = vg_unset", "inset_bottom = vg_unset")
    inset_docs <- c(
      sprintf(
        "#' @param inset_left,inset_right %s Pixel inset at the left/right end of the range; only meaningful for `which = \"%s\"` (`%s`/`%s`).",
        plot_attr_types[plot_attr_name(lr_which, "InsetLeft")], lr_which, plot_attr_name(lr_which, "InsetLeft"), plot_attr_name(lr_which, "InsetRight")
      ),
      sprintf(
        "#' @param inset_top,inset_bottom %s Pixel inset at the top/bottom end of the range; only meaningful for `which = \"%s\"` (`%s`/`%s`).",
        plot_attr_types[plot_attr_name(tb_which, "InsetTop")], tb_which, plot_attr_name(tb_which, "InsetTop"), plot_attr_name(tb_which, "InsetBottom")
      )
    )
  }

  formals_str <- paste(c(
    "spec = NULL",
    if (has_which) sprintf("which = c(%s)", paste(sprintf('"%s"', which_values), collapse = ", ")),
    paste0(args, " = vg_unset"),
    inset_formals,
    "..."
  ), collapse = ", ")

  # prefixed_attrs() builds the real attr name via plain paste0(prefix,
  # table_value) -- for a real prefix that's the capitalized suffix
  # ("x" + "Align" = "xAlign"), but for the bare global channel (prefix
  # ""), the table value itself must already be lowercase-initial
  # ("" + "align" = "align", not "" + "Align" = "Align").
  table_values <- if (nchar(which_values[1]) == 0) {
    vapply(common, function(s) paste0(tolower(substr(s, 1, 1)), substring(s, 2)), character(1))
  } else {
    common
  }
  suffix_table <- paste0("c(", paste(sprintf('%s = "%s"', args, table_values), collapse = ", "), ")")
  attrs_prefix <- if (has_which) "which" else sprintf('"%s"', which_values[1])
  context_expr <- if (has_which) sprintf('paste0("%s", which, "()")', family_prefix) else sprintf('"%s()"', fn)

  body <- c(
    if (has_which) "  which <- match.arg(which)",
    sprintf("  context <- %s", context_expr),
    sprintf("  suffixes <- %s", suffix_table),
    sprintf("  attrs <- prefixed_attrs(%s, suffixes, environment())", attrs_prefix),
    if (has_inset) "  attrs <- add_inset_attrs(attrs, which, environment(), context)",
    "  apply_plot_attrs(spec, attrs, list(...), context)"
  )

  main <- c(
    paste0("#' ", title),
    "#'",
    description,
    "#'",
    if (length(arg_docs) || length(inset_docs)) c(
      "#' See [vg_value_types] for what the `<...>` notation below (`number`, `param()`, ...) means.",
      "#'"
    ),
    spec_doc,
    which_doc,
    arg_docs,
    inset_docs,
    "#' @param ... Additional plot-level attributes not covered above, snake_case",
    "#'   (e.g., `x_domain =`) -- translated to mosaic's own camelCase key.",
    sprintf("#' @family %s", family),
    "#' @export",
    examples,
    sprintf("%s <- function(%s) {", fn, formals_str),
    body,
    "}",
    ""
  )

  aliases <- character()
  if (has_which) {
    for (w in which_values) {
      other_side <- character()
      for (o in setdiff(which_values, w)) {
        if (has_plot_attr(o, "InsetLeft")) other_side <- c(other_side, "inset_left", "inset_right")
        if (has_plot_attr(o, "InsetTop")) other_side <- c(other_side, "inset_top", "inset_bottom")
      }
      drop_arg <- if (has_inset && length(other_side)) {
        sprintf(", drop = c(%s)", paste(sprintf('"%s"', other_side), collapse = ", "))
      } else {
        ""
      }
      aliases <- c(
        aliases,
        sprintf("#' @rdname %s", fn),
        "#' @export",
        sprintf('%s <- wrapper_function(%s, which = "%s"%s)', paste0(family_prefix, w), fn, w, drop_arg),
        ""
      )
    }
  }
  if (!is.null(alias)) {
    aliases <- c(aliases, sprintf("#' @rdname %s", fn), "#' @export", sprintf("%s <- %s", alias, fn), "")
  }

  c(main, aliases)
}

# `vg_inset_side_suffixes`/`vg_position_counterpart` (referenced by
# add_inset_attrs(), R/utils.R) -- which InsetLeft/Right vs InsetTop/Bottom
# pair applies to which `which` value, derived the same way as everything
# else here rather than hand-maintained.
inset_side_data <- local({
  sides <- list()
  counterpart <- c()
  for (grp in list(c("x", "y"), c("fx", "fy"))) {
    a <- grp[1]
    b <- grp[2]
    if (has_plot_attr(a, "InsetLeft")) {
      sides[[a]] <- c(inset_left = "InsetLeft", inset_right = "InsetRight")
      sides[[b]] <- c(inset_top = "InsetTop", inset_bottom = "InsetBottom")
    } else {
      sides[[a]] <- c(inset_top = "InsetTop", inset_bottom = "InsetBottom")
      sides[[b]] <- c(inset_left = "InsetLeft", inset_right = "InsetRight")
    }
    counterpart[a] <- b
    counterpart[b] <- a
  }
  list(sides = sides, counterpart = counterpart)
})

side_list_literal <- function(side) paste0("c(", paste(sprintf('%s = "%s"', names(side), side), collapse = ", "), ")")

scale_lines <- c(
  "# Generated by data-raw/update-schema.R from mosaic's JSON schema.",
  "# DO NOT EDIT BY HAND -- rerun that script instead.",
  "#",
  "# One vg_scale_*() constructor per scale channel/family in mosaic-spec's",
  "# PlotAttributes -- see data-raw/update-schema.R's own comments for how",
  "# properties are classified into these.",
  "",
  "#' @include utils.R",
  "NULL",
  "",
  "# Which InsetLeft/InsetRight vs InsetTop/InsetBottom pair applies to which",
  "# `which` value -- used by add_inset_attrs() (R/utils.R).",
  "vg_inset_side_suffixes <- list(",
  paste0("  ", paste(sprintf("%s = %s", names(inset_side_data$sides), vapply(inset_side_data$sides, side_list_literal, character(1))), collapse = ",\n  ")),
  ")",
  "",
  sprintf(
    "vg_position_counterpart <- c(%s)",
    paste(sprintf('%s = "%s"', names(inset_side_data$counterpart), inset_side_data$counterpart), collapse = ", ")
  ),
  ""
)

guide_lines <- c(
  "# Generated by data-raw/update-schema.R from mosaic's JSON schema.",
  "# DO NOT EDIT BY HAND -- rerun that script instead.",
  "#",
  "# One vg_guide_*() constructor per axis-guide channel/family in",
  "# mosaic-spec's PlotAttributes -- see data-raw/update-schema.R's own",
  "# comments for how properties are classified into these.",
  "",
  "#' @include utils.R",
  "NULL",
  ""
)

scale_group <- function(fn, which_values, has_inset, which_noun = NULL, title, description, examples, alias = NULL) {
  generate_scale_guide(
    fn, which_values, scale_suffixes, has_inset = has_inset, family = "scale functions",
    family_prefix = "vg_scale_", which_noun = which_noun, spec_noun = "scale",
    title = title, description = description, examples = examples, alias = alias
  )
}
guide_group <- function(fn, which_values, which_noun = NULL, title, description, examples, alias = NULL) {
  generate_scale_guide(
    fn, which_values, guide_suffixes, has_inset = FALSE, family = "guide functions",
    family_prefix = "vg_guide_", which_noun = which_noun, spec_noun = "guide",
    title = title, description = description, examples = examples, alias = alias
  )
}

scale_lines <- c(scale_lines, scale_group(
  "vg_scale_position", c("x", "y"), has_inset = TRUE, which_noun = "position scale",
  title = "Set position scale properties (x or y)",
  description = c(
    "#' `vg_scale_x()`/`vg_scale_y()` set the scale properties mosaic-spec",
    "#' exposes per positional axis (`xScale`, `xDomain`, ... -- substitute",
    "#' `y` for the vertical axis). These are already plot-level attributes",
    "#' that [vg_plot()]/[vg_attributes()] accept directly, snake_case; this",
    "#' is a discoverable, per-channel convenience layer on top of that.",
    "#' `vg_scale_x()` and `vg_scale_y()` are thin wrappers around the",
    "#' generic `vg_scale_position()`.",
    "#'",
    "#' Like [vg_plot()], this can be piped in alongside marks/interactors --",
    "#' it only ever sets attributes on the current plot fragment, so it",
    "#' never needs to come last in a chain. For the analogous facet scales",
    "#' (`fx`/`fy`), see [vg_scale_facet()]."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b) |>",
    "#'   vg_scale_x(type = \"log\") |>",
    "#'   vg_scale_y(zero = TRUE, nice = TRUE)"
  )
))

scale_lines <- c(scale_lines, scale_group(
  "vg_scale_facet", c("fx", "fy"), has_inset = TRUE, which_noun = "facet scale",
  title = "Set facet scale properties (fx or fy)",
  description = c(
    "#' `vg_scale_fx()`/`vg_scale_fy()` set the scale properties mosaic-spec",
    "#' exposes per facet axis (`fxDomain`, `fxPadding`, ... -- substitute",
    "#' `fy` for the row facet axis). Facet scales are always band scales,",
    "#' so this covers fewer properties than [vg_scale_position()] (no",
    "#' `type`, `nice`, `zero`, `clamp`, or the log/pow/symlog-only",
    "#' properties) -- exactly which ones is derived from the schema, not",
    "#' hand-picked. `vg_scale_fx()` and `vg_scale_fy()` are thin wrappers",
    "#' around the generic `vg_scale_facet()`."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, fx = ~g) |>",
    "#'   vg_scale_fx(padding = 0.1)"
  )
))

scale_lines <- c(scale_lines, scale_group(
  "vg_scale_color", "color", has_inset = FALSE,
  title = "Set the color scale's properties",
  description = c(
    "#' `vg_scale_color()` sets the scale properties mosaic-spec exposes for",
    "#' the `color` channel (`colorScale`, `colorDomain`, ... -- the scale",
    "#' that `fill`/`stroke` encodings are bound to unless they're a literal",
    "#' constant). Unlike `vg_scale_x()`/`vg_scale_y()`, there's only one",
    "#' color channel, so `vg_scale_color()` isn't built from a `which =`",
    "#' generic -- it's the whole implementation. For the axis-guide",
    "#' properties (`colorLabel`/`colorTickFormat`), see [vg_guide_color()];",
    "#' for an actual rendered color legend, see [vg_legend_color()] -- a",
    "#' different (if related) thing, a standalone/embedded legend mark",
    "#' rather than a plot attribute."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, fill = ~g) |>",
    "#'   vg_scale_color(scheme = \"Viridis\", type = \"linear\")"
  )
))

scale_lines <- c(scale_lines, scale_group(
  "vg_scale_opacity", "opacity", has_inset = FALSE,
  title = "Set the opacity scale's properties",
  description = c(
    "#' `vg_scale_opacity()` sets the scale properties mosaic-spec exposes",
    "#' for the `opacity` channel (`opacityScale`, `opacityDomain`, ... --",
    "#' the scale that `opacity`/`fillOpacity`/`strokeOpacity` encodings are",
    "#' bound to unless they're a literal constant). For the axis-guide",
    "#' properties, see [vg_guide_opacity()]; for an actual rendered opacity",
    "#' legend, see [vg_legend_opacity()]."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, opacity = ~g) |>",
    "#'   vg_scale_opacity(range = c(0.2, 1))"
  )
))

scale_lines <- c(scale_lines, scale_group(
  "vg_scale_r", "r", has_inset = FALSE, alias = "vg_scale_radius",
  title = "Set the radius scale's properties",
  description = c(
    "#' `vg_scale_r()` (aliased as `vg_scale_radius()`) sets the scale",
    "#' properties mosaic-spec exposes for the `r` channel (`rScale`,",
    "#' `rDomain`, ... -- the scale that a `dot`/`circle` mark's `r`",
    "#' encoding is bound to unless it's a literal constant). Which",
    "#' properties exist here (e.g., no `reverse` -- mosaic doesn't define",
    "#' `rReverse`) is derived from the schema, not hand-picked. For the",
    "#' axis-guide property, see [vg_guide_r()]/[vg_guide_radius()]; for an",
    "#' actual rendered radius/size legend, see [vg_legend_symbol()]",
    "#' (mosaic doesn't have a dedicated `r`-typed legend)."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, r = ~g) |>",
    "#'   vg_scale_r(range = c(0, 20), zero = TRUE)"
  )
))

scale_lines <- c(scale_lines, scale_group(
  "vg_scale_length", "length", has_inset = FALSE,
  title = "Set the length scale's properties",
  description = c(
    "#' `vg_scale_length()` sets the scale properties mosaic-spec exposes",
    "#' for the `length` channel (`lengthScale`, `lengthDomain`, ... -- the",
    "#' scale that a `vector`/`spike` mark's `length` encoding is bound to",
    "#' unless it's a literal constant). Mosaic doesn't define a length",
    "#' axis-guide or legend, so there's no `vg_guide_length()`/",
    "#' `vg_legend_length()` to pair with this."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_vector(x = ~a, y = ~b, length = ~g) |>",
    "#'   vg_scale_length(range = c(0, 20))"
  )
))

scale_lines <- c(scale_lines, scale_group(
  "vg_scale_symbol", "symbol", has_inset = FALSE,
  title = "Set the symbol scale's properties",
  description = c(
    "#' `vg_scale_symbol()` sets the scale properties mosaic-spec exposes",
    "#' for the `symbol` channel (`symbolScale`, `symbolDomain`,",
    "#' `symbolRange` -- the scale that a `dot`'s `symbol` encoding is bound",
    "#' to unless it's a literal constant). Mosaic doesn't define a symbol",
    "#' axis-guide, but does have a dedicated legend type -- see",
    "#' [vg_legend_symbol()]."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, symbol = ~g) |>",
    "#'   vg_scale_symbol(range = c(\"circle\", \"square\", \"triangle\"))"
  )
))

scale_lines <- c(scale_lines, scale_group(
  "vg_scale_all", "", has_inset = FALSE,
  title = "Set global default scale properties (all ordinal position scales)",
  description = c(
    "#' `vg_scale_all()` sets mosaic-spec's plot-wide fallback defaults --",
    "#' unlike `xAlign`/`xPadding`/etc. (set via [vg_scale_position()]) or",
    "#' `fxAlign`/etc. (via [vg_scale_facet()]), which only affect one scale,",
    "#' these bare attributes are mosaic's own defaults applied to *every*",
    "#' ordinal position scale (`x`, `y`, `fx`, `fy`) that doesn't set its",
    "#' own value. For the analogous axis-guide defaults, see",
    "#' [vg_guide_all()]."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b) |>",
    "#'   vg_scale_all(padding = 0.2)"
  )
))

writeLines(scale_lines, "R/scale-generated.R")

guide_lines <- c(guide_lines, guide_group(
  "vg_guide_position", c("x", "y"), which_noun = "axis",
  title = "Set axis-guide properties for a position scale (x or y)",
  description = c(
    "#' `vg_guide_x()`/`vg_guide_y()` set the axis-guide properties",
    "#' mosaic-spec exposes per positional axis (`xAxis`, `xTicks`, ... --",
    "#' substitute `y` for the vertical axis). Like the `xScale`/`yScale`",
    "#' properties handled by [vg_scale_position()], these are already",
    "#' plot-level attributes that [vg_plot()]/[vg_attributes()] accept",
    "#' directly, snake_case (e.g., `x_ticks =`).",
    "#'",
    "#' These are named `vg_guide_*()` rather than `vg_axis_*()` to avoid",
    "#' colliding with `vg_mark_axis_x()`/`vg_mark_axis_y()`",
    "#' (mosaic-spec's `axisX`/`axisY` *mark*, a drawable,",
    "#' independently-styled axis) -- a different (if related) thing from",
    "#' the plain `xAxis`/`yAxis` guide attributes set here, which appear",
    "#' automatically alongside a plot's marks. Same story for the",
    "#' analogous facet guides, [vg_guide_facet()] (vs.",
    "#' `vg_mark_axis_fx()`/`vg_mark_axis_fy()`, mosaic's `axisFx`/`axisFy`",
    "#' marks)."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b) |>",
    "#'   vg_guide_x(label = \"A\", grid = TRUE) |>",
    "#'   vg_guide_y(label = \"B\", tick_format = \".0f\")"
  )
))

guide_lines <- c(guide_lines, guide_group(
  "vg_guide_facet", c("fx", "fy"), which_noun = "facet axis",
  title = "Set axis-guide properties for a facet scale (fx or fy)",
  description = c(
    "#' `vg_guide_fx()`/`vg_guide_fy()` set the axis-guide properties",
    "#' mosaic-spec exposes per facet axis (`fxAxis`, `fxTicks`, ... --",
    "#' substitute `fy` for the row facet axis). This is the facet",
    "#' counterpart of [vg_guide_position()]; the property set (which lacks",
    "#' `label_arrow`, among others) is derived from the schema, not",
    "#' hand-picked.",
    "#'",
    "#' See [vg_guide_position()] for why these are `vg_guide_*()` rather",
    "#' than `vg_axis_*()` (to avoid colliding with `vg_mark_axis_fx()`/",
    "#' `vg_mark_axis_fy()`, mosaic's `axisFx`/`axisFy` marks)."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, fx = ~g) |>",
    "#'   vg_guide_fx(label = \"Group\")"
  )
))

guide_lines <- c(guide_lines, guide_group(
  "vg_guide_color", "color",
  title = "Set axis-guide properties for the color scale",
  description = c(
    "#' `vg_guide_color()` sets the axis-guide properties mosaic-spec",
    "#' exposes for the `color` channel (`colorLabel`, `colorTickFormat`) --",
    "#' the counterpart of [vg_scale_color()] for the color channel's",
    "#' label/tick formatting rather than its domain/range/palette.",
    "#'",
    "#' This is named `vg_guide_color()` rather than `vg_legend_color()`",
    "#' because `vg_legend_color()` already exists and means something",
    "#' different: it adds an actual rendered color legend (a",
    "#' standalone/embedded legend mark, [vg_legend()]) to the spec.",
    "#' `vg_guide_color()` only sets these plot attributes -- it neither",
    "#' shows nor requires a legend to be present."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, fill = ~g) |>",
    "#'   vg_guide_color(label = \"Group\")"
  )
))

guide_lines <- c(guide_lines, guide_group(
  "vg_guide_opacity", "opacity",
  title = "Set axis-guide properties for the opacity scale",
  description = c(
    "#' `vg_guide_opacity()` sets the axis-guide properties mosaic-spec",
    "#' exposes for the `opacity` channel -- the counterpart of",
    "#' [vg_scale_opacity()] for the opacity channel's label/tick",
    "#' formatting rather than its domain/range.",
    "#'",
    "#' See [vg_guide_color()] for why this is `vg_guide_opacity()` rather",
    "#' than `vg_legend_opacity()` (already taken by [vg_legend()]'s actual",
    "#' rendered legend)."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, opacity = ~g) |>",
    "#'   vg_guide_opacity(label = \"Group\")"
  )
))

guide_lines <- c(guide_lines, guide_group(
  "vg_guide_r", "r", alias = "vg_guide_radius",
  title = "Set the axis-guide property for the radius scale",
  description = c(
    "#' `vg_guide_r()` (aliased as `vg_guide_radius()`) sets the axis-guide",
    "#' propert(y/ies) mosaic-spec exposes for the `r` channel -- the",
    "#' counterpart of [vg_scale_r()]/[vg_scale_radius()] for the radius",
    "#' channel's label rather than its domain/range. Which properties",
    "#' exist here (just a label, no tick-format) is derived from the",
    "#' schema, not hand-picked.",
    "#'",
    "#' See [vg_guide_color()] for why this is `vg_guide_r()`/",
    "#' `vg_guide_radius()` rather than `vg_legend_r()`/`vg_legend_radius()`",
    "#' -- mosaic doesn't have a dedicated `r`-typed legend mark to collide",
    "#' with here (radius/size is usually shown via [vg_legend_symbol()]",
    "#' instead), but the naming stays consistent with [vg_guide_color()]/",
    "#' [vg_guide_opacity()]."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b, r = ~g) |>",
    "#'   vg_guide_r(label = \"Size\")"
  )
))

guide_lines <- c(guide_lines, guide_group(
  "vg_guide_all", "",
  title = "Set global default axis-guide properties (all position axes)",
  description = c(
    "#' `vg_guide_all()` sets mosaic-spec's plot-wide fallback defaults for",
    "#' the axis-guide properties -- unlike `xAxis`/`xGrid`/etc. (set via",
    "#' [vg_guide_position()]) or `fxAxis`/etc. (via [vg_guide_facet()]),",
    "#' which only affect one axis, these bare attributes are mosaic's own",
    "#' defaults applied to *every* position axis (`x`, `y`, `fx`, `fy`)",
    "#' that doesn't set its own value. For the analogous scale defaults,",
    "#' see [vg_scale_all()]."
  ),
  examples = c(
    "#' @examples",
    "#' vg_mark_dot(x = ~a, y = ~b) |>",
    "#'   vg_guide_all(grid = TRUE)"
  )
))

writeLines(guide_lines, "R/guide-generated.R")

cat("Wrote R/marks-generated.R, R/interactors-generated.R, R/attrs-generated.R,\n")
cat("      R/scale-generated.R, R/guide-generated.R\n")
