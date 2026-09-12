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
# Every vg_<mark>()/vg_<type>() wrapper gets a real, named formal argument
# per property the schema defines for it (rather than just `...`), each
# documented from the schema's own description -- so tab completion and
# `?vg_dot` show the actual options, not just "...". The lower-level
# vg_mark()/vg_interactor() they're built on stay `...`-based on purpose,
# since they take an arbitrary/dynamic mark or interactor type.
#
# What this does NOT cover (hand-written, not schema-driven): transforms
# (R/transforms.R -- a different schema file, Transform.ts, without a
# corresponding section in the JSON schema's mark/attribute defs), legends
# (only 3 types, small enough to maintain by hand in R/legend.R), and
# data-source options (R/create.R).

MOSAIC_VERSION <- "0.31.0"

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
  # A literal backslash in the schema text (e.g. "line breaks (\n, \r\n, or
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
# `properties.mark.const` (e.g. densityX/densityY) -- mirrors mosaic's own
# markInfo() in bin/generate-python-api.js. Returns NULL for a non-mark def.
mark_info <- function(def) {
  if (!is.null(def$properties$mark$const)) {
    return(list(mark = def$properties$mark$const, description = def$description, properties = def$properties))
  }
  branches <- or_else(def$anyOf, def$allOf)
  consts <- unique(unlist(lapply(branches, function(b) b$properties$mark$const)))
  if (length(consts) != 1) return(NULL)
  props <- list()
  for (b in branches) props <- utils::modifyList(props, or_else(b$properties, list()))
  list(mark = consts, description = def$description, properties = props)
}

mark_defs <- Filter(Negate(is.null), lapply(defs, mark_info))
names(mark_defs) <- vapply(mark_defs, function(m) m$mark, character(1))
for (nm in names(mark_defs)) {
  mark_defs[[nm]]$properties <- mark_defs[[nm]]$properties[!names(mark_defs[[nm]]$properties) %in% c("mark", "data")]
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

plot_attrs <- sort(names(defs$PlotAttributes$properties))

cat(length(mark_defs), "marks,", length(interactor_types), "interactors,",
    length(input_types), "inputs,", length(plot_attrs), "plot attributes\n")

# vg_mark_()/vg_interactor_() (R/mark.R, R/interactor.R) pass a mark/
# interactor's own properties through to vg_mark()/vg_interactor() using
# their *exact* schema names, so that e.g. Search's own "type" option (its
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

# Builds one vg_<name>() function + its roxygen block. `helper` is the
# shared runtime function (vg_mark_()/vg_interactor_()) that drops any
# still-`vg_unset` argument before dispatching to the generic constructor.
generate_wrapper <- function(fn, helper, type_arg, properties, prop_docs, extra_formals = character(),
                              extra_docs = character(), title, spec_doc, family) {
  props <- names(properties)
  has_spec <- !is.null(spec_doc)
  formals_str <- paste(c(
    if (has_spec) "spec = NULL",
    paste0(props, " = vg_unset"),
    "...",
    if (length(extra_formals)) paste0(extra_formals, " = vg_unset")
  ), collapse = ", ")
  call_args <- paste(c(
    if (has_spec) "spec" else "NULL",
    sprintf('"%s"', type_arg),
    paste0(props, " = ", props),
    "...",
    if (length(extra_formals)) paste0(extra_formals, " = ", extra_formals)
  ), collapse = ", ")

  c(
    paste0("#' ", title),
    "#'",
    if (!is.null(spec_doc)) sprintf("#' @param spec %s", spec_doc),
    sprintf("#' @param %s %s", props, unlist(prop_docs[props])),
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
  "# One vg_<mark>() wrapper per mark type in the mosaic-spec schema, each",
  "# with a real named argument per property that mark accepts (so tab",
  "# completion and ?vg_dot show the actual options) -- e.g. vg_dot(spec, x",
  "# = ~a, y = ~b) is vg_mark(spec, \"dot\", x = ~a, y = ~b) with x/y (and",
  "# every other dot property) as real, documented arguments instead of an",
  "# opaque `...`.",
  ""
)
for (name in sort(names(mark_defs))) {
  fn <- paste0("vg_", camel_to_snake(name))
  mark_lines <- c(mark_lines, generate_wrapper(
    fn = fn,
    helper = "vg_mark_",
    type_arg = name,
    properties = mark_defs[[name]]$properties,
    prop_docs = mark_prop_docs,
    extra_formals = c("data_from", "filter_by"),
    extra_docs = c(
      "#' @param data_from The name of the data source this mark reads from (see [vg_data()]).",
      "#' @param filter_by A Param/Selection (e.g. from [param()]) to filter this mark's data by."
    ),
    title = docline(mark_defs[[name]]$description, paste0("The `", name, "` mark.")),
    spec_doc = "A plot fragment or `vgspec` to add this mark to, or `NULL` to start a new plot with just this mark.",
    family = "vg_marks"
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
  "# One vg_<type>() wrapper per interactor type (embedded in a plot, e.g.",
  "# vg_pan_zoom()) and input type (a standalone layout widget, e.g.",
  "# vg_menu()) in the mosaic-spec schema, each with a real named argument",
  "# per option that type accepts -- e.g. vg_toggle(spec, as = param(sel))",
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
    title = docline(interactor_defs[[type]]$description, paste0("A `", type, "` interactor.")),
    spec_doc = "A plot fragment or `vgspec` to add this interactor to, or `NULL` to start a new plot with just this interactor.",
    family = "vg_interactors"
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
    title = docline(interactor_defs[[type]]$description, paste0("A `", type, "` input.")),
    spec_doc = NULL,
    family = "vg_interactors"
  ))
}
writeLines(inter_lines, "R/interactors-generated.R")

# --- R/attrs-generated.R ---------------------------------------------------

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
  ")"
)
writeLines(attr_lines, "R/attrs-generated.R")

cat("Wrote R/marks-generated.R, R/interactors-generated.R, R/attrs-generated.R\n")
