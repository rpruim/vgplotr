# Validation of inst/extdata/suggestion-synonyms.yaml, used by test-synonyms.R.
# Returns a character vector describing every problem found (empty = fine), so a
# failing test names exactly what to fix in the file.

synonym_table_problems <- function(raw) {
  problems <- character()
  add <- function(...) problems <<- c(problems, sprintf(...))
  norm <- normalize_name
  ns <- asNamespace("vgplotr")

  sections <- c("arguments", "values", "transform_options", "transform_names")
  unknown_sections <- setdiff(names(raw), sections)
  if (length(unknown_sections)) add("unknown top-level section(s): %s", paste(unknown_sections, collapse = ", "))
  for (section in setdiff(sections, names(raw))) add("missing section `%s`", section)

  formals_of <- function(pattern) unlist(lapply(ls(ns, pattern = pattern), function(f) names(formals(get(f, ns)))))
  always_valid <- c(.vg_plot_attrs, names(.vg_plot_attrs_snake), "data_from", "filter_by", "data_optimize")
  universe <- list(
    arguments = unique(c(
      unlist(.vg_mark_own_props), unlist(.vg_interactor_own_props), .vg_legend_props,
      always_valid, formals_of("^vg_(scale|guide)_")
    )),
    transform_options = unique(unlist(lapply(names(vg_transform_specs), function(n) names(formals(get(n, ns)))))),
    transform_names = c(names(vg_transform_specs), "sql", "agg", "param")
  )

  check_entries <- function(where, entries, allowed, dead_keys = character(), exact = FALSE) {
    keys <- names(entries)
    if (length(entries) && (is.null(keys) || any(!nzchar(keys)))) add("%s: an entry has no key", where)
    dup <- unique(norm(keys)[duplicated(norm(keys))])
    for (d in dup) add("%s: `%s` is listed more than once (case, `_`, `.` and `-` are ignored when matching)", where, d)
    for (k in keys[nzchar(keys)]) {
      targets <- unlist(entries[[k]])
      if (!is.character(targets) || length(targets) == 0) {
        add("%s: `%s` has no candidates", where, k)
        next
      }
      if (norm(k) %in% norm(targets)) add("%s: `%s` suggests itself", where, k)
      if (norm(k) %in% norm(dead_keys)) add("%s: `%s` is always valid here, so this entry can never fire", where, k)
      missing <- if (exact) setdiff(targets, allowed) else targets[!(norm(targets) %in% norm(allowed))]
      for (m in missing) add("%s: `%s` -> `%s`, which doesn't exist", where, k, m)
    }
  }

  check_entries("arguments", raw$arguments, universe$arguments, dead_keys = always_valid)
  check_entries("transform_options", raw$transform_options, universe$transform_options, dead_keys = universe$transform_options)
  check_entries("transform_names", raw$transform_names, universe$transform_names)
  for (k in names(raw$transform_names)) {
    if (startsWith(k, "vg_")) add("transform_names: key `%s` should be written without the `vg_` prefix", k)
  }
  for (p in names(raw$values)) {
    allowed <- .vg_enum_props[[p]]
    if (is.null(allowed)) {
      add("values: `%s` is not an argument with a fixed set of values", p)
      next
    }
    check_entries(paste0("values/", p), raw$values[[p]], allowed, dead_keys = allowed, exact = TRUE)
  }
  problems
}

# The synonym file as parsed, before the loader cleans it up.
raw_synonym_table <- function() {
  parse_yaml12(paste(readLines(synonym_table_path(), warn = FALSE), collapse = "\n"))
}

# Number of entries in a table (values are grouped one level deeper).
count_synonym_entries <- function(table) {
  flat <- c("arguments", "transform_options", "transform_names")
  sum(lengths(table[flat])) + sum(lengths(table$values))
}
