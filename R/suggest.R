# "Did you perhaps mean ...?" suggestions for a misspelled name, much like
# git's own for a mistyped command: find the *closest* valid names, and only
# offer them if they're actually close. Used by warn_unrecognized_args()
# (R/utils.R) to enrich the warning for an unrecognized mark/interactor/
# input/legend argument.
#
# Edit distance only finds *misspellings*. A different word for the same thing
# -- `alpha` for `opacity`, `linewidth` for `stroke_width`, `mean` for `vg_avg`,
# habits carried over from ggplot2, base R and dplyr -- is looked up first in
# the synonym table, inst/extdata/suggestion-synonyms.yaml (its header explains
# the format and how to amend it; tests/testthat/test-synonyms.R checks that
# every entry still points at something real).
#
# Distances are stringdist::stringdist(method = "osa"): the optimal-string-
# alignment form of Damerau-Levenshtein, i.e., the fewest single-character
# insertions, deletions, substitutions, or swaps of two adjacent characters
# that turn one name into the other. Counting a swap as one edit (as plain
# Levenshtein -- utils::adist() -- does not) matters because a transposed
# pair is one of the most common typos (`widht`, `lenght`): measured on
# generated typos of every valid name, "osa" finds the right name for every
# swap, where plain Levenshtein misses about one in six. (stringdist::
# amatch() isn't used: it returns a single match, arbitrarily the first of
# any tie -- e.g. `zDomain` is equally close to `rDomain`, `xDomain` and
# `yDomain`, and it would offer just `rDomain` -- and can't tell a close
# match from a vague one, both of which similar_names() below has to.)

# The valid names in `candidates` closest to `name`, or character(0) if
# nothing is close enough to be worth suggesting. Compared ignoring case and
# `-`, `_`, `.` and spaces (so `fill_opacity`, `fillopacity` and `fillOpacity`
# are all the same name -- and `cardinal_open` the same value as
# `cardinal-open`), but the candidates come back exactly as spelled. A
# *leading* `-`/`+` is kept, since it means something (an `order` of
# `-value` is descending, not a spelling of `value`). Like git,
# only the *best* match is offered -- every candidate tied for the smallest
# distance, never a runner-up -- and only if that distance is small relative
# to the name's length (about one edit per three characters, at least one),
# so a name that resembles nothing gets no suggestion rather than a
# far-fetched one. More than `max_suggestions` tied candidates means the
# match is too vague to be useful (a lone `x` is one edit from half the
# schema), so that yields none either. `limit` overrides the allowed distance
# for a caller that knows better than the name's own length (see
# transform_name_suggestion(), R/transforms.R).
similar_names <- function(name, candidates, max_suggestions = 3, limit = NULL) {
  candidates <- unique(candidates)
  target <- normalize_name(name)
  if (length(candidates) == 0 || !nzchar(target)) return(character())

  dist <- stringdist::stringdist(target, normalize_name(candidates), method = "osa")
  best <- min(dist)
  if (is.null(limit)) limit <- max(1L, nchar(target) %/% 3L)
  hits <- candidates[dist == best]
  if (best > limit || length(hits) > max_suggestions) return(character())
  hits
}

# How names are compared when suggesting: ignoring case and `-`, `_`, `.` and
# spaces, so `fill_opacity`, `fillOpacity` and `fill-opacity` are one name. A
# *leading* `-`/`+` is kept, since it means something (an `order` of `-value` is
# descending, not a spelling of `value`).
normalize_name <- function(x) tolower(gsub("(?<!^)-|[_. ]", "", x, perl = TRUE))

# --- the synonym table --------------------------------------------------------

.vgplotr_synonyms <- new.env(parent = emptyenv())

synonym_table_path <- function() {
  system.file("extdata", "suggestion-synonyms.yaml", package = "vgplotr")
}

# Reads the synonym file into list(arguments = , values = , transform_options =
# , transform_names = ): each a named list of wrong word -> character vector of
# candidates (`values` has one such list per argument name). Read with the same
# YAML 1.2 rules as a pasted spec (parse_yaml12(), R/serialize.R), so a key like
# `n` or `on` needs no quoting. A suggestion is a nicety produced while a
# warning is being built, so a missing or malformed file must never break the
# call: it just means no synonyms (test-synonyms.R is what catches a bad edit).
read_synonym_table <- function(path = synonym_table_path()) {
  empty <- list(arguments = list(), values = list(), transform_options = list(), transform_names = list())
  if (!nzchar(path) || !file.exists(path)) return(empty)
  tryCatch({
    text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    parsed <- parse_yaml12(text)
    flat <- function(section) clean_synonym_section(parsed[[section]])
    list(
      arguments = flat("arguments"),
      values = lapply(parsed$values, clean_synonym_section),
      transform_options = flat("transform_options"),
      transform_names = flat("transform_names")
    )
  }, error = function(e) empty)
}

# A section as a named list of non-empty character vectors; anything else
# (a scalar where a list belongs, an entry with no candidates) is dropped.
clean_synonym_section <- function(x) {
  if (!is.list(x) || is.null(names(x))) return(list())
  out <- lapply(x, function(v) as.character(unlist(v)))
  out[nzchar(names(out)) & lengths(out) > 0]
}

# The table, read once and kept.
vg_synonym_table <- function() {
  if (is.null(.vgplotr_synonyms$table)) .vgplotr_synonyms$table <- read_synonym_table()
  .vgplotr_synonyms$table
}

# One section of it (`values` needs the argument name too).
synonym_section <- function(section, property = NULL) {
  table <- vg_synonym_table()[[section]]
  if (!is.null(property)) table <- table[[property]]
  if (is.null(table)) list() else table
}

# Looks `name` up in one synonym section `table`. NULL if it isn't a known
# wrong word. Otherwise the candidates that are actually in `valid` -- spelled
# the way `valid` spells them, in the table's order -- which can be empty: a
# known wrong word with nothing applicable here (`color` on a brush) means "no
# suggestion", deliberately NOT a fallback to edit distance, which would only
# turn up something unrelated (`colorN`). With `near = TRUE` a one-edit
# misspelling of a key at least five letters long matches it too (`colr` finds
# `color`), so the common typos of a key needn't each be listed; shorter keys
# must match exactly, since one edit is too big a share of a short word.
lookup_synonym <- function(name, valid, table, near = TRUE) {
  keys <- names(table)
  target <- normalize_name(name)
  if (length(keys) == 0 || !nzchar(target)) return(NULL)

  normalized_keys <- normalize_name(keys)
  i <- match(target, normalized_keys)
  if (is.na(i)) {
    if (!near) return(NULL)
    dist <- stringdist::stringdist(target, normalized_keys, method = "osa")
    close <- which(dist <= 1 & nchar(normalized_keys) >= 5)
    if (length(close) == 0) return(NULL)
    i <- close[which.min(dist[close])]
  }

  wanted <- normalize_name(table[[i]])
  normalized_valid <- normalize_name(valid)
  hits <- which(normalized_valid %in% wanted)
  hits <- hits[order(match(normalized_valid[hits], wanted))]
  unique(valid[hits])
}

# Where a word's suggestions come from, in priority order:
#   1. an EXACT synonym key -- the table exists for exactly this, and it wins
#      even over a spelling match (`labels` is a ggplot2 word, not a misspelling
#      of `label`);
#   2. the closest real name by spelling -- a real name in the call must beat a
#      guess: with the table consulted first, a typo of a real name that happens
#      to sit one edit from some key (`ttle` for `title`, `lnieWidth` for
#      `lineWidth`, `labes` for `label`) was steered to the key's answer, which
#      is usually nothing applicable here;
#   3. a near-miss of a synonym key (`colr` for `color`), as a last resort.
# `spelling` is a function returning the spelling matches, so it only runs if
# step 1 found nothing.
suggest_with_synonyms <- function(name, valid, table, spelling) {
  hits <- lookup_synonym(name, valid, table, near = FALSE)
  if (!is.null(hits)) return(hits)
  hits <- spelling()
  if (length(hits) > 0) return(hits)
  hits <- lookup_synonym(name, valid, table, near = TRUE)
  if (is.null(hits)) character() else hits
}

# The suggestions for one unrecognized argument `name`, given the valid names
# `candidates` for the function it was passed to (see suggest_with_synonyms()
# for the order). `table` is the `arguments` synonym section unless the caller
# says otherwise -- transform options have their own. `present` names (already
# supplied alongside `name`) are never suggested: `fil =` next to a valid
# `fill =` isn't a typo of it, and `color = , fill =` should only point at
# `stroke`.
suggest_names <- function(name, candidates, present = character(),
                          table = synonym_section("arguments")) {
  candidates <- setdiff(candidates, present)
  suggest_with_synonyms(name, candidates, table, function() similar_names(name, candidates))
}

# The same for a value of an enum-typed argument (`textAnchor = "center"`):
# that argument's synonyms, then the closest spelling.
suggest_values <- function(value, allowed, property) {
  suggest_with_synonyms(
    value, allowed, synonym_section("values", property),
    function() similar_names(value, allowed)
  )
}

# "Did you perhaps mean `a`?", "... `a` or `b`?", "... `a`, `b` or `c`?" --
# or NULL for no suggestions. With `arg` (when a warning covers several
# unrecognized arguments, so it has to say which one each suggestion is
# for) it reads "For `arg`, did you perhaps mean ...?".
format_suggestion <- function(candidates, arg = NULL) {
  if (length(candidates) == 0) return(NULL)
  quoted <- paste0("`", candidates, "`")
  n <- length(quoted)
  options <- if (n == 1) quoted else paste0(paste(quoted[-n], collapse = ", "), " or ", quoted[[n]])
  if (is.null(arg)) {
    sprintf("Did you perhaps mean %s?", options)
  } else {
    sprintf("For `%s`, did you perhaps mean %s?", arg, options)
  }
}
