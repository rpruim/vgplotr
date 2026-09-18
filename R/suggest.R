# "Did you perhaps mean ...?" suggestions for a misspelled name, much like
# git's own for a mistyped command: find the *closest* valid names, and only
# offer them if they're actually close. Used by warn_unrecognized_args()
# (R/utils.R) to enrich the warning for an unrecognized mark/interactor/
# input/legend argument.
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
# `_`/`.` (so `fill_opacity`, `fillopacity` and `fillOpacity` are all the
# same name), but the candidates come back exactly as spelled. Like git,
# only the *best* match is offered -- every candidate tied for the smallest
# distance, never a runner-up -- and only if that distance is small relative
# to the name's length (about one edit per three characters, at least one),
# so a name that resembles nothing gets no suggestion rather than a
# far-fetched one. More than `max_suggestions` tied candidates means the
# match is too vague to be useful (a lone `x` is one edit from half the
# schema), so that yields none either.
similar_names <- function(name, candidates, max_suggestions = 3) {
  norm <- function(x) tolower(gsub("[_.]", "", x))
  candidates <- unique(candidates)
  target <- norm(name)
  if (length(candidates) == 0 || !nzchar(target)) return(character())

  dist <- stringdist::stringdist(target, norm(candidates), method = "osa")
  best <- min(dist)
  limit <- max(1L, nchar(target) %/% 3L)
  hits <- candidates[dist == best]
  if (best > limit || length(hits) > max_suggestions) return(character())
  hits
}

# The suggestions for one unrecognized argument `name`, given the valid
# names `candidates` for the function it was passed to. `color`/`colour` (and
# near-misses like `colr`) is by far the most common mistake -- mosaic, like
# Observable Plot, has no such channel, only `fill` (the inside of a shape)
# and `stroke` (its outline or line) -- so it gets those two, whichever this
# function actually has, unless it really does have a `color` property (the
# axis marks do), in which case it's just a near-miss of that like any
# other. When it has neither `fill` nor `stroke` (a brush, a legend, ...)
# there is simply nothing to suggest, and the name deliberately isn't
# fuzzy-matched against everything else either: it would only turn up
# something unrelated that merely starts with the same letters (`colorN`).
# `present` names (already supplied alongside `name`) are never suggested:
# `fil =` next to a valid `fill =` isn't a typo of it.
suggest_names <- function(name, candidates, present = character()) {
  candidates <- setdiff(candidates, present)
  if (!("color" %in% candidates) && stringdist::stringdist(tolower(name), "color", method = "osa") <= 1) {
    return(intersect(c("fill", "stroke"), candidates))
  }
  similar_names(name, candidates)
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
