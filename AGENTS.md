# Notes for coding agents working on vgplotr

## Commit messages

Every commit gets a **single-line message** -- no bullet lists, no body
paragraphs, no rationale. Just the one line, imperative mood (e.g. "Fix
POSIXct timezone handling in to_json()/to_yaml()").

The fuller "why" -- context, alternatives considered, what was verified,
follow-up items -- goes in `design/commit-log.qmd` instead, as a new
`## <same title as the commit subject>` section appended to the bottom of
the file, with a `` `<short-hash>` &mdash; <date> &mdash; <author> `` byline
under the heading. That file is gitignored (`design/` is a local dev
record, excluded from `R CMD build`/`check`), so it's edited *after* the
commit exists (the hash has to be real) and is never itself committed.

When a chunk of work naturally splits into more than one commit (e.g. two
independent parts of one plan), each part still gets its own single-line
commit message and its own section in the log -- don't squash the
rationale for two commits into one log entry, and don't put multi-line
detail back into a commit message because splitting felt like it deserved
more explanation.

See `design/commit-log.qmd`'s own header comment for the full policy
statement and the `backup/pre-simplify-2026-09-14` git tag for what
commit messages looked like before this convention started.

## Generated files

`R/*-generated.R` and a few others are produced by `data-raw/update-schema.R`
(and `data-raw/update-mosaic-examples.R` for the vendored test corpus).
Never hand-edit a generated file -- change the generator and rerun it.
After regenerating, diff against the pre-change committed version and
confirm the diff is *exactly* the intended change, nothing incidental
(this has caught real accidental drift before).

## Testing

Render-test both **http-served and file://** (opened directly) when
verifying a browser-facing change -- a bug has slipped through before
that only reproduced in one of the two. A passing spec-level/unit test is
not sufficient verification for a rendering change; open a real browser.

## Suggestion synonyms

The "Did you perhaps mean ...?" suggestions in warnings and errors (`R/suggest.R`)
use edit distance for *misspellings* and a **synonym table** for different
*words* (`alpha` for `opacity`, `linewidth` for `stroke_width`, `mean` for
`vg_avg` -- habits from ggplot2, base R, dplyr). The table is a plain YAML file,
`inst/extdata/suggestion-synonyms.yaml`; its header comment explains the format.

To add or change an entry, edit that file and run
`devtools::test(filter = "synonyms")`. The tests check that every candidate
still exists in the current schema, that no key is repeated, and that no entry
can never fire, so a typo in the file (or a name a future mosaic version drops)
fails a test instead of shipping a bad suggestion. Write candidates in
snake_case; each is shown in whatever spelling the function being called
accepts. Two rules worth knowing before adding one: an exact key never falls
back to edit distance (so list a real name as a candidate when it is the right
answer somewhere, as `labels` does with `label`), and a near-miss of a key is
only a last resort -- a real name in the call always wins.
