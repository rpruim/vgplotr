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

## Open design questions

Things deliberately left undecided, to revisit rather than forget.

### Should `vg_mark()`, `vg_interactor()` and legends accept snake_case?

*Noted 2026-09-19.* Today the spelling rule depends on which function you call:

- The generated wrappers -- `vg_mark_*()`, the interactor and input functions
  (`vg_toggle()`, `vg_slider()`, ...) -- take snake_case (`fill_opacity`,
  `tick_size`), and also mosaic's exact camelCase key (`fillOpacity`) through
  `...`. Scales, guides and attribute setters are snake_case too, and a plot
  attribute (`x_domain`/`xDomain`) is accepted either way *everywhere*.
- The generic `vg_mark()` / `vg_interactor()`, and **every legend**
  (`vg_legend()`, `vg_legend_color()`, ...), accept **only** the exact camelCase
  key for their own options; `vg_mark("dot", fill_opacity = 1)` and
  `vg_legend_color(tick_size = 5)` warn "not a property".

What that costs: warnings that suggest a name must spell it per caller (a
suggestion is only useful if typing it doesn't warn again), which is why
`build_mark()`/`build_interactor()` take a `style` ("snake" from a wrapper,
"camel" from the generic) and `spell_names()` (`R/utils.R`) applies it. Legends
are hand-written with `...` only, so they always suggest camelCase.

The question: make snake_case work in the generic constructors and in legends
too -- canonicalising snake_case to the exact key, as
`canonicalize_plot_attr_names()` already does for plot attributes. The rule
would become uniform ("snake_case everywhere; the exact camelCase key always
works too"), and the `style` plumbing could be deleted, so suggestions would
always be snake_case.

Facts to start from:

- Every property of every mark, interactor and input already has a snake_case
  argument on its wrapper (all 3,558, verified by
  `tests/testthat/test-suggest-style.R`), and the generator fails loudly if two
  properties ever collide once snake_cased (`check_snake_collisions()` in
  `data-raw/update-schema.R`; zero collisions in v0.31.0).
- Legends already have a schema-derived property list (`.vg_legend_props`,
  generated), so they could get real snake_case formals from the generator
  instead of `...`.
- Tests that pin today's behaviour and would change: `test-suggest-style.R`,
  `test-legend-args.R`, `test-mark-args.R`, `test-interactor-args.R` (each has a
  "the generic ... takes camelCase" test), and the `?vg_mark` / `?vg_legend`
  documentation of `...`.
