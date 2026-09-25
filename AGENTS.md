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

## Upstream issues to watch

Bugs in mosaic itself that vgplotr works around. Each has a workaround that
does nothing once upstream is fixed, and a check that says when that happens.
`Rscript data-raw/check-upstream.R` reports on the latest published release
(`Rscript data-raw/check-upstream.R 0.31.0` for a specific one), and
`data-raw/update-schema.R` runs it for the pinned `MOSAIC_VERSION`, so a version
bump reports on it too. Re-run it whenever mosaic publishes a release.

### Window frames with plain-number or `null` offsets (mosaic-spec 0.31.0)

`parseWindowFrame()` in mosaic-spec's `src/ast/WindowFrameNode.js` builds each
literal frame offset with `LiteralNode` imported from `@uwdata/mosaic-sql`,
which has no `instantiate()`; `WindowFrameNode.instantiate()` then calls it and
throws `s.instantiate is not a function`. So `rows = c(6, 0)`, `range =
list(vg_days(6), 0)` or a `NULL` (unbounded) offset fail to render -- and one
failing plot takes the whole page down. Only frames whose offsets are *all*
interval transforms (`{days: 6}`) worked. Reproduced with a hand-written JSON
spec, so it is not a vgplotr bug. 0.31.0 was the latest release when this was
found (2026-09-25).

Workaround: `patchWindowFrames()` in `inst/htmlwidgets/vgplotr.js` wraps
`TransformNode.prototype.instantiate` to give any offset lacking `instantiate()`
a stand-in returning its value. It touches only offsets that lack the method, so
it is inert once mosaic is fixed.

When `check-upstream.R` says it is fixed: rebuild the bundle on that version
(`cd data-raw/js && npm install && node build.js`, after bumping
`MOSAIC_VERSION` everywhere it is pinned), confirm a frame like `rows = c(6, 0)`
renders with `patchWindowFrames()` disabled, then delete that function and its
call, and delete this entry. Worth filing upstream (uwdata/mosaic) if not
already: the fix is importing spec's own `./LiteralNode.js` in that file.

Frame offsets are distances, not signed values: mosaic-sql takes `abs()` of a
number and turns `0` into `CURRENT ROW`, but writes an interval's sign straight
into SQL, so `vg_days()` etc. reject negatives. That is mosaic's design, not
part of this bug.

## Open design questions

Things deliberately left undecided, to revisit rather than forget.

### Should `vg_mark()` and `vg_interactor()` (the generic constructors) accept snake_case?

*Noted 2026-09-19. Legends resolved 2026-09-23 -- see below; the generic
constructors are still open.* Today the spelling rule depends on which
function you call:

- The generated wrappers -- `vg_mark_*()`, the interactor and input functions
  (`vg_toggle()`, `vg_slider()`, ...) -- take snake_case (`fill_opacity`,
  `tick_size`), and also mosaic's exact camelCase key (`fillOpacity`) through
  `...`. Scales, guides and attribute setters are snake_case too, a plot
  attribute (`x_domain`/`xDomain`) is accepted either way *everywhere*, and
  now **every legend** (`vg_legend()`, `vg_legend_color()`, ...) is too --
  `vg_legend_color(tick_size = 5)` and `vg_legend_color(tickSize = 5)` both
  work, via `canonicalize_legend_prop_names()`/`.vg_legend_props_snake`
  (R/utils.R, R/attrs-generated.R), mirroring `canonicalize_plot_attr_names()`.
- The generic `vg_mark()` / `vg_interactor()` still accept **only** the exact
  camelCase key for their own options; `vg_mark("dot", fill_opacity = 1)`
  warns "not a property".

What that costs, for what's still open: warnings that suggest a name must
spell it per caller (a suggestion is only useful if typing it doesn't warn
again), which is why `build_mark()`/`build_interactor()` take a `style`
("snake" from a wrapper, "camel" from the generic) and `spell_names()`
(`R/utils.R`) applies it -- this plumbing is still needed for the
mark/interactor generic-vs-wrapper split, even though legends no longer need
a `style` distinction (there's only one constructor family, so
`warn_unrecognized_legend_args()`/`check_transform_calls()` always pass
`style = "snake"` now).

The remaining question: make snake_case work in `vg_mark()`/`vg_interactor()`
too, the same way -- canonicalising snake_case to the exact key via
something like `canonicalize_plot_attr_names()`/the new
`canonicalize_legend_prop_names()`. The rule would become fully uniform
("snake_case everywhere; the exact camelCase key always works too"), and the
`style` plumbing could be deleted entirely.

Facts to start from:

- Every property of every mark, interactor and input already has a snake_case
  argument on its wrapper (all 3,558, verified by
  `tests/testthat/test-suggest-style.R`), and the generator fails loudly if two
  properties ever collide once snake_cased (`check_snake_collisions()` in
  `data-raw/update-schema.R`; zero collisions in v0.31.0) -- the same check
  now also covers `PlotLegend`'s properties.
- Legends kept their `...`-based, hand-written constructors (deliberately --
  "only 3 types, small enough to maintain by hand") rather than gaining real
  snake_case formals from the generator; `.vg_legend_props`/
  `.vg_legend_prop_types`/`.vg_legend_props_snake` (all schema-derived, in
  R/attrs-generated.R) are what `vg_legend()` checks/canonicalizes/documents
  against, so none of it can go stale when `MOSAIC_VERSION` bumps. The same
  choice (canonicalize on `...`, vs. real generated formals) applies to
  `vg_mark()`/`vg_interactor()` if/when this is resolved for them too.
- Tests that pin today's behaviour and would change: `test-suggest-style.R`,
  `test-mark-args.R`, `test-interactor-args.R` (each has a "the generic ...
  takes camelCase" test), and the `?vg_mark`/`?vg_interactor` documentation of
  `...`.
