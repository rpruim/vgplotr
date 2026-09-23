# Notes: recreating mosaic's example gallery with vgplotr

Each example recreates a page from <https://idl.uw.edu/mosaic/examples/>.
Verification method: build the R spec, then compare `to_json(spec)`
against the "JSON" tab on the corresponding mosaic example page (rather
than rendering, per instruction -- rendering/visual verification is done
separately). Data comes from `vg_example_url("<file>")` (mosaic's own
`data/` directory on GitHub) rather than a local copy.

The `.qmd` files themselves now live in `vignettes/articles/` (flat,
`<category>-<name>.qmd`), rendered into the pkgdown site's "Examples"
gallery (`_pkgdown.yml`) rather than under this `examples/` directory --
moved there because pkgdown only discovers `.qmd`/`.Rmd` articles inside
`vignettes/`. They're flat rather than nested one level under
`vignettes/articles/<category>/<name>.qmd` because pkgdown's quarto
support (as of pkgdown 2.2.0) fails to find the built output for
articles nested in a subdirectory ("No built file found") -- confirmed
directly (a flat file with the same hyphenated name builds fine, an
otherwise-identical nested one doesn't). `vignettes/articles/` is in
`.Rbuildignore`, so none of this is part of `R CMD build`/`check`; only
`pkgdown::build_site()` touches it. This file (`examples/NOTES.md`)
stays here as a dev-notes hand-off, not part of the gallery itself.

## Bugs found and fixed while doing this

1. **`merge_attrs()` silently dropped any plot attribute explicitly set to
   `NULL`** (`R/utils.R`). Mosaic uses `xAxis: null`/`yAxis: null` to hide
   an axis -- a real, meaningful value, not "unset." The old code did
   `old[[nm]] <- new[[nm]]`, and `[[<-` with a `NULL` RHS always *deletes*
   the list element in R, regardless of intent. Fixed to use single-bracket
   assignment (`old[nm] <- new[nm]`), which correctly stores an explicit
   `NULL` without deleting the key. Found via the "Mark Types" example
   (`defaultAttributes` sets `xAxis(null)`/`yAxis(null)`).

2. **Same bug, second location**: `serialize_layout()` (`R/serialize.R`)
   used `utils::modifyList(plot_defaults, layout$attrs)` to merge
   `vg_plot_defaults()` into a plot's own attrs -- `modifyList()` has the
   identical "NULL removes the element" behavior. Added `override_attrs()`
   (`R/utils.R`) as a NULL-safe replacement (no conflict warning, since
   overriding a default is the expected, common case here, unlike
   `merge_attrs()`'s sibling-mark-conflict warning).

3. **`param()` as a transform's field wasn't supported at all** --
   `vg_column(param(x))` (mosaic's `vg.column($x)`, used for menu-driven
   dynamic column selection) errored at serialization with "Only simple
   column references... can be used inside a mapping formula." Fixed
   `serialize_expr()` (`R/serialize.R`) to special-case `param()` calls
   (evaluate directly and serialize the resulting `vg_param`, same
   treatment as `sql()`/`agg()`) *and* to handle a bare symbol whose
   *value* is a `vg_param` (e.g. `xp <- param(x); vg_column(xp)`) rather
   than always treating a symbol as a literal column name. Found via the
   "Symbol Plots" example (its X/Y menus rebind `x =`/`y =` at runtime).

4. **`vg_data(name, query = "...")` serialized as an object
   (`{"query": "..."}`) instead of mosaic-spec's bare-string `DataQuery`
   type** (`"name": "SELECT ..."`, no wrapper -- verified directly against
   mosaic's JSON schema, where `DataQuery` is literally `{type:
   "string"}`, unlike `DataFile`/`DataTable`/etc., which really are
   objects). Fixed in both `serialize_data_source()` (used by
   `to_json()`/`to_yaml()`) and `as_spec_payload()` (used by
   `vg_render()`), which had separate, independent data-source handling
   and both needed the fix. Found via the "Airline Travelers" example
   (`endpoint` is a derived temp table from a raw SQL query).

All four fixes are covered by new tests (`test-merge.R`, `test-serialize.R`,
`test-transforms.R`) and the full suite is green after each.

5. **A mark's own property could be silently stolen by a same-named
   PlotAttributes property** (`R/mark.R`/`R/interactor.R`/`R/utils.R`).
   mosaic-spec disambiguates `inset` (and `clip`, `margin*`, `aria*`) by
   *where* the key appears -- a mark's own `inset` (e.g. shrinking just
   that mark's rects) is a different property from the plot-wide `inset`
   default that a `vg_scale_all(inset = ...)`-style attribute sets, even
   though they share a name and description pattern ("shorthand to set
   the same default for..."). `split_plot_args()` routed *any* argument
   whose name matched `vg_plot_level_args()` up to the plot, with no way
   to tell "this mark's own declared property" apart from "an arbitrary
   plot attribute riding along on this mark call" (a real, useful,
   pre-existing feature -- e.g. `vg_mark_dot(x = ~a, width = 680)`).
   Fixed by generating, per mark/interactor type, its own property-name
   set (`.vg_mark_own_props`/`.vg_interactor_own_props`,
   `R/attrs-generated.R`) and excluding those names from the bubble-up
   check (`split_plot_args(args, protect = ...)`). Found via "Moving
   Average", where `vg_mark_rect_y(inset = 1, ...)` was landing on the
   *plot* instead of staying on the `rectY` mark.

6. **No way to set a mark's data-object `optimize` flag at all** --
   mosaic-spec's mark data source (`{"from": ..., "filterBy": ...,
   "optimize": ...}`) has a third option, `optimize` (disables
   mark-specific query optimizations like M4/LTTB line simplification),
   that `data_from`/`filter_by` had no counterpart for. Added a matching
   `data_optimize` argument to every generated mark wrapper (same
   pattern as `data_from`/`filter_by`) and taught `serialize_encodings()`
   (`R/serialize.R`) to fold it into the `data` object. Found via "Line
   Multi-Series" (`vg.from("bls_unemp", {optimize: false})`).

7. **A param's value couldn't combine other param references** --
   mosaic's own `rotate: [$longitude, $latitude]` pattern (one param
   built from two others, e.g. to combine two sliders into a single
   globe-rotation value) crashed `to_json()`/`vg_render()` outright
   (`No method asJSON S3 class: vg_param`), since `spec$params` was
   spliced into the output completely raw, with no serialization at
   all. Fixed by recursing into a param's declared value
   (`serialize_param_value()`/`serialize_params()`, `R/serialize.R`) so
   any `vg_param` found anywhere inside it -- not just at the top level
   -- becomes its `"$name"` string. Found via "Earthquakes Globe"
   (`vg_params(rotate = list(param(longitude), param(latitude)))`).

8. **Plot-level attribute values were never serialized at all** -- a
   more general version of bug 7: `vg_plot()`/`vg_attributes()`/
   `vg_plot_defaults()` attrs were spliced into the output completely
   raw (no `serialize_value()` call anywhere on the plot-attrs path),
   so a plot-level attribute whose *value* is itself a `param()`
   reference (e.g. `vg_plot(projection_rotate = param(rotate))`, for a
   slider-controlled globe rotation) crashed the same way. Fixed by
   running every attrs value through `serialize_value()` before
   emitting it, in `serialize_layout()` and both `spec$attrs` merge
   sites (`R/serialize.R`). Found alongside bug 7, same example.

9. **mosaic-spec's top-level `config` (e.g. `{"extensions": "spatial"}`,
   the DuckDB extensions to load before the spec runs) was completely
   unreachable** -- `vgspec` already had a `config` slot
   (`vg_create()`, `R/create.R`), but nothing ever set or serialized
   it: no `vg_config()` function existed, and `to_json()`/`to_yaml()`/
   `vg_render()` never emitted `spec$config` even if it had been set by
   hand. Added `vg_config()` (mirrors `vg_meta()`) and wired
   `spec$config` into both serialization paths. Found via "NYC Taxi
   Rides", which needs the `spatial` extension loaded before its
   `ST_Transform()`/`ST_Point()` queries can run.

Bugs 7-9 are covered by new tests (`test-serialize.R`, `test-config.R`)
and the full suite is green after each.

10. **`vg_params()`/`vg_meta()`/`vg_config()` silently dropped an explicit
    `NULL` value** -- the same `utils::modifyList()` pitfall as bug 1
    (`merge_attrs()`) and bug 2 (`serialize_layout()`), just in three
    more places: `vg_params(offset = NULL)` (mosaic's own initial value
    for `densityY`'s `offset`, later set by a menu) silently declared no
    `offset` param at all, rather than one with value `null`. Fixed by
    switching all three to the existing NULL-safe `override_attrs()`
    helper (`R/utils.R`, from bug 2's fix). Found via "Density Groups".

All ten are covered by new tests (`test-merge.R`, `test-plot-defaults.R`,
`test-serialize.R`, `test-transforms.R`, `test-schema-generated.R`,
`test-config.R`, `test-params.R`) and the full suite is green after each.

## Usage tips found along the way (not bugs -- just non-obvious)

- Mosaic's `["2019"]` convention (wrap a mark-encoding value in an array
  to force a **literal constant**, as opposed to a bare string, which is
  interpreted as a **column name**) is already supported in vgplotr --
  just pass an R `list()` instead of a bare vector, e.g. `text =
  list("2019")`. `serialize_value()`'s literal-passthrough branch already
  serializes a plain list as a JSON array (vs. a length-1 atomic vector,
  which `auto_unbox` collapses to a bare scalar) -- no code change needed,
  just wasn't obvious without checking. Found via "Airline Travelers".

- **Selections are already supported, despite the docs saying otherwise.**
  `vg_params()`'s own docstring claimed "selection support not yet
  implemented," but this was stale/wrong: `vg_params()` just stores
  whatever value it's given as-is, so passing mosaic-spec's `Selection`
  shape directly (`vg_params(query = list(select = "intersect"))`, mirroring
  the JS side's `vg.Selection.intersect()`) round-trips correctly with no
  code changes needed. Fixed the misleading docstring (`R/create.R`) and
  added a regression test (`test-serialize.R`) so this doesn't silently
  regress. Found via "Sorted Bars", whose menu's `as = query` output is a
  Selection (used as `filter_by` on the bar mark), not a plain Param.

- Similarly, `vg_create()`'s docstring claimed multi-plot layouts via
  `vg_vconcat()`/`vg_hconcat()` were "not yet implemented" -- also stale;
  they're fully implemented and used throughout this gallery (e.g.
  "Symbol Plots", "Voronoi Diagram"). Fixed the docstring.

## Cosmetic (non-functional) JSON differences, not fixed

- vgplotr's serializer always wraps a mark inside a `vconcat`/`hconcat`
  child as `{"plot": [...marks...], ...attrs}`, even when there's exactly
  one mark and no extra attributes -- mosaic's own serializer collapses
  that case to a bare mark object (`{"mark": "barY", ...}`). Both are
  valid mosaic-spec JSON; not fixed since it's purely a verbosity
  difference with no behavior change. (Would need `serialize_layout()` to
  special-case "one item, `attrs` empty".)
- Object key order differs (roughly alphabetical for named args reaching
  `...`, vs. call-argument order in mosaic's own examples). No functional
  difference; not fixed.
- Same "single item, no collapse" verbosity issue as above, but for
  `vg_vconcat()`/`vg_hconcat()`: a layout with exactly one child (e.g. a
  lone `vg_table()` input with nothing else) still serializes as
  `"vconcat": [...]` with one element, where mosaic's own serializer would
  put that one child directly at the top level. Valid, semantically
  identical mosaic-spec JSON either way. Found via "Sortable Table".

## Examples

Status key: ✅ JSON matches (aside from the cosmetic differences above and
the intentional `vg_example_url()` vs. relative-path difference) · ⚠️ gap/issue
noted below.

### Basic Marks & Inputs

- ✅ `mark-types.qmd`
- ✅ `symbol-plots.qmd`
- ✅ `axes-gridlines.qmd`
- ✅ `airline-travelers.qmd`
- ✅ `aeromagnetic-survey.qmd`
- ✅ `athlete-birth-waffle.qmd`
- ✅ `driving-shifts.qmd`
- ✅ `population-arrows.qmd`
- ✅ `presidential-opinion.qmd`
- ✅ `voronoi.qmd`
- ✅ `seattle-temperatures.qmd`
- ✅ `sorted-bars.qmd` (uses a mosaic Selection, not just a Param -- see
  "Bugs found and fixed" above)
- ✅ `table.qmd` (aside from the vconcat-collapse cosmetic difference noted
  above)

### Data Transformation

- ✅ `athlete-height.qmd`
- ✅ `bias.qmd`
- ✅ `linear-regression.qmd`
- ✅ `linear-regression-10m.qmd` (10M-row remote dataset; rendering may be
  slow -- see the note in the file)
- ✅ `moving-average.qmd` (surfaced the `inset`/plot-attribute collision
  bug -- see "Bugs found and fixed" above)
- ✅ `line-multi-series.qmd` (surfaced the missing `data_optimize` support
  -- see "Bugs found and fixed" above)
- ✅ `normalize.qmd`
- ✅ `seattle-weather-pivot.qmd` (aside from the vconcat-collapse cosmetic
  difference)
- ✅ `overview-detail.qmd`
- ✅ `wind-map.qmd`
- ✅ `wnba-shots.qmd`

### Maps & Spatial Data

- ✅ `earthquakes-feed.qmd`
- ✅ `earthquakes-globe.qmd` (surfaced bugs 7 and 8 -- see "Bugs found
  and fixed" above)
- ✅ `us-state-map.qmd`
- ✅ `us-county-map.qmd`
- ✅ `unemployment.qmd`
- ✅ `walmart-openings.qmd`
- ✅ `nyc-taxi-rides.qmd` (surfaced the missing `vg_config()` support --
  see "Bugs found and fixed" above)

### Multi-View Coordination

- ✅ `flights-200k.qmd`
- ✅ `flights-10m.qmd` (10M-row remote dataset; rendering may be slow)
- ✅ `gaia.qmd` (confirms the batch-3 plot-attribute serialization fix:
  `color_scale`/`y_scale` are param references, e.g. `vg_plot(color_scale
  = scale_type)`)
- ✅ `observable-latency.qmd` (confirms the mark/plot-attribute collision
  fix: the `raster` mark's own `width`/`height` differ from the plot's)
- ✅ `athletes.qmd` (confirms the batch-3 nested-param-reference fix: a
  Selection's `include` field names another Selection,
  `list(select = "intersect", include = category)`)
- ✅ `protein-design.qmd`
- ✅ `pan-zoom.qmd`
- ✅ `splom.qmd` (16-plot grid; built with an R loop rather than
  mosaic's own hand-written repetition, since vgplotr specs are just R
  values/functions)
- ✅ `weather.qmd`

No new bugs found in this batch -- every gap this category could have
exercised (nested param references, plot-attribute param values, the
mark/plot-attribute collision) had already been fixed while working
through "Maps & Spatial Data".

### Density Visualizations

- ✅ `contours.qmd`
- ✅ `density-groups.qmd` (surfaced bug 10 -- see "Bugs found and fixed"
  above)
- ✅ `density1d.qmd`
- ✅ `density2d.qmd` (confirms the mark/plot-attribute collision fix
  holds when the mark's own `width`/`height` are bound to a param, not
  just a literal)
- ✅ `flights-density.qmd`
- ✅ `flights-hexbin.qmd`
- ✅ `line-density.qmd`

## Summary

All 47 examples across all 5 gallery categories (Basic Marks & Inputs,
Data Transformation, Maps & Spatial Data, Multi-View Coordination,
Density Visualizations) are recreated, and each one's `to_json()`
output was verified to match mosaic's own reference JSON exactly (aside
from the intentional `vg_example_url()` vs. relative-path difference and
cosmetic object-key ordering). Along the way, this exercise found and
fixed 10 real vgplotr bugs (see above), all covered by new regression
tests, with the full test suite green throughout.
