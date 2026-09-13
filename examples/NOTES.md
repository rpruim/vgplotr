# Notes: recreating mosaic's example gallery with vgplotr

Each example under `examples/<category>/` recreates a page from
<https://idl.uw.edu/mosaic/examples/>. Verification method: build the R
spec, then compare `to_json(spec)` against the "JSON" tab on the
corresponding mosaic example page (rather than rendering, per instruction
-- rendering/visual verification is done separately). Data comes from
`vg_data_url("<file>")` (mosaic's own `data/` directory on GitHub) rather
than a local copy.

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
the intentional `vg_data_url()` vs. relative-path difference) · ⚠️ gap/issue
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
