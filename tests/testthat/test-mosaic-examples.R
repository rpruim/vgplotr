# Snapshot-tests vgplotr's generated mark/interactor/input vocabulary
# against mosaic's own published example corpus (vendored by
# data-raw/update-mosaic-examples.R into fixtures/mosaic-examples/, pinned
# to the same MOSAIC_VERSION as data-raw/update-schema.R) -- catches
# vgplotr's generated schema falling behind a mark/interactor/input type
# that mosaic's own real-world examples actually use, independent of
# whatever vgplotr's own unit tests assume the schema looks like.

# Recursively collects every scalar-string value found under a `key` at
# any level of a parsed YAML/JSON spec -- e.g. every `mark:`/`input:`/
# `select:` value, wherever it occurs (a mark's own `mark:`, an embedded
# interactor's `select:`, a standalone input's `input:`, or a param's own
# `select:` resolution strategy, e.g. `crossfilter`).
collect_type_refs <- function(x, key) {
  out <- character(0)
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms) && key %in% nms) {
      val <- x[[key]]
      if (is.character(val) && length(val) == 1) out <- c(out, val)
    }
    for (el in x) out <- c(out, collect_type_refs(el, key))
  }
  out
}

fixture_files <- list.files(
  testthat::test_path("fixtures", "mosaic-examples"),
  pattern = "\\.yaml$", full.names = TRUE
)

test_that("mosaic's example corpus was actually vendored", {
  expect_gt(length(fixture_files), 0)
})

test_that("every mark/interactor/input type used in mosaic's own examples is recognized", {
  skip_if(length(fixture_files) == 0, "no vendored mosaic examples found")

  # `select:` means three different things depending on where it appears,
  # confirmed against mosaic's own schema (not guessed) -- an embedded plot
  # interactor (intervalX, pan, ...); a param's own selection-resolution
  # strategy (crossfilter/intersect/single/union, `Selection.select`); or,
  # for a Slider/similar input, that input's own predicate-mode option
  # (point/interval, `Slider.select`). All three are legitimate depending
  # on context, so all three vocabularies are accepted for any `select:`
  # found, rather than tracking structural position just to tell them apart.
  known_non_interactor_selects <- c("crossfilter", "intersect", "single", "union", "point", "interval")

  for (path in fixture_files) {
    spec <- parse_spec_string(paste(readLines(path, warn = FALSE), collapse = "\n"))

    marks <- unique(collect_type_refs(spec, "mark"))
    unknown_marks <- setdiff(marks, names(.vg_mark_own_props))
    expect_true(
      length(unknown_marks) == 0,
      info = sprintf(
        "%s uses mark type(s) vgplotr doesn't recognize: %s -- rerun data-raw/update-schema.R",
        basename(path), paste(unknown_marks, collapse = ", ")
      )
    )

    inputs <- unique(collect_type_refs(spec, "input"))
    unknown_inputs <- setdiff(inputs, names(.vg_interactor_own_props))
    expect_true(
      length(unknown_inputs) == 0,
      info = sprintf(
        "%s uses input type(s) vgplotr doesn't recognize: %s -- rerun data-raw/update-schema.R",
        basename(path), paste(unknown_inputs, collapse = ", ")
      )
    )

    selects <- unique(collect_type_refs(spec, "select"))
    unknown_selects <- setdiff(selects, c(.vg_interactor_types, known_non_interactor_selects))
    expect_true(
      length(unknown_selects) == 0,
      info = sprintf(
        "%s uses select/interactor type(s) vgplotr doesn't recognize: %s -- rerun data-raw/update-schema.R",
        basename(path), paste(unknown_selects, collapse = ", ")
      )
    )
  }
})

test_that("the vocabulary actually exercised by mosaic's examples is stable", {
  skip_if(length(fixture_files) == 0, "no vendored mosaic examples found")

  all_marks <- character(0)
  all_inputs <- character(0)
  all_selects <- character(0)
  for (path in fixture_files) {
    spec <- parse_spec_string(paste(readLines(path, warn = FALSE), collapse = "\n"))
    all_marks <- c(all_marks, collect_type_refs(spec, "mark"))
    all_inputs <- c(all_inputs, collect_type_refs(spec, "input"))
    all_selects <- c(all_selects, collect_type_refs(spec, "select"))
  }

  # A human-reviewable snapshot of exactly which vocabulary mosaic's own
  # examples exercise -- not a pass/fail check by itself (that's the test
  # above), but flags at a glance, on any corpus refresh, which types are
  # newly (un)covered.
  expect_snapshot(sort(unique(all_marks)))
  expect_snapshot(sort(unique(all_inputs)))
  expect_snapshot(sort(unique(all_selects)))
})
