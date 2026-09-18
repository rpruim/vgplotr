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

# Recursively collects every (property name, value) pair anywhere in a
# parsed spec whose name is a known enum property (.vg_enum_props,
# R/attrs-generated.R) and whose value is a length-1 character -- the same
# shape warn_unrecognized_enum_values() (R/utils.R) checks at call time.
collect_enum_kv <- function(x) {
  out <- list()
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms)) {
      for (nm in nms) {
        if (nm %in% names(.vg_enum_props)) {
          val <- x[[nm]]
          if (is.character(val) && length(val) == 1) out[[length(out) + 1]] <- list(name = nm, value = val)
        }
      }
    }
    for (el in x) out <- c(out, collect_enum_kv(el))
  }
  out
}

test_that(".vg_enum_props recognizes every enum-typed literal value mosaic's own examples actually use", {
  skip_if(length(fixture_files) == 0, "no vendored mosaic examples found")

  # Three categories of raw-YAML `key: value` pair can't actually happen as
  # a warn_unrecognized_enum_values(args) false positive, even though they
  # show up walking the parsed YAML this way, so they're excluded here
  # rather than chased as real gaps:
  #  - `select`/`type` are structurally ambiguous key names reused for
  #    something else entirely elsewhere in the same YAML (`select` is also
  #    an embedded interactor's own discriminant and a param's Selection
  #    strategy; `type` is also vg_data()'s spatial-source `type`) -- see
  #    the `known_non_interactor_selects` comment above. A real
  #    vg_mark()/vg_interactor() call never routes either through `...`
  #    the way it would need to for this check to apply to them.
  #  - a `$name` string is mosaic-spec's own serialized form of a param()
  #    reference -- at the R level, warn_unrecognized_enum_values() sees
  #    the actual vg_param object (and skips it) *before* serialization
  #    ever turns it into this string.
  #  - a bare column-reference shorthand (e.g. `symbol: species`, meaning
  #    "vary this channel by the species column") serializes identically
  #    to a literal enum string for one of the ~306 enum properties that
  #    are really a ChannelValueSpec (mark.R's args_reference_data()
  #    finding) -- at the R level this is a formula (`~species`), also
  #    skipped before serialization, but indistinguishable from a literal
  #    once it's already a plain YAML/JSON string.
  ambiguous_keys <- c("select", "type")
  bad <- character(0)
  for (path in fixture_files) {
    spec <- parse_spec_string(paste(readLines(path, warn = FALSE), collapse = "\n"))
    for (kv in collect_enum_kv(spec)) {
      if (kv$name %in% ambiguous_keys || startsWith(kv$value, "$")) next
      if (!(kv$value %in% .vg_enum_props[[kv$name]])) {
        bad <- c(bad, sprintf("%s: %s = \"%s\"", basename(path), kv$name, kv$value))
      }
    }
  }
  # symbols.yaml's `symbol: species` is a real, known instance of the
  # column-reference-shorthand ambiguity above (Species is a real column
  # in that example's data, not a SymbolType literal) -- excluded by name
  # rather than lumped into ambiguous_keys, since `symbol` is a completely
  # unambiguous literal enum property everywhere else it appears.
  bad <- setdiff(bad, 'symbols.yaml: symbol = "species"')
  expect_true(
    length(bad) == 0,
    info = paste(
      "Value(s) mosaic's own examples use but .vg_enum_props doesn't",
      "recognize (a false-positive risk for warn_unrecognized_enum_values()",
      "-- rerun data-raw/update-schema.R, or check for a genuine schema",
      "gap):", paste(bad, collapse = "; ")
    )
  )
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
