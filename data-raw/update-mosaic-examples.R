# Vendors mosaic's own published example specs (docs/public/specs/yaml in
# uwdata/mosaic) into tests/testthat/fixtures/mosaic-examples/, pinned to
# the same MOSAIC_VERSION as the rest of the schema-driven generation (see
# data-raw/update-schema.R) -- this is what
# tests/testthat/test-mosaic-examples.R checks against, to catch vgplotr's
# generated mark/interactor vocabulary falling behind what mosaic's own
# real-world examples actually use.
#
# Run interactively (source this file, or `Rscript data-raw/update-mosaic-examples.R`)
# whenever MOSAIC_VERSION is bumped -- keep it in sync with the constant of
# the same name in data-raw/update-schema.R and data-raw/js/build.js.
#
# It also vendors the JSON version of each spec that mosaic publishes
# alongside the YAML (docs/public/specs/json) into
# tests/testthat/fixtures/mosaic-examples-json/. mosaic's own tooling
# produced those, so they're an independent statement of what each YAML
# spec *means* -- tests/testthat/test-mosaic-examples.R checks that
# parse_spec_string() reads every YAML example into the same structure as
# its JSON twin, which is what catches a YAML 1.1-vs-1.2 difference (an
# unquoted `y` read as a boolean, `data: [0]` collapsing to `0`, `9.75e5`
# staying a string). (Verified separately that these JSON files match an
# independent `yaml`@2 parse of the YAML exactly, all 55.)
#
# This is dev-only tooling (fetches from GitHub) -- the vendored .yaml and
# .json files it writes are the only things the test suite itself reads, so
# running the tests never needs network access.

MOSAIC_VERSION <- "0.31.0"

out_dir <- "tests/testthat/fixtures/mosaic-examples"
unlink(out_dir, recursive = TRUE)
dir.create(out_dir, recursive = TRUE)

listing_url <- sprintf(
  "https://api.github.com/repos/uwdata/mosaic/contents/docs/public/specs/yaml?ref=v%s",
  MOSAIC_VERSION
)
listing <- jsonlite::fromJSON(listing_url, simplifyVector = FALSE)
names_ <- vapply(listing, function(x) x$name, character(1))
names_ <- sort(names_[grepl("\\.yaml$", names_)])

for (nm in names_) {
  raw_url <- sprintf(
    "https://raw.githubusercontent.com/uwdata/mosaic/v%s/docs/public/specs/yaml/%s",
    MOSAIC_VERSION, nm
  )
  utils::download.file(raw_url, file.path(out_dir, nm), quiet = TRUE, mode = "wb")
}

cat("Vendored", length(names_), "example specs (mosaic v", MOSAIC_VERSION, ") into", out_dir, "\n")

json_dir <- "tests/testthat/fixtures/mosaic-examples-json"
unlink(json_dir, recursive = TRUE)
dir.create(json_dir, recursive = TRUE)

for (nm in sub("\\.yaml$", ".json", names_)) {
  raw_url <- sprintf(
    "https://raw.githubusercontent.com/uwdata/mosaic/v%s/docs/public/specs/json/%s",
    MOSAIC_VERSION, nm
  )
  utils::download.file(raw_url, file.path(json_dir, nm), quiet = TRUE, mode = "wb")
}

cat("Vendored", length(names_), "JSON twins into", json_dir, "\n")
