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
# This is dev-only tooling (fetches from GitHub) -- the vendored .yaml
# files it writes are the only thing the test suite itself reads, so
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
