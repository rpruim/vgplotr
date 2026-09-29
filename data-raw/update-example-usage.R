# Regenerates everything derived from which functions the example articles
# use:
#
# * data/vg_example_usage.rda -- the exported 0-1 function x example matrix.
# * R/example-usage-generated.R -- a "Used in examples" section, with links,
#   added to each help topic whose functions an example uses.
#
# The examples are the .qmd files in vignettes/articles/ other than the index
# pages (EXAMPLE_INDEXES in data-raw/example-usage.R); mosaic-*.qmd are the
# ones recreated from Mosaic's gallery. Run from the package root whenever an example is
# added, removed, renamed or retitled, or its code changes which functions it
# calls:
#
#   Rscript data-raw/update-example-usage.R
#
# tests/testthat/test-example-usage.R fails when these files are out of date.

source("data-raw/example-usage.R")

vg_example_usage <- build_example_usage()
save(vg_example_usage, file = "data/vg_example_usage.rda", compress = "xz", version = 2)
writeLines(example_links_lines(vg_example_usage), "R/example-usage-generated.R")

# Rebuild the Rd files so the new sections show up.
devtools::document()

cat(sprintf(
  "%d functions x %d examples; %d functions used by at least one example.\n",
  nrow(vg_example_usage), ncol(vg_example_usage), sum(rowSums(vg_example_usage) > 0)
))
