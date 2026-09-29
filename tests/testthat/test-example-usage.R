# data/vg_example_usage.rda and R/example-usage-generated.R are generated
# from the example articles by data-raw/update-example-usage.R. These tests
# fail when an example has changed since they were last regenerated. They
# need the source tree (the articles and data-raw/ aren't in the built
# package), so they're skipped under R CMD check.

pkg_root <- test_path("..", "..")
articles_dir <- file.path(pkg_root, "vignettes", "articles")
helpers <- file.path(pkg_root, "data-raw", "example-usage.R")

skip_if_not(dir.exists(articles_dir) && file.exists(helpers), "needs the source tree")
source(helpers, local = TRUE)

test_that("vg_example_usage is current -- if not, run data-raw/update-example-usage.R", {
  current <- build_example_usage(articles_dir, file.path(pkg_root, "NAMESPACE"))
  expect_identical(vg_example_usage, current)
})

test_that("R/example-usage-generated.R is current -- if not, run data-raw/update-example-usage.R", {
  current <- example_links_lines(
    vg_example_usage,
    articles_dir = articles_dir,
    man_dir = file.path(pkg_root, "man"),
    site_url = pkgdown_url(file.path(pkg_root, "_pkgdown.yml"))
  )
  expect_identical(readLines(file.path(pkg_root, "R", "example-usage-generated.R")), current)
})

test_that("every example has a title and R code that parses", {
  for (ex in colnames(vg_example_usage)) {
    path <- file.path(articles_dir, paste0(ex, ".qmd"))
    expect_type(example_title(path), "character")
    expect_no_error(example_calls(path))
  }
})

test_that("example_calls() finds calls in chunks, formulas and pkg::fn, not in prose", {
  qmd <- tempfile(fileext = ".qmd")
  on.exit(unlink(qmd))
  writeLines(c(
    "---", "title: \"T\"", "---",
    "Prose mentioning vg_hconcat() is not code.",
    "```{r}",
    "vg_create() |> vg_mark_dot(x = ~ vg_bin(a))",
    "vgplotr::vg_render(spec)",
    "```",
    "```{python}",
    "vg_vconcat()",
    "```"
  ), qmd)
  expect_setequal(example_calls(qmd), c("vg_create", "vg_mark_dot", "vg_bin", "vg_render"))
})

test_that("list_examples() skips the index pages and lists mosaic-* examples first", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  file.create(file.path(dir, c(
    "mosaic-examples.qmd", "other-examples.qmd",
    "mosaic-b.qmd", "a-other.qmd", "mosaic-a.qmd", "notes.txt"
  )))
  expect_identical(list_examples(dir), c("mosaic-a", "mosaic-b", "a-other"))
})
