# Functions shared by data-raw/update-example-usage.R (which writes the
# generated files) and tests/testthat/test-example-usage.R (which checks
# they're current). Everything here works from the source tree alone -- it
# reads the example .qmd files, NAMESPACE and man/*.Rd, never the installed
# package.

# The index pages listing the examples; every other .qmd in
# vignettes/articles/ is an example. Examples recreated from Mosaic's own
# gallery are named mosaic-<name>.qmd; any other name is one of the "other"
# examples.
EXAMPLE_INDEXES <- c("mosaic-examples", "other-examples")

# A function used in at least this fraction of all examples gets a link to
# the examples' landing page(s) in its help page, not a link to every example
# (see example_links_lines()). 1 means only functions used in every example.
SUMMARIZE_AT <- 1

# Every example, as its .qmd file name without the extension (e.g.,
# "mosaic-maps-spatial-data-us-state-map") -- the same name pkgdown serves it
# under, as articles/<name>.html. The Mosaic examples come first.
list_examples <- function(articles_dir = "vignettes/articles") {
  names_ <- sub("\\.qmd$", "", sort(list.files(articles_dir, pattern = "\\.qmd$")))
  names_ <- setdiff(names_, EXAMPLE_INDEXES)
  is_mosaic <- startsWith(names_, "mosaic-")
  c(names_[is_mosaic], names_[!is_mosaic])
}

# The `title:` from a .qmd's YAML header.
example_title <- function(path) {
  lines <- readLines(path, warn = FALSE)
  fences <- which(lines == "---")
  if (length(fences) < 2 || fences[[1]] != 1) stop(path, " has no YAML header.", call. = FALSE)
  title <- yaml::yaml.load(paste(lines[2:(fences[[2]] - 1)], collapse = "\n"))$title
  if (!is.character(title) || length(title) != 1) stop(path, " has no `title:`.", call. = FALSE)
  title
}

# The code of every R chunk in a .qmd (```{r ...} through the closing ```).
example_code <- function(path) {
  lines <- readLines(path, warn = FALSE)
  in_chunk <- FALSE
  keep <- logical(length(lines))
  for (i in seq_along(lines)) {
    if (!in_chunk && grepl(r"(^\s*```+\s*\{r\b)", lines[[i]])) {
      in_chunk <- TRUE
    } else if (in_chunk && grepl(r"(^\s*```+\s*$)", lines[[i]])) {
      in_chunk <- FALSE
    } else if (in_chunk) {
      keep[[i]] <- TRUE
    }
  }
  lines[keep]
}

# Names of every function called in a .qmd's R chunks (including ones called
# inside a formula, e.g., `~ vg_centroid_x(geom)`, or as vgplotr::fn()).
example_calls <- function(path) {
  code <- example_code(path)
  if (!length(code)) return(character())
  exprs <- tryCatch(
    parse(text = code, keep.source = TRUE),
    error = function(e) stop(path, ": its R code doesn't parse: ", conditionMessage(e), call. = FALSE)
  )
  pd <- utils::getParseData(exprs)
  unique(pd$text[pd$token == "SYMBOL_FUNCTION_CALL"])
}

# Exported function names, from NAMESPACE.
exported_functions <- function(namespace = "NAMESPACE") {
  lines <- grep(r"(^export\()", readLines(namespace), value = TRUE)
  sort(sub(r"(^export\((.*)\)$)", r"(\1)", lines))
}

# The 0-1 function x example matrix: a row for every exported function, a
# column for every example (named as in list_examples()), 1 where the
# example calls the function.
build_example_usage <- function(articles_dir = "vignettes/articles", namespace = "NAMESPACE") {
  fns <- exported_functions(namespace)
  examples <- list_examples(articles_dir)
  usage <- matrix(
    0L,
    nrow = length(fns), ncol = length(examples),
    dimnames = list(`function` = fns, example = examples)
  )
  for (ex in examples) {
    called <- intersect(example_calls(file.path(articles_dir, paste0(ex, ".qmd"))), fns)
    usage[called, ex] <- 1L
  }
  usage
}

# Rd topic (file name without .Rd) of every exported function, from the
# \alias{} entries in man/.
function_topics <- function(fns, man_dir = "man") {
  db <- tools::Rd_db(dir = dirname(normalizePath(man_dir)))
  aliases <- lapply(db, function(rd) {
    is_alias <- vapply(rd, function(x) identical(attr(x, "Rd_tag"), "\\alias"), logical(1))
    unlist(lapply(rd[is_alias], as.character))
  })
  topic <- rep(sub("\\.Rd$", "", names(aliases)), lengths(aliases))
  names(topic) <- unlist(aliases)
  missing <- setdiff(fns, names(topic))
  if (length(missing)) {
    stop("No help topic for exported function(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  topic[fns]
}

# The \name{} of every help topic, keyed by topic (file name without .Rd). A
# NULL roxygen block needs an @name, and giving it the topic's own name lets
# roxygen merge it into that topic.
rd_topic_names <- function(man_dir = "man") {
  db <- tools::Rd_db(dir = dirname(normalizePath(man_dir)))
  names_ <- vapply(db, function(rd) {
    is_name <- vapply(rd, function(x) identical(attr(x, "Rd_tag"), "\\name"), logical(1))
    as.character(rd[is_name][[1]])
  }, character(1))
  names(names_) <- sub("\\.Rd$", "", names(db))
  names_
}

# The generated R/example-usage-generated.R: one roxygen block per help
# topic whose functions any example uses, adding a section of links to those
# examples (merged into the existing topic via @rdname).
#
# A function used in at least `summarize_at` (a fraction) of all examples
# gets a link to the examples' landing page(s) instead of a link to each one
# -- by default only functions used in every example, like vg_create().
example_links_lines <- function(usage, articles_dir = "vignettes/articles", man_dir = "man",
                                site_url = pkgdown_url(), summarize_at = SUMMARIZE_AT) {
  examples <- colnames(usage)
  titles <- vapply(
    examples,
    function(ex) example_title(file.path(articles_dir, paste0(ex, ".qmd"))),
    character(1)
  )
  links <- sprintf("[%s](%sarticles/%s.html)", titles, site_url, examples)
  names(links) <- examples

  # The landing page for each example: Mosaic's gallery or the others.
  index_of <- ifelse(startsWith(examples, "mosaic-"), "mosaic-examples", "other-examples")
  index_links <- vapply(EXAMPLE_INDEXES, function(index) {
    title <- example_title(file.path(articles_dir, paste0(index, ".qmd")))
    sprintf("[%s](%sarticles/%s.html)", title, site_url, index)
  }, character(1))

  # Links to each example that uses a function -- or, for a very common
  # function (see `summarize_at`), NULL, with summary() describing it instead.
  fn_links <- function(fn) {
    uses <- usage[fn, ] == 1
    if (sum(uses) < summarize_at * length(uses)) links[uses]
  }
  # How many examples use a (very common) function, and a pointer to the
  # landing page(s) of the ones that do.
  summary <- function(fn) {
    uses <- usage[fn, ] == 1
    how_many <- if (all(uses)) "every example" else sprintf("%d of the %d examples", sum(uses), length(uses))
    see <- paste0("the ", index_links[unique(index_of[uses])], collapse = " and ")
    list(how_many = how_many, see = see)
  }

  used <- rownames(usage)[rowSums(usage) > 0]
  topics <- function_topics(rownames(usage), man_dir)
  topic_names <- rd_topic_names(man_dir)
  out <- c(
    "# Generated by data-raw/update-example-usage.R from the example articles",
    "# in vignettes/articles/.",
    "# DO NOT EDIT BY HAND -- rerun that script instead.",
    "#",
    "# Adds a \"Used in examples\" section to each help topic, linking every",
    "# example that calls one of its functions. The same information is in the",
    "# vg_example_usage dataset.",
    ""
  )
  for (topic in sort(unique(topics[used]))) {
    fns <- intersect(names(topics)[topics == topic], used)
    body <- if (length(fns) == 1 && sum(topics == topic) == 1) {
      fl <- fn_links(fns)
      if (is.null(fl)) {
        sm <- summary(fns)
        paste0("#' Used in ", sm$how_many, " on the package website. See ", sm$see, ".")
      } else {
        c(
          "#' Examples on the package website that use this function:",
          "#'",
          paste0("#' * ", fl)
        )
      }
    } else {
      c(
        "#' Examples on the package website that use these functions:",
        "#'",
        vapply(fns, function(fn) {
          fl <- fn_links(fn)
          if (is.null(fl)) {
            sm <- summary(fn)
            paste0("#' * `", fn, "()`: ", sm$how_many, " (see ", sm$see, ")")
          } else {
            paste0("#' * `", fn, "()`: ", paste(fl, collapse = ", "))
          }
        }, character(1))
      )
    }
    out <- c(
      out,
      "#' @section Used in examples:",
      body,
      paste0("#' @name ", topic_names[[topic]]),
      paste0("#' @rdname ", topic),
      "NULL",
      ""
    )
  }
  unname(out)
}

# The site URL (with a trailing slash) from _pkgdown.yml.
pkgdown_url <- function(config = "_pkgdown.yml") {
  url <- yaml::read_yaml(config)$url
  if (!grepl("/$", url)) url <- paste0(url, "/")
  url
}
