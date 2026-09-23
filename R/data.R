#' Build the URL for one of mosaic's own example data sets
#'
#' @param file The example data set's file name, as listed in
#'   [mosaic's own `data/` directory](https://github.com/uwdata/mosaic/tree/main/data).
#' @return The URL as a string.
#' @family spec functions
#' @export
#' @examples
#' vg_example_url('athletes.parquet')
#'
vg_example_url <- function(file) {
  paste0(
    "https://raw.githubusercontent.com/uwdata/mosaic/refs/heads/main/data/",
    file
  )
}
