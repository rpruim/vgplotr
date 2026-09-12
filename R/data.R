#' Build URL for fetching uwdata/mosaic data sets
#'
#' @param file file name
#' @return The URL as a string
#' @export
#' @examples
#' vg_data_url('atheletes.csv')
#'
vg_data_url <- function(file) {
  paste0(
    "https://raw.githubusercontent.com/uwdata/mosaic/refs/heads/main/data/",
    file
  )
}
