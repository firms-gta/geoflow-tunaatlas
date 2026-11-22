#' Use a dataset given as a data frame or as a file path
#'
#' The core functions of the Global Tuna Atlas take their inputs either as data
#' frames already in memory or as paths to the files: this helper returns the
#' data frame in both cases.
#'
#' @param x A data frame, or the path to a file.
#' @param reader Function reading the file when `x` is a path, so that each
#'   dataset keeps its own reading options (separator, sheet, encoding...).
#' @return A data frame.
gta_data <- function(x, reader = utils::read.csv) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (!is.character(x) || length(x) != 1L || !file.exists(x)) {
    stop("Expected a data frame or the path of an existing file.", call. = FALSE)
  }
  reader(x)
}

#' Read a CSV file the way the pre-harmonisation scripts always did
#'
#' `utils::read.csv()` with the UTF-8 encoding, restored afterwards.
#'
#' @param path Path to the CSV file.
#' @return A data frame.
gta_read_csv_utf8 <- function(path) {
  old <- options(encoding = "UTF-8")
  on.exit(options(old), add = TRUE)
  utils::read.csv(path)
}
