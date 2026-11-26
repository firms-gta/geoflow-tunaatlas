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
#' @export
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
#' @noRd
gta_read_csv_utf8 <- function(path) {
  old <- options(encoding = "UTF-8")
  on.exit(options(old), add = TRUE)
  utils::read.csv(path)
}

#' Evaluate an expression with the UTF-8 encoding option
#'
#' The pre-harmonisation scripts set `options(encoding = "UTF-8")` before
#' reading their files; the core functions read the same way, then restore the
#' option.
#'
#' @param expr Expression to evaluate, e.g. a `read.csv()` call.
#' @return The value of `expr`.
#' @noRd
gta_utf8 <- function(expr) {
  old <- options(encoding = "UTF-8")
  on.exit(options(old), add = TRUE)
  expr
}

#' Path of a dataset given as a data frame or as a file path
#'
#' For the core functions whose reading is done by a helper that takes a file
#' path: a data frame is written to a temporary CSV file, a path is returned as is.
#'
#' @param x A data frame, or the path to a file.
#' @return A file path.
#' @noRd
gta_path <- function(x) {
  if (is.data.frame(x)) {
    path <- tempfile(fileext = ".csv")
    utils::write.csv(x, path, row.names = FALSE)
    return(path)
  }
  if (!is.character(x) || length(x) != 1L || !file.exists(x)) {
    stop("Expected a data frame or the path of an existing file.", call. = FALSE)
  }
  x
}
