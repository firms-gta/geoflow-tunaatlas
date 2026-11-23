#' Write an information message to the log
#'
#' Same output as the log_info() of the workflow launcher when it has no
#' geoflow configuration: "[INFO] <message>". The arguments after the first
#' one are formatted into it with sprintf().
#'
#' @param msg The message, or a sprintf() format.
#' @param ... Values for the format.
#' @return NULL, invisibly.
log_info <- function(msg, ...) {
  msg <- if (...length() > 0) sprintf(msg, ...) else msg
  message("[INFO] ", msg)
  invisible(NULL)
}
