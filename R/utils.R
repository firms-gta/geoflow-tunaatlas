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

#' Check that a database connection is given
#'
#' The gta functions run without database with the options used by the
#' workflow. A few optional branches still query the Tuna Atlas database
#' (code list mappings stored in it, spatial relations with other area code
#' lists, gear groups): they call this function first, so that running them
#' without connection stops with a clear message.
#'
#' @param con The connection, or NULL.
#' @param what What needs the database, for the message.
#' @return `con`, invisibly.
gta_require_con <- function(con, what) {
  if (is.null(con) || !inherits(con, "DBIConnection")) {
    stop(sprintf(paste0("%s needs a connection to the Tuna Atlas database (argument con). ",
                        "This branch is not used with the default options of the workflow."), what),
         call. = FALSE)
  }
  invisible(con)
}
