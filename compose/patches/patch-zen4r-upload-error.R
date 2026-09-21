# zen4R 0.10.6 : startFileUpload() plante en cas d'échec (`out$message` sur un logical).
# Remplace la méthode pour afficher le vrai message de Zenodo. Sans effet si déjà appliqué.
if (requireNamespace("zen4R", quietly = TRUE)) {
  local({
    gen <- zen4R::ZenodoManager
    if (!isTRUE(attr(gen$public_methods$startFileUpload, "gta_patched"))) {
      patched <- function(path, recordId) {
        self$INFO(sprintf("Start upload procedure for file '%s'", path))
        fileparts <- unlist(strsplit(path, "/"))
        filename <- fileparts[length(fileparts)]
        zenReq <- zen4R::ZenodoRequest$new(private$url, "POST",
                                           sprintf("records/%s/draft/files", recordId),
                                           data = list(list(key = filename)), accept = "application/json",
                                           token = self$getToken(), logger = self$loggerType)
        zenReq$execute()
        if (zenReq$getStatus() == 201) {
          infoMsg <- sprintf("Successfully started upload procedure for file '%s'", path)
          cli::cli_alert_success(infoMsg)
          self$INFO(infoMsg)
          return(TRUE)
        }
        detail <- paste(utils::capture.output(utils::str(zenReq$getResponse(), max.level = 3)), collapse = " ")
        errMsg <- sprintf("Error while starting upload procedure for file '%s' in record %s (HTTP %s): %s",
                          path, recordId, zenReq$getStatus(), detail)
        cli::cli_alert_danger(errMsg)
        self$ERROR(errMsg)
        FALSE
      }
      attr(patched, "gta_patched") <- TRUE
      tryCatch(gen$set("public", "startFileUpload", patched, overwrite = TRUE),
               error = function(e) { gen$unlock(); gen$set("public", "startFileUpload", patched, overwrite = TRUE) })
      message("[patch] zen4R::ZenodoManager$startFileUpload patché")
    }
  })
}