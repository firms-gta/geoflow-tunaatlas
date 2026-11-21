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
# zen4R 0.10.6 : une erreur passagère de Zenodo (504 Gateway Timeout, 502, 503,
# 429, coupure réseau) fait planter tout le workflow, car la page HTML d'erreur
# est lue comme du JSON ("lexical error: invalid char in json text"). On
# relance la requête quelques fois, avec une attente croissante, avant
# d'abandonner. Attention : un POST qui a expiré côté passerelle a pu aboutir
# côté Zenodo ; la relance peut alors créer un brouillon en double.
if (requireNamespace("zen4R", quietly = TRUE)) {
  local({
    gen <- zen4R::ZenodoRequest
    if (!isTRUE(attr(gen$public_methods$execute, "gta_retry"))) {
      once <- gen$public_methods$execute
      retry <- function() {
        delays <- c(10, 30, 60, 120)
        transient_status <- c(429, 500, 502, 503, 504)
        transient_error <- "lexical error|Gateway|<html|[Tt]imeout|timed out|Could not resolve|Connection|SSL"
        for (attempt in seq_len(length(delays) + 1)) {
          err <- tryCatch({ self$execute_once(); NULL }, error = function(e) e)
          status <- suppressWarnings(as.integer(private$status))
          transient <- if (!is.null(err)) grepl(transient_error, conditionMessage(err))
                       else isTRUE(status %in% transient_status)
          if (!transient || attempt > length(delays)) {
            if (!is.null(err)) stop(err)
            return(invisible(NULL))
          }
          msg <- sprintf("Zenodo: transient error (%s), retry %d/%d in %ds",
                         if (!is.null(err)) substr(conditionMessage(err), 1, 80) else paste("HTTP", status),
                         attempt, length(delays), delays[attempt])
          cli::cli_alert_warning(msg)
          self$WARN(msg)
          Sys.sleep(delays[attempt])
        }
      }
      attr(retry, "gta_retry") <- TRUE
      set <- function(name, fun) {
        tryCatch(gen$set("public", name, fun, overwrite = TRUE),
                 error = function(e) { gen$unlock(); gen$set("public", name, fun, overwrite = TRUE) })
      }
      set("execute_once", once)
      set("execute", retry)
      message("[patch] zen4R::ZenodoRequest$execute : relance sur erreur passagère")
    }
  })
}
