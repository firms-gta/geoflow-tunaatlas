#' Ensure GeoServer workspace and datastore exist before running the workflow
#'
#' Idempotent bootstrap: creates the GeoServer workspace and PostGIS datastore
#' if they don't already exist. Safe to call on every run.
#'
#' All arguments default to the corresponding environment variables already
#' used elsewhere in this project's docker-compose setup, so this function
#' needs no arguments in the normal case — just call ensure_geoserver_ready().
#'
#' @param base_url GeoServer REST base URL. Defaults to env var GEOSERVER_URL
#'   (e.g. "http://geoserver:8080/geoserver").
#' @param user GeoServer admin username. Defaults to env var GEOSERVER_USER.
#' @param pwd GeoServer admin password. Defaults to env var GEOSERVER_PASSWORD.
#' @param workspace GeoServer workspace name. Defaults to env var GEOSERVER_WORKSPACE.
#' @param store GeoServer datastore name. Defaults to env var GEOSERVER_STORE.
#' @param db_host PostgreSQL host, as seen from the GeoServer container. Defaults to env var DB_HOST.
#' @param db_port PostgreSQL port. Defaults to env var DB_PORT.
#' @param db_name PostgreSQL database name. Defaults to env var DB_NAME.
#' @param db_user PostgreSQL user. Defaults to env var DB_USER.
#' @param db_password PostgreSQL password. Defaults to env var DB_PASSWORD.
#' @param max_retries number of attempts before giving up (GeoServer may still
#'   be starting up). Default 10.
#' @param retry_delay seconds to wait between attempts. Default 5.
#'
#' @return invisible(TRUE) on success, invisible(FALSE) if it gave up after max_retries.
ensure_geoserver_ready <- function(
    base_url    = Sys.getenv("GEOSERVER_URL"),
    user        = Sys.getenv("GEOSERVER_USER"),
    pwd         = Sys.getenv("GEOSERVER_PASSWORD"),
    workspace   = Sys.getenv("GEOSERVER_WORKSPACE"),
    store       = Sys.getenv("GEOSERVER_STORE"),
    db_host     = Sys.getenv("DB_HOST"),
    db_port     = Sys.getenv("DB_PORT"),
    db_name     = Sys.getenv("DB_NAME"),
    db_user     = Sys.getenv("DB_USER"),
    db_password = Sys.getenv("DB_PASSWORD"),
    max_retries = 10,
    retry_delay = 5
){
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for ensure_geoserver_ready()")
  }
  
  required <- c(base_url = base_url, user = user, workspace = workspace, 
                store = store, db_host = db_host, db_port = db_port, 
                db_name = db_name, db_user = db_user)
  missing_vars <- names(required)[required == ""]
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "ensure_geoserver_ready(): missing required value(s): %s. Set the corresponding environment variable(s) or pass them explicitly.",
      paste(missing_vars, collapse = ", ")
    ))
  }
  
  auth <- httr::authenticate(user, pwd)
  
  do_attempt <- function(){
    # 1. Ensure workspace exists
    ws_check <- httr::GET(
      sprintf("%s/rest/workspaces/%s.xml", base_url, workspace), 
      auth
    )
    if (httr::status_code(ws_check) == 404) {
      message(sprintf("[geoserver-init] Creating workspace '%s'...", workspace))
      ws_resp <- httr::POST(
        sprintf("%s/rest/workspaces", base_url),
        auth,
        httr::content_type("text/xml"),
        body = sprintf("<workspace><name>%s</name></workspace>", workspace)
      )
      if (httr::status_code(ws_resp) >= 300) {
        stop(sprintf("Failed to create workspace: HTTP %s - %s", 
                     httr::status_code(ws_resp), httr::content(ws_resp, "text", encoding = "UTF-8")))
      }
      message("[geoserver-init] Workspace created.")
    } else if (httr::status_code(ws_check) == 200) {
      message(sprintf("[geoserver-init] Workspace '%s' already exists, skipping.", workspace))
    } else {
      stop(sprintf("Unexpected status checking workspace: HTTP %s", httr::status_code(ws_check)))
    }
    
    # 2. Ensure datastore exists
    ds_check <- httr::GET(
      sprintf("%s/rest/workspaces/%s/datastores/%s.xml", base_url, workspace, store), 
      auth
    )
    if (httr::status_code(ds_check) == 404) {
      message(sprintf("[geoserver-init] Creating datastore '%s'...", store))
      ds_body <- sprintf('<dataStore>
        <name>%s</name>
        <connectionParameters>
          <entry key="host">%s</entry>
          <entry key="port">%s</entry>
          <entry key="database">%s</entry>
          <entry key="user">%s</entry>
          <entry key="passwd">%s</entry>
          <entry key="dbtype">postgis</entry>
          <entry key="schema">public</entry>
          <entry key="validate connections">true</entry>
          <entry key="Expose primary keys">true</entry>
        </connectionParameters>
      </dataStore>', store, db_host, db_port, db_name, db_user, db_password)
      
      ds_resp <- httr::POST(
        sprintf("%s/rest/workspaces/%s/datastores", base_url, workspace),
        auth,
        httr::content_type("text/xml"),
        body = ds_body
      )
      if (httr::status_code(ds_resp) >= 300) {
        stop(sprintf("Failed to create datastore: HTTP %s - %s", 
                     httr::status_code(ds_resp), httr::content(ds_resp, "text", encoding = "UTF-8")))
      }
      message("[geoserver-init] Datastore created.")
    } else if (httr::status_code(ds_check) == 200) {
      message(sprintf("[geoserver-init] Datastore '%s' already exists, skipping.", store))
    } else {
      stop(sprintf("Unexpected status checking datastore: HTTP %s", httr::status_code(ds_check)))
    }
    
    invisible(TRUE)
  }
  
  for (i in seq_len(max_retries)) {
    result <- tryCatch(do_attempt(), error = function(e) {
      message(sprintf("[geoserver-init] Attempt %d/%d failed: %s", i, max_retries, conditionMessage(e)))
      FALSE
    })
    if (isTRUE(result)) return(invisible(TRUE))
    if (i < max_retries) Sys.sleep(retry_delay)
  }
  
  warning("[geoserver-init] Gave up after max_retries attempts. GeoServer workspace/datastore may be missing.")
  invisible(FALSE)
}