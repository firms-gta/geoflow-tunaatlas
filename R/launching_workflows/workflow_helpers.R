# Patch geoflow resource resolution so that job resources are retrieved from the
# local data directory. This is useful when the workflow JSON references files
# whose runtime location differs between local machines, Docker, and servers.
patch_geoflow_get_job_data_resource <- function() {
  gen <- geoflow::geoflow_entity
  
  gen$set(
    "public",
    "getJobDataResource",
    overwrite = TRUE,
    value = function(config, path) {
      path <- gsub("\\\\", "/", path)
      file.path(getwd(), "data", basename(path))
    }
  )
  
  invisible(TRUE)
}

patch_geoflow_get_job_data_resource()


# =============================================================================
# 2. INPUT DATA VALIDATION FOR PRE-HARMONISATION WORKFLOWS
# =============================================================================

# Extract source file names from CSV metadata files. The pre-harmonisation JSON
# files point to metadata CSVs, and those CSVs contain the actual source file
# references used by the workflow.
extract_sources_from_csv <- function(files) {
  unique(unlist(lapply(files, function(file) {
    txt <- readLines(file, warn = FALSE)
    source_lines <- grep("source:", txt, value = TRUE)
    unlist(strsplit(sub(".*source:", "", source_lines), ","))
  }))) |>
    trimws() |>
    sub("^\\./data/", "", x = _) |>
    sub("_$", "", x = _)
}

# Read one workflow JSON file and return all CSV entity sources declared in its
# metadata section.
get_entity_csv_from_json <- function(json_file) {
  config <- jsonlite::fromJSON(here::here(json_file), simplifyVector = FALSE)
  
  csv_entities <- Filter(
    function(entity) identical(entity$handler, "csv"),
    config$metadata$entities
  )
  
  unique(vapply(csv_entities, function(entity) entity$source, character(1)))
}

# Check that all raw files required by several pre-harmonisation workflows exist
# locally. This avoids discovering missing inputs after a long processing run.
check_files_from_json <- function(json_files, data_dir = "data", max_age_days = 365) {
  csv_files <- unique(unlist(lapply(json_files, get_entity_csv_from_json)))
  files_detected <- extract_sources_from_csv(csv_files)
  paths <- file.path(data_dir, files_detected)
  
  file_exists <- file.exists(paths)
  missing_files <- files_detected[!file_exists]
  
  if (length(missing_files)) {
    message("Missing files:")
    print(missing_files)
  } else {
    message("No missing files detected. The pre-harmonisation workflows can run.")
  }
  
  existing_paths <- paths[file_exists]
  if (length(existing_paths)) {
    info <- file.info(existing_paths)
    ages_days <- as.numeric(Sys.Date() - as.Date(info$mtime))
    old_files <- ages_days > max_age_days
    
    if (any(old_files)) {
      warning(
        "Files older than ", max_age_days, " days:\n",
        paste0(
          basename(existing_paths[old_files]),
          " (",
          as.Date(info$mtime[old_files]),
          ")",
          collapse = "\n"
        ),
        call. = FALSE
      )
    }
  }
  
  invisible(list(
    csv_files = csv_files,
    files_detected = files_detected,
    missing = missing_files
  ))
}

# =============================================================================
# 3. GENERIC DATABASE AND WORKFLOW UTILITIES
# =============================================================================

# Null-coalescing helper. This keeps list extraction readable when some geoflow
# objects do not contain the expected DBI connection.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Remove DBI-related software and upload options from a workflow JSON file. This
# allows the same workflow to run locally or in Docker when the database is not
# reachable, while keeping the original JSON file untouched.
remove_dbi_software <- function(file) {
  workflow <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  
  if (!is.null(workflow$software)) {
    workflow$software <- Filter(function(software) {
      id <- software$id %||% ""
      !(
        identical(software$software_type, "dbi") ||
          grepl("database", id, ignore.case = TRUE)
      )
    }, workflow$software)
  }
  
  if (!is.null(workflow$actions)) {
    for (i in seq_along(workflow$actions)) {
      if (!is.null(workflow$actions[[i]]$options)) {
        workflow$actions[[i]]$options$upload_to_db <- FALSE
        workflow$actions[[i]]$options$upload_to_db_public <- FALSE
        workflow$actions[[i]]$options$create_materialized_view <- FALSE
        workflow$actions[[i]]$options$add_sql_comments <- FALSE
      }
    }
  }
  
  output_file <- file.path(
    dirname(file),
    paste0(tools::file_path_sans_ext(basename(file)), "_nodb.json")
  )
  
  jsonlite::write_json(workflow, output_file, pretty = TRUE, auto_unbox = TRUE)
  output_file
}

# Return TRUE only in the expected GTA database context and only if the DBI
# connection is valid. This prevents accidental upload to another database or
# failed upload attempts when the local environment variables are incomplete.
should_upload_to_db <- function(config) {
  expected_context <- identical(Sys.getenv("DB_DRV"), "PostgreSQL") &&
    identical(Sys.getenv("DB_PORT"), "5432") &&
    identical(Sys.getenv("DB_HOST"), "db-tunaatlas.d4science.org") &&
    Sys.getenv("DB_NAME") %in% c("tunaatlas_sandbox", "tunaatlas") &&
    identical(Sys.getenv("DB_USER"), "tunaatlas_u")
  
  if (!expected_context) return(FALSE)
  
  con <- config$software$output$dbi
  if (is.null(con)) return(FALSE)
  
  tryCatch(
    DBI::dbIsValid(con) && DBI::dbGetQuery(con, "SELECT 1 AS ok")$ok[1] == 1,
    error = function(e) FALSE
  )
}

# Enable all actions required for database publication. This is called only after
# should_upload_to_db() has confirmed that the execution context is safe.
force_upload_to_db <- function(config, action_id = "load_dataset") {
  db_related_actions <- c(
    "enrich_metadata",
    "enrich_for_db_services",
    "load_metadata",
    "geometa-create-iso-19115",
    "geometa-create-iso-19110"
  )
  
  for (entity_index in seq_along(config$entities)) {
    actions <- config$entities[[entity_index]]$actions
    action_ids <- vapply(actions, `[[`, character(1), "id")
    
    # Main dataset loading action.
    for (action_index in which(action_ids == action_id)) {
      config$entities[[entity_index]]$actions[[action_index]]$options$upload_to_db <- TRUE
      config$entities[[entity_index]]$actions[[action_index]]$options$upload_to_db_public <- TRUE
      config$entities[[entity_index]]$actions[[action_index]]$options$create_materialized_view <- TRUE
      config$entities[[entity_index]]$actions[[action_index]]$options$add_sql_comments <- TRUE
      config$entities[[entity_index]]$actions[[action_index]]$options$upload_to_googledrive <- FALSE
    }
    
    # Metadata and database service actions.
    for (action_index in which(action_ids %in% db_related_actions)) {
      config$entities[[entity_index]]$actions[[action_index]]$run <- TRUE
    }
  }
  
  config
}

# Initialise a workflow. If DBI initialisation fails, write a temporary _nodb JSON
# file and retry without database software blocks.
init_workflow_maybe_without_dbi <- function(file, handle_metadata = TRUE) {
  tryCatch(
    initWorkflow(file, handleMetadata = handle_metadata),
    error = function(e) {
      message("initWorkflow failed. Retrying without DBI software: ", e$message)
      initWorkflow(remove_dbi_software(file), handleMetadata = handle_metadata)
    }
  )
}

# Execute a workflow and optionally rename the output job folder. Upload is
# activated dynamically only when the database context is explicitly validated.
execute_workflow_maybe_upload <- function(file, dir = ".", rename_suffix = NULL) {
  config_test <- try(
    initWorkflow(file, handleMetadata = FALSE),
    silent = TRUE
  )
  
  workflow_file <- file
  
  if (inherits(config_test, "try-error")) {
    message("initWorkflow failed. Retrying execution without DBI software.")
    workflow_file <- remove_dbi_software(file)
  } else {
    unlink(config_test$job, recursive = TRUE)
  }
  
  output <- executeWorkflow(
    file = workflow_file,
    dir = dir,
    on_initWorkflow = function(config, queue) {
      if (should_upload_to_db(config)) {
        config$logger.info("Database reachable and context validated: enabling upload.")
        force_upload_to_db(config, "load_dataset")
      } else {
        config$logger.info("Database unavailable or context not validated: upload disabled.")
      }
    }
  )
  
  if (!is.null(rename_suffix)) {
    output <- executeAndRename(output, rename_suffix)
  }
  
  output
}

# Initialise a workflow only to retrieve its DBI connection. NULL is returned when
# the workflow cannot be initialised with DBI, which lets summaries run without DB.
get_workflow_con <- function(config_file) {
  config <- try(
    initWorkflow(here::here(config_file), handleMetadata = FALSE),
    silent = TRUE
  )
  
  if (inherits(config, "try-error")) {
    message("DB unavailable: returning NULL connection.")
    return(NULL)
  }
  
  on.exit(unlink(config$job, recursive = TRUE), add = TRUE)
  config$software$output$dbi %||% NULL
}

# Wrapper around the invalid-data summary. Kept as a small helper so the same
# options are reused consistently across pre-harmonisation outputs.
summarise_invalid <- function(path, con = NULL) {
  CWP.dataset::summarising_invalid_data(
    path,
    connectionDB = con,
    upload_DB = FALSE,
    upload_drive = FALSE
  )
}

# Run a short summary report for one processed dataset. This avoids repeating the
# same init / connection / cleanup / summarising_step code for each level.
run_step_summary <- function(workflow_file,
                             workflow_output,
                             source_authority = "all",
                             sizepdf = "short",
                             savestep = FALSE,
                             usesave = FALSE,
                             nameoutput = NULL,
                             parameter_colnames_to_keep_fact = NULL) {
  gc()
  
  config <- init_workflow_maybe_without_dbi(
    here::here(workflow_file),
    handle_metadata = TRUE
  )
  
  on.exit(unlink(config$job, recursive = TRUE), add = TRUE)
  
  con <- config$software$output$dbi %||% NULL
  
  args <- list(
    main_dir = workflow_output,
    connectionDB = con,
    config = config,
    sizepdf = sizepdf,
    savestep = savestep,
    usesave = usesave,
    source_authoritylist = source_authority
  )
  
  if (!is.null(nameoutput)) {
    args$nameoutput <- nameoutput
  }
  
  if (!is.null(parameter_colnames_to_keep_fact)) {
    args$parameter_colnames_to_keep_fact <- parameter_colnames_to_keep_fact
  }
  
  do.call(CWP.dataset::summarising_step, args)
  
  invisible(list(config = config, connection = con))
}
