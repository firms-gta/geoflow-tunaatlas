# =============================================================================
# ZENODO INPUT HELPERS
# =============================================================================
#
# These functions are kept in a dependency-free file so the launcher interface
# can be tested without loading the complete scientific workflow.

get_zenodo_record_id <- function(doi) {
  value <- trimws(as.character(doi)[1])
  
  if (grepl("^[0-9]+$", value)) {
    return(value)
  }
  
  matched <- regmatches(
    value,
    regexec(
      "zenodo(?:\\.org)?(?:\\.|/records?/)([0-9]+)",
      value,
      ignore.case = TRUE
    )
  )[[1]]
  
  if (length(matched) == 2) {
    return(matched[2])
  }
  
  stop(
    "Unsupported DOI. Expected a Zenodo DOI, record URL, or numeric record ID: ",
    value,
    call. = FALSE
  )
}

select_zenodo_input_archive <- function(files, doi_file = NULL) {
  file_names <- vapply(files, function(x) x$key, character(1))
  
  if (!is.null(doi_file)) {
    match_index <- match(doi_file, file_names)
    if (is.na(match_index)) {
      stop(
        "Requested Zenodo file not found: ", doi_file, "\n",
        "Available files:\n  - ", paste(file_names, collapse = "\n  - "),
        call. = FALSE
      )
    }
    return(files[[match_index]])
  }
  
  preferred <- which(tolower(file_names) == "all_raw_data_gta.zip")
  if (length(preferred) == 1) {
    return(files[[preferred]])
  }
  
  archive_indexes <- grep(
    "raw.*data.*\\.(zip|tar\\.gz|tgz)$",
    file_names,
    ignore.case = TRUE
  )
  
  if (length(archive_indexes) == 1) {
    return(files[[archive_indexes]])
  }
  
  stop(
    "The Zenodo record does not contain one unambiguous raw-data archive.\n",
    "Set GTA_DOI_FILE to one of:\n  - ", paste(file_names, collapse = "\n  - "),
    call. = FALSE
  )
}