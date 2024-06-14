#' Process Directories and Organize Files for DOI
#'
#' This function processes directories under a given root directory, organizing files into 
#' `data` and `metadata` subdirectories, and includes special handling for specific file types.
#'
#' @param root_dir The root directory containing the entities to process.
#' @param output_dir The directory where processed files will be stored.
#' @param replace_firms_with_ird Logical, if TRUE, replaces occurrences of "firms" with "ird" in filenames.
#' @return None
#' @examples
#' process_entities_for_DOI("~/firms-gta/geoflow-tunaatlas/jobs/20240612081508/entities", "~/firms-gta/geoflow-tunaatlas/jobs/processed_entities", TRUE)
#' @export
process_entities_for_DOI <- function(root_dir, output_dir, replace_firms_with_ird = FALSE) {
  library(dplyr)
  library(lubridate)
  library(readr)
  library(rlist)
  library(logging)
  
  # Initialize logging
  basicConfig()
  addHandler(writeToFile, file = "process_log.txt")
  
  # Function to get the creation time of the first file in the last subdirectory in the Markdown directory
  get_last_subfolder_first_file_time <- function(markdown_dir) {
    loginfo("Getting the creation time of the first file in the last subdirectory of %s", markdown_dir)
    subdirs <- list.dirs(markdown_dir, recursive = FALSE, full.names = TRUE)
    if (length(subdirs) == 0) {
      logwarn("No subdirectories found in %s", markdown_dir)
      return(NA)
    }
    last_subdir <- subdirs[length(subdirs)]
    files <- list.files(last_subdir, full.names = TRUE, recursive = TRUE)
    if (length(files) == 0) {
      logwarn("No files found in the last subdirectory %s", last_subdir)
      return(NA)
    }
    file_info <- file.info(files)
    return(min(file_info$ctime))
  }
  
  # Function to copy files to a new directory
  copy_files <- function(src, dest) {
    loginfo("Copying file %s to %s", src, dest)
    file.copy(src, dest, overwrite = TRUE)
  }
  
  # Function to convert CSV to RDS
  convert_csv_to_rds <- function(csv_file) {
    loginfo("Converting CSV %s to RDS", csv_file)
    data <- read_csv(csv_file)
    rds_file <- sub("\\.csv$", ".rds", csv_file)
    saveRDS(data, rds_file)
  }
  
  # Check if a filename starts with a datetime pattern
  is_datetime_prefixed <- function(filename) {
    grepl("^\\d{4}-\\d{2}-\\d{2}_\\d{2}:\\d{2}:\\d{2}", basename(filename))
  }
  
  loginfo("Starting to process directories in %s", root_dir)
  dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
  for (dir in dirs) {
    markdown_dir <- file.path(dir, "Markdown")
    if (dir.exists(markdown_dir)) {
      loginfo("Processing directory %s", dir)
      last_file_time <- get_last_subfolder_first_file_time(markdown_dir)
      if (is.na(last_file_time)) {
        logwarn("Skipping directory %s due to missing last file time", dir)
        next
      }
      metadata_dir <- file.path(dir, "metadata")
      data_dir <- file.path(dir, "data")
      files_to_copy <- list()
      
      # Collect metadata files
      if (dir.exists(metadata_dir)) {
        loginfo("Collecting metadata files from %s", metadata_dir)
        metadata_files <- list.files(metadata_dir, full.names = TRUE)
        files_to_copy <- append(files_to_copy, metadata_files)
      }
      
      # Collect relevant data files
      if (dir.exists(data_dir)) {
        loginfo("Collecting relevant data files from %s", data_dir)
        data_files <- list.files(data_dir, full.names = TRUE)
        for (file in data_files) {
          file_info <- file.info(file)
          if (file_info$ctime > last_file_time && !is_datetime_prefixed(file) && basename(file) != "rawdata.rds") {
            if (grepl("_public\\.", file)) {
              loginfo("Removing public file %s", file)
              next
            }
            if (grepl("\\.sql$", file)) {
              loginfo("Skipping SQL file %s", file)
              next
            }
            loginfo("Adding file %s to copy list", file)
            files_to_copy <- append(files_to_copy, file)
            if (grepl("\\.csv$", file) && grepl("_harmonized\\.csv$", file)) {
              convert_csv_to_rds(file)
              files_to_copy <- append(files_to_copy, sub("\\.csv$", ".rds", file))
            }
          }
        }
      }
      
      # Collect recap HTML file
      recap_file <- file.path(dir, "tableau_recap_global_action_effort.html")
      if (file.exists(recap_file)) {
        loginfo("Found recap file %s", recap_file)
        files_to_copy <- append(files_to_copy, recap_file)
      }
      
      # Create output directories
      entity_name <- basename(dir)
      if (replace_firms_with_ird) {
        entity_name <- gsub("firms", "ird", entity_name)
      }
      entity_output_dir <- file.path(output_dir, entity_name)
      data_output_dir <- file.path(entity_output_dir, "data")
      metadata_output_dir <- file.path(entity_output_dir, "metadata")
      loginfo("Creating output directories %s and %s", data_output_dir, metadata_output_dir)
      dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(metadata_output_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Copy files to appropriate directories
      for (file in files_to_copy) {
        dest_file <- basename(file)
        if (replace_firms_with_ird) {
          dest_file <- gsub("firms", "ird", dest_file)
        }
        if (grepl("_harmonized\\.(csv|rds|nc)$", file)) {
          copy_files(file, file.path(data_output_dir, dest_file))
        } else if (grepl("\\.xml$|\\.html$", file) || basename(file) == "Recap_of_the_process_on_the_data.html") {
          copy_files(file, file.path(metadata_output_dir, dest_file))
        } else if (basename(file) == "tableau_recap_global_action_effort.html") {
          copy_files(file, file.path(metadata_output_dir, "Recap_of_the_process_on_the_data.html"))
        } else {
          copy_files(file, file.path(metadata_output_dir, dest_file))
        }
      }
    } else {
      loginfo("Skipping directory %s as it does not contain a Markdown folder", dir)
    }
  }
  loginfo("Finished processing directories")
}

