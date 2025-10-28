#' Copy Selected Directory
#'
#' This function copies a selected subdirectory from an ancient directory to a new directory. 
#' The user is prompted to select which subdirectory to copy if multiple matches are found.
#'
#' @param ancient_dir A string representing the path of the ancient directory where the search begins.
#' @param entity_name A string representing the name of the entity to match the subdirectories.
#' @param new_dir A string representing the path of the new directory where the selected subdirectory will be copied.
#'
#' @return NULL if no directory is selected or copied, otherwise it returns the copied directory path.
#' 
#' @details 
#' The function searches for subdirectories in the specified ancient directory that match the entity name.
#' If one or more matching directories are found, the user is prompted to select which one to copy.
#' If no matching directories are found, the function prints a message and returns NULL.
#' If the user does not select a directory within 30 seconds, the first matching directory is selected by default.
#' The selected directory is then copied to the new directory in an ordered manner based on file modification time.
#'
#' @examples
#' \dontrun{
#' copy_selected_directory("path/to/ancient_dir", "entity_name", "path/to/new_dir")
#' }
#'
#' @export
copy_selected_directory <- function(ancient_dir, entity_name, new_dir) {
  all_dirs <- list.dirs(path = ancient_dir, full.names = TRUE, recursive = TRUE)
  matching_dirs <- all_dirs[grep(sprintf("/%s$", entity_name), all_dirs)]
  matching_dirs_same_opts <- c()
  for (i in matching_dirs){
    hash1 <- digest(file = file.path(matching_dirs, "list_options.csv"), algo = "md5")
    hash2 <- digest(file = file.path(new_dir, "list_options.csv"), algo = "md5")
    if(hash1 == hash2){
      matching_dirs_same_opts <- matching_dirs[i]
    }
  }
  
  if (length(matching_dirs_same_opts) == 0) {
    cat("No directories matching the entity name were found.\n")
    return(NULL)
  }
  lines <- ""
  for (i in seq_along(matching_dirs_same_opts)) {
    lines <- paste0(lines,cat(sprintf("[%d] %s\n", i, matching_dirs_same_opts[i])))
  }
  cat("Please select the directory to copy by entering its number:\n")
  
  start_time <- proc.time()
  repeat {
    answer <- as.integer(readline(prompt = paste0(lines, "\n Enter the number of the directory to copy (e.g., 1, 2, 3):\n Enter 0 if you do not want to use cache and begin from scratch ")))
    elapsed_time <- proc.time() - start_time
    
    if (!is.na(answer) && answer %in% c(0, seq_along(matching_dirs_same_opts))) {
      break
    } else if (elapsed_time[3] >= 30) { # Check if 30 seconds have passed
      cat("Timeout reached. Proceeding as if 1 was entered.\n")
      answer <- 1
      break
    } else {
      cat("Invalid input. Proceeding without using cache.\n")
      answer <- 0
    }
  }
  
  if (answer == 0) {
    cat("No directory selected. Proceeding without using cache.\n")
    return(NULL)
  } else {
    # Proceed with copying the selected directory
    selected_dir <- matching_dirs_same_opts[answer]
    if (!dir.exists(new_dir)) {
      dir.create(new_dir, recursive = TRUE)
    }
    
    # Get the list of all files in the selected directory, recursively
    files <- list.files(selected_dir, recursive = TRUE, full.names = TRUE)
    
    # Order files by modification time
    files_ordered <- files[order(file.info(files)$mtime)]
    
    # Copy the files in the ordered manner
    for (file_path in files_ordered) {
      # Determine the relative path
      relative_path <- gsub(paste0(escape(selected_dir), "/"), "", file_path)
      dest_path <- file.path(new_dir, relative_path)
      
      # Ensure the destination directory exists
      if (!dir.exists(dirname(dest_path))) {
        dir.create(dirname(dest_path), recursive = TRUE)
      }
      
      # Copy the file
      file.copy(file_path, dest_path)
    }
    config$logger.info(sprintf("Directory '%s' copied to '%s'.\n", selected_dir, new_dir))
    cat(sprintf("Directory '%s' copied to '%s'.\n", selected_dir, new_dir))
  }
}
