copy_project_files <- function(original_repo_path, new_repo_path) {
  # Ensure the new repository directory exists; if not, create it
  if (!dir.exists(new_repo_path)) {
    dir.create(new_repo_path, recursive = TRUE, showWarnings = TRUE)
  }
  
  # Check if original_repo_path is a local directory. If not, you may need to clone/download it first.
  if (!dir.exists(original_repo_path)) {
    stop("The original_repo_path does not exist or is not accessible. Please make sure it's a local path.")
  }
  
  # Define the patterns for the file types we're interested in
  file_patterns <- c("\\.Rmd$", "\\.tex$", "\\.csl$", ".yml")  # add other file types if needed
  
  
  # Run the copy for each pattern
  for (pattern in file_patterns) {
    files_to_copy <- list.files(original_repo_path, pattern = pattern, full.names = TRUE, recursive = FALSE)
    
    for (file in files_to_copy) {
      new_file_path <- file.path(new_repo_path, basename(file))
      file.copy(file, new_file_path, overwrite = TRUE)
      message(paste0("Copy of ", file, " in ", new_file_path))
    }
    
  }
  
  # Message to show it's done
  message("Files have been copied to the new repository.")
}
