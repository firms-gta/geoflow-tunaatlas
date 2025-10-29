executeAndRename <- function(executed_file, suffix) {
  
  # Derive folder and file names
  folder_file <- file.path("jobs", basename(executed_file))
  
  # Rename the file with the given suffix
  file.rename(folder_file, paste0("jobs/", basename(executed_file), suffix))
  return(paste0("jobs/", basename(executed_file), suffix))
}