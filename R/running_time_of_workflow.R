running_time_of_workflow <- function(folder){
  # Get the last modified times of the files
  json_time <- file.info(file.path(folder, "job.json"))$mtime
  txt_time <- file.info(file.path(folder, "job-logs.txt"))$mtime
  
  # Calculate the difference
  time_difference <- txt_time - json_time
  
  return(time_difference)
}