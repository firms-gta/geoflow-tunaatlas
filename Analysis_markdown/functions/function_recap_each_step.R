#' Function to Record Step Details and Save Results
#'
#' This function records the details of each processing step, including explanations, function names, and options. It saves the results as RDS files and text files in a specified directory.
#'
#' @param step_name A character string specifying the name of the step.
#' @param rds_data A data frame containing the data to be saved as an RDS file.
#' @param explanation A character string providing an explanation of the step.
#' @param functions A character string listing the functions used in the step.
#' @param option_list A list of options used in the step.
#' 
#' @return None
#' @examples
#' \dontrun{
#' function_recap_each_step("step1", data, "This step does X", "function1, function2", list(option1 = "value1"))
#' }
#' @export
#' @author
#' Bastien Grasset

function_recap_each_step <- function(step_name, rds_data, explanation = "No explanation provided to this step", functions = "No function used in this step", option_list = NULL, entity = NULL) {
  # Check if global variables exist
  if (!exists("options_written_total")) {assign("options_written_total", "", envir = .GlobalEnv)}
  if (!exists("explanation_total")) {assign("explanation_total", "", envir = .GlobalEnv)}
  
  # Create directories if they do not exist
  dir.create("Markdown", showWarnings = FALSE)
  dir.create(file.path("Markdown", step_name), showWarnings = FALSE)
  
  # Set the directory name
  step_dir <- file.path("Markdown", step_name)
  
  rds_data <- rds_data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across(-measurement_value)) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), .groups = 'drop')
  
  # Ensure measurement_value is numeric
  rds_data$measurement_value <- as.numeric(rds_data$measurement_value)
  
  # Filter data based on measurement units
  rds_t <- rds_data %>% filter(measurement_unit %in% c("t", "MTNO", "MT"))
  rds_no <- rds_data %>% filter(measurement_unit %in% c("no", "NOMT", "NO"))
  
  # Calculate sums
  sum_t <- sum(rds_t$measurement_value, na.rm = TRUE)
  sum_no <- sum(rds_no$measurement_value, na.rm = TRUE)
  lines <- nrow(rds_data)
  
  # Save sums to CSV
  fwrite(data.frame(sum_t, sum_no, lines), file.path(step_dir, "sums.csv"))
  
  # Handle options
  if (!is.null(option_list) && length(option_list) != 0) {
    options_substi <- as.list(substitute(option_list))[-1]
    options_written <- ""
    for (i in 1:length(options_substi)) {
      options_written <- paste0(options_written, paste0(options_substi[i], " = ", option_list[[i]]), sep = " , \n ")
    }
  } else {
    options_written <- "NONE"
  }
  
  # Update global variables
  assign("options_written_total", paste0(options_written_total, options_written), envir = .GlobalEnv)
  assign("explanation_total", paste0(explanation_total, explanation), envir = .GlobalEnv)
  
  # Save RDS and text files
  qs::qsave(rds_data, file.path(step_dir, "data.qs"))
  write(explanation, file.path(step_dir, "explanation.txt"))
  write(explanation_total, file.path(step_dir, "explanation_total.txt"))
  write(functions, file.path(step_dir, "functions.txt"))
  write(options_written_total, "options_total.txt")
  write(options_written_total, file.path(step_dir, "options_total.txt"))
  write(options_written, file.path(step_dir, "options_written.txt"))
  
  if(!is.null(entity)){
    if(entity$provenance$statement != "The following processes are applied to the dataset:"){
      entity$provenance$setStatement("The following processes are applied to the dataset:")
      entity$provenance$processes <- NULL
    }
    rationale <- geoflow_process$new()
    rationale$rationale <- explanation
    entity$provenance$addProcess(rationale)
  }
}
