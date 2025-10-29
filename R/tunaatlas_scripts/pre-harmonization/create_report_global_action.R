#' Execute a Predefined Processing Action on a Given Script
#'
#' This function automates the cleaning and report generation process for scripts associated with
#' the Global Tuna Atlas data pre-harmonization. It retrieves a specific script from a URL, cleans it,
#' generates a new report script, and then deletes the temporary report file.
#'
#' @param config A config geoflow object, not used in this function
#' @param entity An entity geoflow object containing script-related data, including path and script specific actions.
#' @param action An action geoflow parameter, not used in this function
#' 
#' @details The function begins by sourcing a remote cleaning script from a GitHub repository, which
#' clean the input script. This script is specific for pre-harmonisation function of the geoflow-tunaatlas, it may not work for other scripts.
#'  It then fetches the script path #' from the `entity` object, cleans the script using the sourced cleaning function, and prepares a new
#' filename for the output by appending "_Report" to the original script name. This cleaned script is
#' then saved temporarily, processed through `knitr::spin` to generate a report, and finally, the temporary
#' file is removed.
#'
#' @return The function does not return any value; it performs file I/O and script processing.
#'
create_report_global_action = function(config, entity, action){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/tunaatlas_scripts/pre-harmonization/clean_script_for_report.R")
  script_path <- entity$data$actions[[1]]$script
  cleaned_code <- clean_script(script_path)
  new_filename <- str_replace(basename(script_path), ".R", "_Report.R")
  writeLines(cleaned_code, new_filename)
  knitr::spin(new_filename,knit =FALSE, 
              doc = "^#\\s*|^#+'[ ]?",
              precious = TRUE)
  file.remove(new_filename)
}
