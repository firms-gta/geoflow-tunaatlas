#' Tidy and Map Data for Harmonization and Mapping Processes
#'
#' This function prepares and maps data for harmonization and mapping processes based on predefined configurations. It involves a series of steps including setting options and configurations, sourcing scripts for pre-harmonization, handling negative or null values, standardizing units of measure, dealing with irregular areas, and mapping to CWP standard codelists. The function is designed to work within a larger framework that utilizes external configuration, action, and entity objects to manage the harmonization and mapping workflow.
#'
#' @param action A geoflow object containing options and configurations for actions.
#' @param entity An geoflow object containing the dataset to be processed, including resources like harmonized data paths.
#' @param config A geoflow object that includes software configurations and a logger for information logging.
#' @return Updates the entity object with the processed dataset and logs various steps of the process.
#' @importFrom readr read_csv
#' @importFrom dplyr mutate filter inner_join group_by summarise
#' @importFrom stringr str_detect
#' @export
#' @examples
#' Tidying_and_mapping_data(action, entity, config)

Tidying_and_mapping_data = function(action, entity, config) {
  # Set options and configurations
  harmonized <- entity$resources$harmonized
  output_name_dataset_mapped <- gsub("harmonized", "mapped", harmonized)
  library(RPostgreSQL)
  opts <- action$options
  con <- config$software$output$dbi
  options(encoding = "UTF-8")
  recap_each_step <- TRUE
  
  # Define step logger function
  stepLogger <- function(level, step, msg) {
    log_info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  
  # Source scripts from URLs
  CWP.dataset::write_options_to_csv(opts)
  # Processing: core function gta_tidy_raw_dataset() (R/tidy_raw_dataset.R)
  dataset <- gta_tidy_raw_dataset(harmonized, opts = opts, nominal = grepl("nominal", harmonized),
                                  recap_each_step = recap_each_step, data_dir = "data")
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(dataset$time_start), "%Y"), "-01-01"),
    paste0(format(max(dataset$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  log_info("temporal extent")
  
  entity$setTemporalExtent(dataset_temporal_extent)
  
  log_info("seting temporal extend")
  
  #@geoflow -> export as csv
  
  
  log_info(output_name_dataset_mapped)
  
  write.csv(dataset, output_name_dataset_mapped, row.names = FALSE)
  
  log_info("writted")
  
  entity$addResource("mapped", output_name_dataset_mapped)
  
  log_info("addingressource")
  gc()
  
}
