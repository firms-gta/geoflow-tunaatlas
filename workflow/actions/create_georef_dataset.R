#' Create Your Own Tuna Atlas Dataset
#'
#' This function allows users to create their own georeferenced Tuna Atlas dataset of catch or effort.
#' It takes as input the public domain datasets of the five Tuna Regional Fisheries Management Organizations (tRFMOs)
#' (IOTC, ICCAT, WCPFC, IATTC, CCSBT) stored within the Tuna atlas database. Users can customize the computation
#' of the tuna atlas using a set of parameters.
#'
#' @param action An action object containing options for dataset processing.
#' @param entity A geoflow entity object with dataset identifiers and metadata.
#' @param config A geoflow configuration object with software settings.
#' @return The status of the dataset creation process.
#' @examples
#' \dontrun{
#' # Example usage:
#' action <- list(options = list(fact = "catch", dataset_level = 1))
#' entity <- list(identifiers = list(id = "example_id"))
#' config <- list(software = list(output = list(dbi = "some_dbi_connection")),
#'                logger = list(info = function(msg) cat(msg, "\n")))
#' create_own_tuna_atlas_catch_effort(action, entity, config)
#' }
#' @export
#' 
create_georef_dataset <- function(action, entity, config) {
  
  # Initialisation ----------------------------------------------------------
  
  opts <- action$options
  con <- config$software$output$dbi
  options(encoding = "UTF-8")
  CWP.dataset::write_options_to_csv(opts)
  # List of required packages
  packages <- c("dplyr" ,"sf", "stringr", "reshape2", "readr", "tools", "RPostgreSQL", "DBI", "googledrive")
  
  # Function to check and install missing packages
  install_and_load <- function(package) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      require(package, character.only = TRUE)
    }
  }
  # Apply the function to each package
  sapply(packages, install_and_load)
  
  stepnumber <- 1
  #scripts
  url_scripts_create_own_tuna_atlas <- here::here("R/tunaatlas_scripts/generation")
  
  # Retrieve data from zenodo 
  
  #for level 0 - FIRMS
  source(here::here("R/tunaatlas_scripts/generation/get_rfmos_datasets_level0.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R")) #modified for geoflow
  #for filtering if needed
  
  # Process and aggregate final data 
  source(here::here("R/tunaatlas_scripts/generation/process_and_aggregate_dataset.R"))
  
  
  #for level 1 - FIRMS (candidate)
  
  
  #for level 2 - IRD
  
  # For filtering/aggregating
  
  #stepLog
  stepLogger = function(level, step, msg){
    log_info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  
  # Saving options in a csv file and creating a new variable for each options
  
  # Processing: core function gta_georef_dataset() (R/georef_dataset.R), on the
  # files of the entity (data/ folder of the job)
  # (the nominal catch is not a georeferenced dataset: excluded by its name,
  # whatever its position among the sources)
  files <- basename(unlist(entity$data$source))
  files <- files[!grepl("nominal", files)]
  res <- gta_georef_dataset(files, opts, con = con, entity = entity, data_dir = "data")
  georef_dataset <- res$georef_dataset
  opts <- res$opts
  recap_each_step <- if (!is.null(opts$recap_each_step)) opts$recap_each_step else TRUE
  rm(res)

  process_and_aggregate_dataset(georef_dataset, entity, config, opts, 
                                columns_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode",
                                                    "time_start", "time_end", "year", "month", "quarter", "geographic_identifier", "measurement_unit", "measurement_value", "measurement_type",
                                                    "measurement_processing_level", "measurement") ) 
  log_info("End: Your tuna atlas dataset has been created!")
  
  # Clean up
  rm(georef_dataset)
  gc()
  
  
  
}
