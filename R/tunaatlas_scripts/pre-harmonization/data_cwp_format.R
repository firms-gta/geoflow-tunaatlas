#' Format pre-harmonized data to cwp format
#'
#' This function preprocesses catch data for harmonization, including renaming columns, converting data types,
#' and filtering records. It then exports the processed data along with associated code lists as CSV files.
#' Additionally, it updates the entity with temporal coverage information based on the dataset's date range.
#'
#' @param action An action placeholder, not directly used but reserved for future extensions or logging.
#' @param entity An object encapsulating dataset metadata and methods for managing data resources and metadata.
#' @param config A configuration object providing settings and utilities, such as a logger for information logging.
#'
#' @details
#' The function performs several steps to prepare the catch data for further processing or integration:
#' - Reads the dataset from a specified path.
#' - Optionally renames the 'fishingfleet' column to 'fishing_fleet' if it exists.
#' - Converts time fields to character format, then to Date, and numerically transforms the 'measurement_value'.
#' - Filters out records with a 'measurement_value' of zero.
#' - Selects specific columns for the final dataset.
#' - Calculates and sets the temporal extent of the dataset based on the time fields.
#' - Exports the harmonized dataset and code lists to CSV files, updating the entity with these resources.
#'
#' It relies on the 'dplyr' package for data manipulation and 'readr' for reading CSV files. The function assumes
#' that the 'entity' object provides methods for accessing data sources and setting metadata properties.
#'
#' @return Does not return a value but writes out CSV files and updates the 'entity' object.
#'
#' @examples
#' \dontrun{
#'   pre_harmonize_catch_data(action, entity, config)
#'   # Ensure 'action', 'entity', and 'config' are properly defined before running.
#' }
#'
#' @importFrom dplyr filter mutate rename select
#' @importFrom readr read_csv
#' @export


function(action, entity, config){
  
  require(dplyr)
  catches <- readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]), guess_max = 0)
# Historical name for the dataset at source  iccat_catch_all_1m_firms_level0_20230405.csv or iotc_catch_all_1m_firms_level0.csv

  filename1 <- entity$data$source[[1]]
# Historical name for the dataset at source  iccat_catch_all_1m_firms_level0_20230405.csv or iotc_catch_all_1m_firms_level0.csv
  filename2 <- entity$data$source[[2]]
# Historical name for the dataset at source  iccat_catch_code_lists.csv or iotc_catch_code_lists.csv
  path_to_raw_dataset <- entity$getJobDataResource(config, entity$data$source[[1]])
# Historical name for the dataset at source  iccat_catch_all_1m_firms_level0_20230405.csv or iotc_catch_all_1m_firms_level0.csv
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  #----------------------------------------------------------------------------------------------------------------------------
  
  ## Catches
  if("fishingfleet" %in%colnames(catches)){
  catches <- catches %>% dplyr::rename(fishing_fleet = fishingfleet) 
  }
  
  if(!"measurement_processing_level" %in% colnames(catches)){
    catches$measurement_processing_level <- "raised"
  }
  
  catches <- catches %>% dplyr::mutate( time_start = as.character(time_start), time_end = as.character(time_end),  geographic_identifier= as.character(geographic_identifier), measurement_value = as.numeric(catches$measurement_value))
  
  catches <- catches %>% filter(measurement_value!= 0)
  
  catches<-catches %>% dplyr::select("fishing_fleet","gear_type","time_start","time_end","geographic_identifier",
                                     "fishing_mode","species","measurement_type","measurement_unit","measurement_value", "source_authority", "measurement_processing_level") %>% 
    dplyr::mutate(geographic_identifier = as.character(geographic_identifier))
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
  catches$time_start <- as.Date(catches$time_start)
  catches$time_end <- as.Date(catches$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(catches$time_start), "%Y"), "-01-01"),
    paste0(format(max(catches$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #----------------------------------------------------------------------------------------------------------------------------
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(min(catches$time_start),max(catches$time_end),sep = "/")
  entity$setTemporalExtent(dataset_temporal_extent)
  
  base1 <- tools::file_path_sans_ext(basename(filename1))
  #@geoflow -> export as csv
  # sorties same folder as path_to_raw_dataset 
  output_name_dataset   <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_harmonized.csv"))
  output_name_codelists <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_codelists.csv"))
  
  write.csv(catches, output_name_dataset, row.names = FALSE)
  
  file.rename(
    from = entity$getJobDataResource(config, filename2),
    to   = output_name_codelists
  )
  #----------------------------------------------------------------------------------------------------------------------------
  entity$addResource("source", output_name_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  
  
}  

