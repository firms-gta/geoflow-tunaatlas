#' Format pre-harmonized data to cwp format
#'
#' This function preprocesses catch data for harmonization, including renaming columns, converting data types,
#' and filtering records. It then exports the processed data along with associated code lists as CSV files.
#'
#' @details
#' The function performs several steps to prepare the catch data for further processing or integration:
#' - Reads the dataset from a specified path.
#' - Optionally renames the 'fishingfleet' column to 'fishing_fleet' if it exists.
#' - Converts time fields to character format, then to Date, and numerically transforms the 'measurement_value'.
#' - Filters out records with a 'measurement_value' of zero.
#' - Selects specific columns for the final dataset.
#' - Calculates and sets the temporal extent of the dataset based on the time fields.
#'
#' It relies on the 'dplyr' package for data manipulation and 'readr' for reading CSV files. The function assumes
#'
#'
#' @param data The dataset, already in CWP format (ICCAT, IOTC): a data frame, or
#'   the path to the CSV file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (cwp_format/action_cwp_format.R).
#' @export
gta_rawdata_cwp_format <- function(data) {
  catches <- gta_data(data, reader = function(path) readr::read_csv(path, guess_max = 0))
  #----------------------------------------------------------------------------------------------------------------------------
  
  ## Catches
  if("fishingfleet" %in%colnames(catches)){
  catches <- catches %>% dplyr::rename(fishing_fleet = fishingfleet) 
  }
  
  # IOTC gives the processing level in measurement_obs ("raised", "unknown",
  # "original sample"): same codes as measurement_processing_level, with a space
  if(!"measurement_processing_level" %in% colnames(catches) && "measurement_obs" %in% colnames(catches)){
    catches$measurement_processing_level <- gsub(" ", "_", catches$measurement_obs, fixed = TRUE)
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
  catches
}
