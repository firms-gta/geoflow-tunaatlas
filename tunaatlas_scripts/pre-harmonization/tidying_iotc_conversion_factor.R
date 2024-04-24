#' Tidy and Convert IOTC Data with Conversion Factors
#'
#' This function takes initial IOTC (Indian Ocean Tuna Commission) data, reads it into a dataframe, 
#' and performs several tidying and conversion operations. Specifically, it calculates conversion factors
#' from average weights, adjusts unit measurements, assigns geographic identifiers, and defines start and end
#' times for the data. The final dataset is then saved as a harmonized CSV file. Additionally, the entity is 
#' enriched with temporal coverage information.
#'
#' @param action An action placeholder, not directly used in the function but may signify the action context in which the function is called.
#' @param entity An object containing methods and properties for dataset metadata, including methods to access job data resources and to set temporal extent.
#' @param config A configuration object, potentially containing settings and utilities, such as a logger or access credentials.
#'
#' @details 
#' The function assumes `entity` and `config` to be well-defined objects with specific properties and methods:
#' - `entity$getJobDataResource` to obtain the path to dataset sources.
#' - `entity$setTemporalExtent` to set the dataset's temporal coverage based on provided data.
#' - `entity$addResource` to update the entity with references to generated resources.
#' The function uses `readr` for reading CSV data, `dplyr` for data manipulation, and `lubridate` for date operations.
#' Data transformations include unit conversion from individual counts to tonnage based on average weights (`AVG_WEIGHT`),
#' reassignment of column names to more generic terms, and calculation of start and end dates for each record.
#'
#' @return The function does not return a value but writes the harmonized dataset to a CSV file and updates the `entity` object
#'         with new resources and temporal extent information.
#'
#' @examples
#' \dontrun{
#'   tidying_iotc_conversion_factor(action, entity, config)
#'   action, entity and config are object created with geoflow package
#'   # Where `action` is your action context, `entity` is your entity object with data source and metadata handling methods,
#'   # and `config` is your configuration object with necessary settings.
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select filter rename
#' @importFrom lubridate as_date ceiling_date
#' @export

tidying_iotc_conversion_factor <- function(action, entity, config){
  

  
    iotc_data_initial <- as.data.frame(readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]), guess_max = 0, 
                                                       col_types = cols(AVG_WEIGHT = col_double())))
    
    iotc_data_final <- iotc_data_initial%>%
      mutate(geographic_identifier = FISHING_GROUND_CODE,
             unit = "no", unit_target = "t", species = SPECIES_CODE, gear = GEAR_CODE,source_authority = "IOTC",
             conversion_factor = AVG_WEIGHT/1000, 
             time_start = lubridate::as_date(paste0(YEAR,"-",MONTH_START, "-01 "))) %>%
      mutate(time_end =lubridate::ceiling_date(time_start, "month") - 1 ) %>%
      select(gear	,source_authority,	species	,geographic_identifier,	time_start,	time_end,	unit	,unit_target,	conversion_factor) %>% mutate(value = conversion_factor) %>% 
      select(-conversion_factor)


  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
    iotc_data_final$time_start <- as.Date(iotc_data_final$time_start)
    iotc_data_final$time_end <- as.Date(iotc_data_final$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(iotc_data_final$time_start), "%Y"), "-01-01"),
    paste0(format(max(iotc_data_final$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #----------------------------------------------------------------------------------------------------------------------------
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(min(iotc_data_final$time_start),max(iotc_data_final$time_end),sep = "/")
  entity$setTemporalExtent(dataset_temporal_extent)
  
  iotc_data_final$time_start <- as.character(iotc_data_final$time_start)
  iotc_data_final$time_end <- as.character(iotc_data_final$time_end)
  iotc_data_final$geographic_identifier <- as.character(iotc_data_final$geographic_identifier)
  
  
  filename1 <- entity$data$source[[1]] #data
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  
  output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
  write.csv(iotc_data_final, output_name_dataset, row.names = FALSE)
  #----------------------------------------------------------------------------------------------------------------------------
  # entity$addResource("source", output_name_dataset)
  entity$addResource("harmonized", output_name_dataset)

  
}  


