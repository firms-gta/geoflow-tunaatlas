#' Harmonize IATTC PS ByFlag Catch Datasets
#'
#' Harmonizes the structure of IATTC PS (Purse Seine) catch datasets by flag. This function is designed
#' to adjust catch data to fit standardized formats required for integration into the Tuna Atlas database.
#' The process involves reformatting the data and possibly integrating metadata and code lists if they
#' will be loaded into the Tuna Atlas database.
#' This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|Flag|LatC1|LonC1|NumSets}
#'
#' @param action Contextual action data typically provided by the geoflow framework.
#' @param entity Contextual entity data describing the dataset within the geoflow framework.
#' @param config Configuration settings provided by the geoflow framework.
#'
#' @details The function reads raw data, processes it according to specified stratifications such as
#'          'PublicPSBillfishFlag', 'PublicPSTunaFlag', and 'PublicPSSharkFlag', and then outputs a
#'          harmonized dataset. It can conditionally include metadata and code lists based on whether the
#'          data is intended for database loading.
#'
#' @return Does not return anything; it outputs files directly to the specified location within the geoflow system.
#'
#' @importFrom dplyr filter mutate
#' @importFrom readr read_csv write_csv
#' @seealso \code{\link{convertDSD_iattc_ce}} for other conversions related to IATTC data,
#'          \code{\link{format_time_db_format}} to standardize time formatting for database integration.
#' @export
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' @keywords IATTC, tuna, fisheries, data harmonization, catch data
function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/FUN_catches_IATTC_CE_Flag_or_SetType.R")
#packages

  if(!require(data.table)){
    install.packages("data.table")
    require(data.table)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  if(!require(reshape2)){
    install.packages("reshape2")
    require(reshape2)
  }
#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  PublicPSTunaFlag.csv or  PublicPSBillfishFlag.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iattc_catch_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------


## Catches

catches_pivot_IATTC <-FUN_catches_IATTC_CE_Flag_or_SetType(path_to_raw_dataset,"Flag","PS")
catches_pivot_IATTC$NumSets<-NULL

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/IATTC_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IATTC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"IATTC"
catches$measurement_type <- "RC" # Retained catches
catches$measurement <- "catch"
catches$measurement_processing_level <- "unknown"
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

#@geoflow -> export as csv
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
