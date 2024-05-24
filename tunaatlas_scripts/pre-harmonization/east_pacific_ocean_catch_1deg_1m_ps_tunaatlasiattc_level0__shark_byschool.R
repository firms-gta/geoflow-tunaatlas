#' Harmonize IATTC PS Shark BySchool Catch Datasets
#'
#' This function processes and harmonizes the Inter-American Tropical Tuna Commission (IATTC)
#' PS (Purse Seine) shark catch datasets by school type. It prepares the data for integration into the
#' Tuna Atlas database, aligning with the requirements for data standardization and optionally
#' including metadata if the dataset is intended for database loading.
#'
#' @param action Contextual action data provided by the geoflow framework, used for controlling workflow processes.
#' @param entity Contextual entity data describing the dataset within the geoflow framework.
#' @param config Configuration settings provided by the geoflow framework.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details This function restructures the dataset to include only essential fields, performs any necessary calculations
#'          for catch units, and standardizes the format for date fields and geographical identifiers.
#'          Metadata integration is contingent on the final use of the dataset within the Tuna Atlas database.
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr filter mutate
#' @importFrom tidyr gather
#' @seealso \code{\link{FUN_catches_IATTC_CE_Flag_or_SetType_Shark}} for specific data processing of shark catches by school,
#'          \code{\link{IATTC_CE_catches_pivotDSD_to_harmonizedDSD}} for general data structuring operations.
#' @export
#' @keywords IATTC, tuna, fisheries, data harmonization, shark catch, school
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
# '# This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|Flag|LatC1|LonC1|NumSets}

function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_catches_IATTC_CE_Flag_or_SetType_Shark.R")
#packages


#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  PublicPSSharkSetType.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iattc_catch_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------

##Catches

catches_pivot_IATTC <-FUN_catches_IATTC_CE_Flag_or_SetType_Shark(path_to_raw_dataset,"SetType","PS")
catches_pivot_IATTC$NumSets<-NULL

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/IATTC_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IATTC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"IATTC"
catches$measurement_type <- "RC" # Retained catches
catches$measurement <- "catch"
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
