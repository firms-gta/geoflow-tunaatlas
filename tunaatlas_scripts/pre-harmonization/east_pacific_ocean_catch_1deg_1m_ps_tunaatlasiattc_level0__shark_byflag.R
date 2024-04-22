#' Harmonize IATTC PS Shark ByFlag Catch Datasets
#'
#' This function harmonizes the structure of IATTC PS (Purse Seine) shark catch datasets by flag,
#' preparing them for integration into the Tuna Atlas database. It ensures that only the essential
#' fields are retained and that metadata is included if the dataset is destined for the database.
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; this function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details The function processes the data based on flag categories specifically for shark species.
#'          It involves cleaning, restructuring, and harmonizing data fields to meet specified
#'          standards. Metadata integration is optional and contingent on the destination of the dataset.
#'
#' @importFrom dplyr select mutate
#' @importFrom readr read_csv write_csv
#' @seealso \code{\link{FUN_catches_IATTC_CE_Flag_or_SetType_Shark}} for the specific processing of shark catches by flag,
#'          \code{\link{IATTC_CE_catches_pivotDSD_to_harmonizedDSD}} for general data structuring.
#' @export
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @keywords IATTC, tuna, fisheries, data harmonization, shark catch


# '# This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|Flag|LatC1|LonC1|NumSets}
function(action, entity, config){
  
#packages

  


#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  PublicPSSharkFlag.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iattc_catch_code_lists.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------

##Catches
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_catches_IATTC_CE_Flag_or_SetType_Shark.R")
catches_pivot_IATTC <-FUN_catches_IATTC_CE_Flag_or_SetType_Shark(path_to_raw_dataset,"Flag","PS")
catches_pivot_IATTC$NumSets<-NULL

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/IATTC_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IATTC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"IATTC"

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
