#' Harmonize IATTC Longline Catch Datasets
#'
#' This function processes and harmonizes Inter-American Tropical Tuna Commission (IATTC) longline
#' catch datasets for shark and tuna_billfish species. It prepares the data for integration into the
#' Tuna Atlas database, ensuring compliance with data standardization requirements and optionally
#' including metadata if the dataset is intended for database loading.
#'
#' @param action Contextual action data provided by the geoflow framework, used for controlling workflow processes.
#' @param entity Contextual entity data describing the dataset within the geoflow framework.
#' @param config Configuration settings provided by the geoflow framework.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details The function restructures the dataset to include only essential fields, performs any necessary calculations
#'          for catch units, and standardizes the format for date fields and geographical identifiers.
#'          Metadata integration is contingent on the final use of the dataset within the Tuna Atlas database.
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr filter mutate left_join
#' @importFrom tidyr gather
#' @importFrom reshape2 melt
#' @seealso \code{\link{IATTC_CE_catches_pivotDSD_to_harmonizedDSD}} for specific data structuring operations.
#' @export
#' @keywords IATTC, tuna, fisheries, data harmonization, longline catch
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}

# Catch input data sample:
# Record Spp DTypeID Number Weight
#  11407 ALB       2     17     NA
#  11407 BUM       2      4     NA
#  11407 BLM       2      2     NA
#  11407 SWO       2     10     NA
#  11407 BET       2    403     NA
#  11407 BIL       2      1     NA


# Catch: final data sample:
# FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
#  USA   LL 1992-07-01 1992-08-01  6425135    ALL     BSH       ALL         NO     4
#  USA   LL 1993-04-01 1993-05-01  6425135    ALL     BSH       ALL         NO    75
#  USA   LL 1993-04-01 1993-05-01  6430135    ALL     BSH       ALL         NO    15
#  USA   LL 1993-05-01 1993-06-01  6425135    ALL     BSH       ALL         NO    24
#  USA   LL 1994-03-01 1994-04-01  6425135    ALL     BSH       ALL         NO    14
#  USA   LL 1994-03-01 1994-04-01  6430135    ALL     BSH       ALL         NO     4
function(action, entity, config){
  
#packages

  
if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}

#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename_catch <- entity$data$source[[1]] #catch data
# Historical name for the dataset at source  PublicCatchOrigFormatTunaBillfish.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
# Historical name for the dataset at source  PublicCatchOrigFormatShark.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
filename_effort <- entity$data$source[[2]] #effort data
# Historical name for the dataset at source  PublicEffortOrigFormatTunaBillfish.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
# Historical name for the dataset at source  PublicEffortOrigFormatShark.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
filename_str <- entity$data$source[[3]] #structure
# Historical name for the dataset at source  iattc_catch_code_lists.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
path_to_raw_dataset_catch <- entity$getJobDataResource(config, filename_catch)
path_to_raw_dataset_effort <- entity$getJobDataResource(config, filename_effort)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------

##Catches
catches<-read.csv(path_to_raw_dataset_catch, stringsAsFactors = F)
efforts<-read.csv(path_to_raw_dataset_effort, stringsAsFactors = F)
catches <- catches %>% tidyr::gather(variable, value, -c("Record","Spp","DTypeID"))
# remove values=0
catches <- catches  %>% 
  dplyr::filter( ! value %in% 0 ) %>%
  dplyr::filter( ! is.na(value)) 

# Set catchunit values
# DType code 1 means that the data was submitted in both weight and number for the same catch
# DType code 2 means that the data was submitted as number only
# DType code 3 means that the data was submitted as weight only

#Initialisation
catches$CatchUnits<-'NA'

index.CatchunitMT<-which(catches[,"DTypeID"]==3 & catches[,"variable"]=="Weight")
index.CatchunitNO<-which(catches[,"DTypeID"]==2 & catches[,"variable"]=="Number")
index.CatchunitMTNO<-which(catches[,"DTypeID"]==1 & catches[,"variable"]=="Weight")
index.CatchunitNOMT<-which(catches[,"DTypeID"]==1 & catches[,"variable"]=="Number")

catches$CatchUnits[index.CatchunitMT]<-"t"
catches$CatchUnits[index.CatchunitNO]<-"no"
catches$CatchUnits[index.CatchunitMTNO]<-"MTNO"
catches$CatchUnits[index.CatchunitNOMT]<-"NOMT"

# Merge catches and efforts to have the strata in the catch file
catches<-left_join(catches,efforts,by=c("Record"))
catches <- catches[c("Spp","value","CatchUnits","Year","Month","FlagAbv","Lat","Lon")]

colnames(catches)<-c("variable","value","CatchUnits","Year","Month","Flag","Lat","Lon")

catches$SquareSize<-5
catches$CodeSquareSize<-6
catches$Gear<-"LL"
catches$SetType<-"ALL"

catches$variable[which(catches[,"variable"]=="BuM")]<-"BUM"

colnames(catches)[colnames(catches)=="Flag"] <- "FishingFleet"

# Reach the catches harmonized DSD using a function in IATTC_functions.R
colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/IATTC_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches,colToKeep_captures)

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
output_name_dataset <- gsub(filename_catch, paste0(unlist(strsplit(filename_catch,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset_catch)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename_catch, paste0(unlist(strsplit(filename_catch,".csv"))[1], "_codelists.csv"), path_to_raw_dataset_catch)
file.rename(from = entity$getJobDataResource(config, filename_str), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset_catch)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
 
}
