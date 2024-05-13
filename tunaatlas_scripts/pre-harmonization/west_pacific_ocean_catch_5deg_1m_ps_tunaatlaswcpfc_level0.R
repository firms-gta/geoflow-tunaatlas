#' Harmonize WCPFC Purse Seine Catch Datasets
#'
#' This function harmonizes WCPFC Purse Seine catch datasets for integration into the Tuna Atlas database, ensuring data compliance with specified format requirements.
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details This function modifies the Purse Seine catch dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @importFrom dplyr %>% filter select mutate group_by summarise
#' @importFrom tidyr gather
#' @importFrom reshape melt
#' @seealso \code{\link{WCPFC_CE_catches_pivotDSD_to_harmonizedDSD}} to convert WCPFC task 2 Purse Seine data structure.
#' @export
#' @keywords data harmonization, fisheries, WCPFC, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}

  # Input data sample:
  # YY MM LAT5 LON5 DAYS SETS_UNA SETS_LOG SETS_DFAD SETS_AFAD SETS_OTH SKJ_C_UNA YFT_C_UNA BET_C_UNA OTH_C_UNA SKJ_C_LOG YFT_C_LOG BET_C_LOG OTH_C_LOG SKJ_C_DFAD
  # 1967  2  30N 135E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
  # 1967  2  30N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
  # 1967  2  35N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
  # 1967  2  40N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
  # 1967  2  40N 145E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
  # 1967  3  30N 135E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
  # YFT_C_DFAD BET_C_DFAD OTH_C_DFAD SKJ_C_AFAD YFT_C_AFAD BET_C_AFAD OTH_C_AFAD SKJ_C_OTH YFT_C_OTH BET_C_OTH OTH_C_OTH
  #          0          0          0          0          0          0          0         0         0         0         0
  #          0          0          0          0          0          0          0         0         0         0         0
  #          0          0          0          0          0          0          0         0         0         0         0
  #          0          0          0          0          0          0          0         0         0         0         0
  #          0          0          0          0          0          0          0         0         0         0         0
  #          0          0          0          0          0          0          0         0         0         0         0
  
  
  # Catch: final data sample:
  # FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits   Catch
  #  ALL    S 1970-01-01 1970-02-01  6100135    LOG     BET       ALL         MT  12.181
  #  ALL    S 1970-01-01 1970-02-01  6100135    LOG     SKJ       ALL         MT  84.587
  #  ALL    S 1970-01-01 1970-02-01  6100135    LOG     YFT       ALL         MT 110.307
  #  ALL    S 1970-02-01 1970-03-01  6100125    LOG     BET       ALL         MT   5.943
  #  ALL    S 1970-02-01 1970-03-01  6100125    LOG     SKJ       ALL         MT  35.133
  #  ALL    S 1970-02-01 1970-03-01  6100125    LOG     YFT       ALL         MT  53.466
function(action, entity, config){
  
#packages

  
    

if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
}

if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}

if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

    
  
#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  WCPFC_S_PUBLIC_BY_YR_MON.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  wcpfc_catch_code_lists.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------

  
##Catches

### Reach the catches pivot DSD using a function stored in WCPFC_functions.R
#catches_pivot_WCPFC<-FUN_catches_WCPFC_CE_Purse_Seine_2016 (path_to_raw_dataset)
#2020-11-13 @eblondel for Tuna atlas update
#Changes
#	- change from dbf to csv
#	- remove cwp_grid code
#	- to upper colnames
#-----------------------------------------------------------
DF <- read.csv(path_to_raw_dataset)
colnames(DF) <- toupper(colnames(DF))
DF$CWP_GRID <- NULL

# DF <- melt(DF, id = c(colnames(DF[1:10])))  #@juldebar error with melt function from reshape package
# DF <- melt(as.data.table(DF), id=c(colnames(DF[1:10])))
DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:10])))

DF <- DF %>% dplyr::filter(!value %in% 0) %>% dplyr::filter(!is.na(value))
DF$variable <- as.character(DF$variable)
colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
DF$School <- substr(DF$Species, 7, nchar(DF$Species))
DF$Species <- sub("_C_UNA", "", DF$Species)
DF$Species <- sub("_C_LOG", "", DF$Species)
DF$Species <- sub("_C_DFAD", "", DF$Species)
DF$Species <- sub("_C_AFAD", "", DF$Species)
DF$Species <- sub("_C_OTH", "", DF$Species)
DF$CatchUnits <- "t"
DF$EffortUnits <- colnames(DF[5])
colnames(DF)[5] <- "Effort"
catches_pivot_WCPFC <- DF; rm(DF)

#-----------------------------------------------------------
#Gear
catches_pivot_WCPFC$Gear<-"S"

# Catchunits
# Check data that exist both in number and weight

### Reach the catches harmonized DSD using a function in WCPFC_functions.R
colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/WCPFC_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-WCPFC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_WCPFC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"WCPFC"
catches$measurement_type <- "RC" # Retained catches
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
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".DBF"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".DBF"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
