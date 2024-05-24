#' Harmonize WCPFC Purse Seine Effort Datasets
#'
#' This function harmonizes the WCPFC Purse Seine effort datasets,
#' preparing them for integration into the Tuna Atlas database according to specified format requirements.
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details This function modifies the dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @importFrom stringr str_replace
#' @seealso \code{\link{WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD}} for converting WCPFC Drifnet data structure,
#'          \code{\link{convertDSD_wcpfc_nc}} for nominal catch data structure.
#' @export
#' @keywords data harmonization, fisheries, WCPFC, purse seine, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' 
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


# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  ALL    S 1970-01-01 1970-02-01  6100135    ALL        DAYS     53
#  ALL    S 1970-01-01 1970-02-01  6100135    LOG        SETS     55
#  ALL    S 1970-02-01 1970-03-01  6100125    ALL        DAYS     42
#  ALL    S 1970-02-01 1970-03-01  6100125    LOG        SETS     42
#  ALL    S 1970-02-01 1970-03-01  6100135    ALL        DAYS     57
#  ALL    S 1970-02-01 1970-03-01  6100135    LOG        SETS     50


#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  WCPFC_S_PUBLIC_BY_YR_MON.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  wcpfc_effort_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------


##Efforts

DF <- read.csv(path_to_raw_dataset)
colnames(DF) <- toupper(colnames(DF))
DF$CWP_GRID <- NULL

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
efforts_pivot_WCPFC <- DF; rm(DF)

#-----------------------------------------------------------
#Gear
efforts_pivot_WCPFC$Gear<-"S"

colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD.R")
efforts<-WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_WCPFC,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"WCPFC"
efforts$measurement <- "effort" 
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(
  paste0(format(min(efforts$time_start), "%Y"), "-01-01"),
  paste0(format(max(efforts$time_end), "%Y"), "-12-31"),
  sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

#@geoflow -> export as csv
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".DBF"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(efforts, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".DBF"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
