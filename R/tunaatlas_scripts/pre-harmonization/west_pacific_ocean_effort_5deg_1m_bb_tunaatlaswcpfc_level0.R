#' Harmonize WCPFC Pole-and-line Effort Datasets
#'
#' This function harmonizes the WCPFC Pole-and-line effort datasets, preparing them
#' for integration into the Tuna Atlas database according to specified format requirements.
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
#' @import reshape
#' @importFrom stringr str_replace
#' @seealso \code{\link{WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD}} for converting WCPFC Driftnet data structure,
#' @export
#' @keywords data harmonization, fisheries, WCPFC, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
function(action, entity, config){
  
  # Input data sample:
  # YY MM LAT5 LON5 DAYS SKJ_C YFT_C OTH_C
  # 1950  1  30N 135E    0     0     0     0
  # 1950  1  30N 140E    0     0     0     0
  # 1950  1  35N 140E    0     0     0     0
  # 1950  1  40N 140E    0     0     0     0
  # 1950  1  40N 145E    0     0     0     0
  # 1950  2  30N 135E    0     0     0     0
  
  # Effort: pivot data sample:
  # YY MM LAT5 LON5 Effort EffortUnits School Gear
  # 1950  1  30N 135E      0        DAYS    ALL    P
  # 1950  1  30N 140E      0        DAYS    ALL    P
  # 1950  1  35N 140E      0        DAYS    ALL    P
  # 1950  1  40N 140E      0        DAYS    ALL    P
  # 1950  1  40N 145E      0        DAYS    ALL    P
  # 1950  2  30N 135E      0        DAYS    ALL    P
  
  # Effort: final data sample:
  # Flag Gear time_start   time_end AreaName School EffortUnits Effort
  #  ALL    P 1970-03-01 1970-04-01  6200150    ALL        DAYS     82
  #  ALL    P 1970-04-01 1970-05-01  6200150    ALL        DAYS     74
  #  ALL    P 1970-05-01 1970-06-01  6200150    ALL        DAYS     82
  #  ALL    P 1970-06-01 1970-07-01  6200150    ALL        DAYS     81
  #  ALL    P 1970-07-01 1970-08-01  6200150    ALL        DAYS     75
  #  ALL    P 1970-12-01 1971-01-01  6200150    ALL        DAYS     56

  #packages
  
    
  if(!require(foreign)){
    install.packages("foreign")
    require(foreign)
  }
  
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
# Historical name for the dataset at source  WCPFC_P_PUBLIC_BY_YR_MON.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  wcpfc_effort_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
  
##Efforts

DF <- read.csv(path_to_raw_dataset)
colnames(DF) <- toupper(colnames(DF))
DF$CWP_GRID <- NULL 
DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:5])))

DF <- DF %>% dplyr::filter(!value %in% 0) %>% dplyr::filter(!is.na(value))
DF$variable <- as.character(DF$variable)
colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
DF$CatchUnits <- substr(DF$Species, nchar(DF$Species), nchar(DF$Species))
DF$CatchUnits <- toupper(DF$CatchUnits) 
#@eblondel to upper
DF$Species <- toupper(DF$Species) 
#@eblondel to upper
DF$Species <- sub("_C", "", DF$Species)
DF$Species <- sub("_N", "", DF$Species)
DF$School <- "OTH"
DF$EffortUnits <- colnames(DF[5])
colnames(DF)[5] <- "Effort"
efforts_pivot_WCPFC <- DF; rm(DF)
#-------------

#Gear
efforts_pivot_WCPFC$Gear<-"P"

# Catchunits
index.kg <- which( efforts_pivot_WCPFC[,"CatchUnits"] == "C" )
efforts_pivot_WCPFC[index.kg,"CatchUnits"]<- "t"

index.nr <- which( efforts_pivot_WCPFC[,"CatchUnits"] == "N" )
efforts_pivot_WCPFC[index.nr,"CatchUnits"]<- "no" 

# School
efforts_pivot_WCPFC$School<-"UNK"

colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD.R")

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
