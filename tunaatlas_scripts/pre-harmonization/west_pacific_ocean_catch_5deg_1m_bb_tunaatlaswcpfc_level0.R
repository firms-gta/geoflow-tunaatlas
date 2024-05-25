#' Harmonize WCPFC Pole-and-Line Catch Datasets
#'
#' This function processes and harmonizes the structure of Western and Central Pacific Fisheries Commission (WCPFC)
#' pole-and-line catch datasets. The output is tailored for integration into the Tuna Atlas database, aligning with 
#' the requirements for data standardization and optionally including metadata if the dataset is intended for database loading.
#'
#' @param action Contextual action data provided by the geoflow framework, used for controlling workflow processes.
#' @param entity Contextual entity data describing the dataset within the geoflow framework.
#' @param config Configuration settings provided by the geoflow framework.
#'
#' @return None; the function outputs files directly, including harmonized datasets, optional metadata, and code lists for database integration.
#'
#' @details The function restructures datasets to include only essential fields, performs any necessary calculations for catch units, 
#'          and standardizes the format for date fields and geographical identifiers. Metadata incorporation is contingent on the final 
#'          data usage within the Tuna Atlas database.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom readr read_csv write_csv
#' @importFrom tidyr gather
#' @importFrom reshape2 melt
#' @seealso \code{\link{WCPFC_CE_catches_pivotDSD_to_harmonizedDSD}} for detailed data structuring operations.
#' @export
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' @keywords WCPFC, tuna, fisheries, data harmonization, pole-and-line catch

# Input data sample:
# YY MM LAT5 LON5 DAYS SKJ_C YFT_C OTH_C
# 1950  1  30N 135E    0     0     0     0
# 1950  1  30N 140E    0     0     0     0
# 1950  1  35N 140E    0     0     0     0
# 1950  1  40N 140E    0     0     0     0
# 1950  1  40N 145E    0     0     0     0
# 1950  2  30N 135E    0     0     0     0

# Catch: pivot data sample:
# YY MM LAT5 LON5 Effort Species  value CatchUnits School EffortUnits Gear
# 1970  3  05S 150E     82     SKJ 279.16         MT    ALL        DAYS    P
# 1970  4  05S 150E     74     SKJ 336.61         MT    ALL        DAYS    P
# 1970  5  05S 150E     82     SKJ 361.80         MT    ALL        DAYS    P
# 1970  6  05S 150E     81     SKJ 438.48         MT    ALL        DAYS    P
# 1970  7  05S 150E     75     SKJ 472.75         MT    ALL        DAYS    P
# 1970 12  05S 150E     56     SKJ 215.05         MT    ALL        DAYS    P


# Catch: final data sample:
# FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits  Catch
#  ALL    P 1970-03-01 1970-04-01  6200150    ALL     OTH       ALL         MT   0.01
#  ALL    P 1970-03-01 1970-04-01  6200150    ALL     SKJ       ALL         MT 279.16
#  ALL    P 1970-03-01 1970-04-01  6200150    ALL     YFT       ALL         MT  27.81
#  ALL    P 1970-04-01 1970-05-01  6200150    ALL     OTH       ALL         MT   0.14
#  ALL    P 1970-04-01 1970-05-01  6200150    ALL     SKJ       ALL         MT 336.61
#  ALL    P 1970-04-01 1970-05-01  6200150    ALL     YFT       ALL         MT  11.34
function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/WCPFC_CE_catches_pivotDSD_to_harmonizedDSD.R")
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
# Historical name for the dataset at source  WCPFC_P_PUBLIC_BY_YR_MON.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  wcpfc_catch_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------


## Catches

### Reach the catches pivot DSD using a function stored in WCPFC_functions.R
#-------------
#Changes:
#	- switch to csv
#	- switch to upper colnames
#	- CWP grid (removed for the timebeing to apply rtunaatlas codes)
#	- toupper applied to Species/CatchUnits

DF <- read.csv(path_to_raw_dataset)
colnames(DF) <- toupper(colnames(DF))
DF$CWP_GRID <- NULL 

DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:5])))

DF <- DF %>% dplyr::filter(!value %in% 0) %>% dplyr::filter(!is.na(value))
DF$variable <- as.character(DF$variable)
colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
DF$CatchUnits <- substr(DF$Species, nchar(DF$Species), nchar(DF$Species))
DF$CatchUnits <- toupper(DF$CatchUnits) 
DF$Species <- toupper(DF$Species) 
DF$Species <- sub("_C", "", DF$Species)
DF$Species <- sub("_N", "", DF$Species)
DF$School <- "OTH"
DF$EffortUnits <- colnames(DF[5])
colnames(DF)[5] <- "Effort"
catches_pivot_WCPFC <- DF; rm(DF)
#-------------

#Gear
catches_pivot_WCPFC$Gear<-"P"

# Catchunits
index.kg <- which( catches_pivot_WCPFC[,"CatchUnits"] == "C" )
catches_pivot_WCPFC[index.kg,"CatchUnits"]<- "t"

index.nr <- which( catches_pivot_WCPFC[,"CatchUnits"] == "N" )
catches_pivot_WCPFC[index.nr,"CatchUnits"]<- "no" 

# School
catches_pivot_WCPFC$School<-"ALL"

### Reach the catches harmonized DSD using a function in WCPFC_functions.R

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches<-WCPFC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_WCPFC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"WCPFC"
catches$measurement_type <- "RC" # Retained catches
catches$measurement <- "catch" 
#----------------------------------------------------------------------------------------------------------------------------
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
