#' Harmonize WCPFC Driftnet Catch Datasets
#'
#' This function processes and harmonizes Western and Central Pacific Fisheries Commission (WCPFC)
#' driftnet catch datasets. It prepares the data for integration into the Tuna Atlas database,
#' ensuring compliance with data standardization requirements and optionally including metadata.
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details This function modifies the dataset to include only essential fields, performs necessary
#'          recalculations for catch units, and standardizes the format for date fields and geographical identifiers.
#'          Metadata integration is contingent on the final use of the dataset within the Tuna Atlas database.
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr filter mutate
#' @importFrom tidyr gather
#' @importFrom foreign read.dbf
#' @importFrom reshape2 melt
#' @seealso \code{\link{WCPFC_CE_catches_pivotDSD_to_harmonizedDSD}} for specific data structuring operations.
#' @export
#' @keywords WCPFC, tuna, fisheries, data harmonization, driftnet catch
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' This script works with any dataset that has the first 5 columns named and ordered as follow: {YY|MM|LAT5|LON5|DAYS} followed by a list of columns specifing the species codes with "_N" for catches expressed in number and "_T" for catches expressed in tons
  
  # Input data sample:
  # YY MM LAT5 LON5 DAYS ALB_N  ALB_C
  # 1983 11  30S 170W    0     0  0.000
  # 1983 11  35S 170W  133   886  4.960
  # 1983 12  35S 165W    0     0  0.000
  # 1983 12  35S 170W  133   870  4.872
  # 1983 12  40S 165W    0     0  0.000
  # 1983 12  40S 170W  248  3822 21.402
  
  # Catch: final data sample:
  # FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits    Catch
  #  ALL    D 1983-11-01 1983-12-01  6330165    ALL     ALB       ALL         MT    4.960
  #  ALL    D 1983-11-01 1983-12-01  6330165    ALL     ALB       ALL         NO  886.000
  #  ALL    D 1983-12-01 1984-01-01  6330165    ALL     ALB       ALL         MT    4.872
  #  ALL    D 1983-12-01 1984-01-01  6330165    ALL     ALB       ALL         NO  870.000
  #  ALL    D 1983-12-01 1984-01-01  6335165    ALL     ALB       ALL         MT   21.402
  #  ALL    D 1983-12-01 1984-01-01  6335165    ALL     ALB       ALL         NO 3822.000
function(action, entity, config){
  
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
# Historical name for the dataset at source  WCPFC_G_PUBLIC_BY_YR_MON.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  wcpfc_catch_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------
  
  
## Catches
colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")

### Reach the catches pivot DSD using a function stored in WCPFC_functions.R

DF <- read.csv(path_to_raw_dataset)
colnames(DF) <- toupper(colnames(DF))
DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:5])))

DF <- DF %>% dplyr::filter(!value %in% 0) %>% dplyr::filter(!is.na(value))
DF$variable <- as.character(DF$variable)
colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
DF$CatchUnits <- substr(DF$Species, nchar(DF$Species), nchar(DF$Species))
DF$Species <- toupper(DF$Species) 
#@eblondel added
DF$Species <- sub("_C", "", DF$Species)
DF$Species <- sub("_N", "", DF$Species)
DF$School <- "OTH"
DF$EffortUnits <- colnames(DF[5])
colnames(DF)[5] <- "Effort"
#--------------------------------------------------
catches_pivot_WCPFC <- DF; rm(DF)

#Gear
catches_pivot_WCPFC$Gear<-"D"

# Catchunits
index.kg <- which( catches_pivot_WCPFC[,"CatchUnits"] == "C" )
catches_pivot_WCPFC[index.kg,"CatchUnits"]<- "t"

index.nr <- which( catches_pivot_WCPFC[,"CatchUnits"] == "N" )
catches_pivot_WCPFC[index.nr,"CatchUnits"]<- "no" 

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/WCPFC_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-WCPFC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_WCPFC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"WCPFC"
catches$measurement_type <- "RC" # Retained catches
catches$measurement <- "catch" 
catches$measurement_processing_level <- "raised"
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
