#' Harmonize WCPFC Driftnet Effort Datasets
#'
#' This function harmonizes the WCPFC Driftnet effort datasets,
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
#' @seealso \code{\link{WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD}} for converting WCPFC Driftnet data structure,
#' @export
#' @keywords data harmonization, fisheries, WCPFC, driftnet, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' This script works with any dataset that has the first 5 columns named and ordered as follow: {YY|MM|LAT5|LON5|DAYS} followed by a list of columns specifing the species codes with "_N" for catches expressed in number and "_T" for catches expressed in tons


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
# YY MM LAT5 LON5 DAYS ALB_N  ALB_C
# 1983 11  30S 170W    0     0  0.000
# 1983 11  35S 170W  133   886  4.960
# 1983 12  35S 165W    0     0  0.000
# 1983 12  35S 170W  133   870  4.872
# 1983 12  40S 165W    0     0  0.000
# 1983 12  40S 170W  248  3822 21.402

# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  ALL    D 1983-11-01 1983-12-01  6330165    ALL        DAYS    133
#  ALL    D 1983-12-01 1984-01-01  6330165    ALL        DAYS    133
#  ALL    D 1983-12-01 1984-01-01  6335165    ALL        DAYS    248
#  ALL    D 1984-01-01 1984-02-01  6230165    ALL        DAYS     85
#  ALL    D 1984-01-01 1984-02-01  6240160    ALL        DAYS     59
#  ALL    D 1984-01-01 1984-02-01  6335165    ALL        DAYS    704


#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  WCPFC_G_PUBLIC_BY_YR_MON.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  wcpfc_effort_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------



colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")


##Efforts

DF <- read.csv(path_to_raw_dataset)
colnames(DF) <- toupper(colnames(DF))
# DF <- melt(DF, id = c(colnames(DF[1:5]))) #@juldebar error with melt function from reshape package
# DF <- melt(as.data.table(DF), id=c(colnames(DF[1:5])))
DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:5])))

DF <- DF %>% dplyr::filter(!value %in% 0) %>% dplyr::filter(!is.na(value))
DF$variable <- as.character(DF$variable)
colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
DF$CatchUnits <- substr(DF$Species, nchar(DF$Species), nchar(DF$Species))
DF$Species <- toupper(DF$Species) 
DF$Species <- sub("_C", "", DF$Species)
DF$Species <- sub("_N", "", DF$Species)
DF$School <- "OTH"
DF$EffortUnits <- colnames(DF[5])
colnames(DF)[5] <- "Effort"
#--------------------------------------------------
efforts_pivot_WCPFC <- DF; rm(DF)

#Gear
efforts_pivot_WCPFC$Gear<-"D"

# Catchunits
index.kg <- which( efforts_pivot_WCPFC[,"CatchUnits"] == "C" )
efforts_pivot_WCPFC[index.kg,"CatchUnits"]<- "t"

index.nr <- which( efforts_pivot_WCPFC[,"CatchUnits"] == "N" )
efforts_pivot_WCPFC[index.nr,"CatchUnits"]<- "no" 

# School
efforts_pivot_WCPFC$School<-"UNK"

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
