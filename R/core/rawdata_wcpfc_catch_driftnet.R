#' Harmonize WCPFC Driftnet Catch Datasets
#'
#' This function processes and harmonizes Western and Central Pacific Fisheries Commission (WCPFC)
#' driftnet catch datasets. It prepares the data for integration into the Tuna Atlas database,
#' ensuring compliance with data standardization requirements and optionally including metadata.
#'
#' @details This function modifies the dataset to include only essential fields, performs necessary
#'          recalculations for catch units, and standardizes the format for date fields and geographical identifiers.
#'          Metadata integration is contingent on the final use of the dataset within the Tuna Atlas database.
#'
#' @seealso \code{\link{WCPFC_CE_catches_pivotDSD_to_harmonizedDSD}} for specific data structuring operations.
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' This script works with any dataset that has the first 5 columns named and ordered as follow: {YY|MM|LAT5|LON5|DAYS} followed by a list of columns specifing the species codes with "_N" for catches expressed in number and "_T" for catches expressed in tons
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (wcpfc/catch/driftnet/action_wcpfc_catch_driftnet.R).
gta_rawdata_wcpfc_catch_driftnet <- function(data) {
  # Samples of the input and output data (from the former script)
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
  
#packages

  





  
#----------------------------------------------------------------------------------------------------------------------------
#config --> the global config of the workflow
# Historical name for the dataset at source  WCPFC_G_PUBLIC_BY_YR_MON.csv
# Historical name for the dataset at source  wcpfc_catch_code_lists.csv

#----------------------------------------------------------------------------------------------------------------------------


## Catches
colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")

### Reach the catches pivot DSD using a function stored in WCPFC_functions.R

DF <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(read.csv(path_to_raw_dataset)))
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

source(here::here("./R/sardara_functions/WCPFC_CE_catches_pivotDSD_to_harmonizedDSD.R"))
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
  catches
}
