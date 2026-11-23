#' Harmonize WCPFC Driftnet Effort Datasets
#'
#' This function harmonizes the WCPFC Driftnet effort datasets,
#' preparing them for integration into the Tuna Atlas database according to specified format requirements.
#'
#' @details This function modifies the dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @seealso \code{\link{WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD}} for converting WCPFC Driftnet data structure,
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' This script works with any dataset that has the first 5 columns named and ordered as follow: {YY|MM|LAT5|LON5|DAYS} followed by a list of columns specifing the species codes with "_N" for catches expressed in number and "_T" for catches expressed in tons
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (wcpfc/effort/driftnet/action_wcpfc_effort_driftnet.R).
gta_rawdata_wcpfc_effort_driftnet <- function(data) {
  
  #packages 
  
    
      
  
  
  

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
#config --> the global config of the workflow
# Historical name for the dataset at source  WCPFC_G_PUBLIC_BY_YR_MON.csv
# Historical name for the dataset at source  wcpfc_effort_code_lists.csv

#----------------------------------------------------------------------------------------------------------------------------



colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")


##Efforts

DF <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(read.csv(path_to_raw_dataset)))
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
efforts_pivot_WCPFC$School<-"OTH"

efforts<-WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_WCPFC,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"WCPFC"
efforts$measurement <- "effort" 
efforts$measurement_processing_level <- "unknown" 
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
  efforts
}
