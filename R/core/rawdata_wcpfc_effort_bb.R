#' Harmonize WCPFC Pole-and-line Effort Datasets
#'
#' This function harmonizes the WCPFC Pole-and-line effort datasets, preparing them
#' for integration into the Tuna Atlas database according to specified format requirements.
#'
#' @details This function modifies the dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @import dplyr
#' @import tidyr
#' @import reshape
#' @seealso \code{\link{WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD}} for converting WCPFC Driftnet data structure,
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (wcpfc/effort/bb/action_wcpfc_effort_bb.R).
gta_rawdata_wcpfc_effort_bb <- function(data) {
  
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
  
    
  
  
  
  



#----------------------------------------------------------------------------------------------------------------------------
  #config --> the global config of the workflow
  # Historical name for the dataset at source  WCPFC_P_PUBLIC_BY_YR_MON.csv
  # Historical name for the dataset at source  wcpfc_effort_code_lists.csv
  
  
  ##Efforts
  
  DF <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(read.csv(path_to_raw_dataset)))
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
  efforts_pivot_WCPFC$School<-"OTH"
  
  colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
  source(here::here("./R/sardara_functions/WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD.R"))
  
  efforts<-WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_WCPFC,colToKeep_efforts)
  
  colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
  efforts$source_authority<-"WCPFC"
  efforts$measurement <- "effort" 
  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
  efforts$time_start <- as.Date(efforts$time_start)
  efforts$time_end <- as.Date(efforts$time_end)
  efforts$measurement_processing_level <- "unknown" 
  efforts
}
