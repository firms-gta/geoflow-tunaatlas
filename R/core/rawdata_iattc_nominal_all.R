#' Harmonize IATTC Nominal Catch Datasets
#'
#' This function harmonizes the IATTC nominal catch datasets,
#' preparing them for integration into the Tuna Atlas database, according to specified format requirements.
#'
#' @details This function modifies the dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @import dplyr
#' @import readr
#' @seealso \code{\link{format_time_db_format}} for converting time format,
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (iattc/nominal/all/action_iattc_nominal_all.R).
gta_rawdata_iattc_nominal_all <- function(data) {
  # Samples of the input and output data (from the former script)
  # Input data sample:
  # AnoYear BanderaFlag ArteGear EspeciesSpecies    t
  #    1918         OTR       LP             SKJ 1361
  #    1918         OTR       LP             YFT    0
  #    1919         OTR       LP             SKJ 3130
  #    1919         OTR       LP             YFT  136
  #    1920         OTR       LP             SKJ 3583
  #    1920         OTR       LP             YFT  907
  # Catch: final data sample:
  # FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     ALB       ALL         MT  4854
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     BET       ALL         MT  1987
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     BIL       ALL         MT   122
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     PBF       ALL         MT   131
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     SFA       ALL         MT    93
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     SKH       ALL         MT  1326
  source(here::here("./R/sardara_functions/format_time_db_format.R"))
#packages

  
  


#----------------------------------------------------------------------------------------------------------------------------
#config --> the global config of the workflow

#----------------------------------------------------------------------------------------------------------------------------
   
### Nominal NC

NC <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(read.csv(path_to_raw_dataset, header=TRUE, stringsAsFactors=FALSE, strip.white=TRUE)))

colToKeep_NC<-c("AnoYear","BanderaFlag","ArteGear","EspeciesSpecies","t")
NC_harm_IATTC<-NC[,colToKeep_NC]
colnames(NC_harm_IATTC)<-c("Year", "Flag","Gear","Species","Catch")

NC_harm_IATTC$AreaName<-"EPO"
NC_harm_IATTC$AreaCWPgrid<-NA
NC_harm_IATTC$School<-"UNK"
NC_harm_IATTC$CatchType<-"NC" #measurement_type
NC_harm_IATTC$CatchUnits<-"t"
NC_harm_IATTC$RFMO<-"IATTC"
NC_harm_IATTC$Ocean<-"PAC_E"

NC_harm_IATTC$MonthStart<-1
NC_harm_IATTC$Period<-12

#Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end

NC_harm_IATTC<-format_time_db_format(NC_harm_IATTC)

NC <- NC_harm_IATTC[NC_harm_IATTC$Catch != 0,]

rm(NC_harm_IATTC)

colnames(NC)[colnames(NC)=="Flag"] <- "FishingFleet"

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
NC <-NC[,colToKeep_captures]
# remove 0 and NA values 
NC <- NC[NC$Catch != 0,]
NC <- NC[!is.na(NC$Catch),] 

NC <- aggregate(NC$Catch,
                FUN = sum,
                by = list(
                  FishingFleet = NC$FishingFleet,
                  Gear = NC$Gear,
                  time_start = NC$time_start,
                  time_end = NC$time_end,
                  AreaName = NC$AreaName,
                  School = NC$School,
                  Species = NC$Species,
                  CatchType = NC$CatchType,
                  CatchUnits = NC$CatchUnits
                )
)

colnames(NC)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
NC$source_authority<-"IATTC"
NC$measurement_type<-"NC"
NC$measurement<-"catch"
NC$measurement_processing_level<-"raised"
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
NC$time_start <- as.Date(NC$time_start)
NC$time_end <- as.Date(NC$time_end)
  NC
}
