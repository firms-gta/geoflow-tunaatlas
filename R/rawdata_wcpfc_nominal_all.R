#' Harmonize WCPFC Nominal Catch Datasets
#'
#' This function harmonizes WCPFC nominal catch datasets for integration into the Tuna Atlas database, ensuring data compliance with specified format requirements.
#'
#' @details This function modifies the nominal catch dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @seealso \code{format_time_db_format()} for converting WCPFC task 2 data structures.
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data_wcpfc The WCPFC nominal catch dataset (file XLS_WCPFC_<date>.csv): a data frame,
#'   or the path to the CSV file.
#' @param data_wcpo The WCPO nominal catch dataset (file XLS_WCPO_<date>.csv), idem.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (wcpfc/nominal/all/action_wcpfc_nominal_all.R).
#' @export
gta_rawdata_wcpfc_nominal_all <- function(data_wcpfc, data_wcpo) {
#----------------------------------------------------------------------------------------------------------------------------


### Nominal catches
#from wcpfc
wcpfc_species = c("ALV", "BLM", "BSH", "BTH", "BUM", "FAL", "LMA", "MAK", "OCS", "POR", "PTH", "RHN", "SMA", "SPK", "SPL", "SPN", "SPY", "SPZ", "THR")
NC1 <- gta_data(data_wcpfc, reader = function(path) gta_utf8(read.csv(path)))
NC1<-NC1[NC1$SP_CODE %in% wcpfc_species,]
#from wcpo
wcpo_species = c("ALB", "BET", "MLS", "PBF", "SKJ", "SWO", "YFT")
NC2 <- gta_data(data_wcpo, reader = function(path) gta_utf8(read.csv(path)))
NC2<-NC2[NC2$SP_CODE %in% wcpo_species,]

NC2$AreaName <- "WCPO"
NC1$AreaName <- "WCPFC"

#bind both sources
NC <- rbind(NC1,NC2)

colnames(NC)[colnames(NC) == "YY"] <- "Year"
colnames(NC)[colnames(NC) == "FLAG_CODE"] <- "FishingFleet"
colnames(NC)[colnames(NC) == "GEAR_CODE"] <- "Gear"
colnames(NC)[colnames(NC) == "SP_CODE"] <- "Species"
colnames(NC)[colnames(NC) == "SP_MT"] <- "Catch"
NC$Catch<-as.numeric(NC$Catch)
NC <- NC[!is.na(NC$Catch),]
NC <- NC[NC$Catch != 0,]
NC$CatchUnits <- "t"
NC$SP_NAME <- NULL
NC$FLEET_CODE <- NULL

NCAreaCWPgrid<-NA
NC$School<-"UNK"
NC$CatchType<-"NC"
NC$CatchUnits<-"t"
NC$RFMO<-"WCPFC"
NC$Ocean<-"PAC_W"

NC$MonthStart<-1
NC$Period<-12
#Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
NC<-format_time_db_format(NC)
NC <- NC[NC$Catch !=0 ,] #not sure if needed

NC <-NC[c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")]

# remove 0 and NA values 
NC <- NC[!is.na(NC$Catch),]
NC <- NC[NC$Catch != 0,]

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


colnames(NC)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type",
                "measurement_unit","measurement_value")
NC$source_authority<-"WCPFC"
NC$measurement<-"catch"
NC$measurement_processing_level<-"raised"
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
NC$time_start <- as.Date(NC$time_start)
NC$time_end <- as.Date(NC$time_end)
  NC
}
