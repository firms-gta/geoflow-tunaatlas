#' Harmonize IATTC Nominal Catch Datasets
#'
#' This function harmonizes the IATTC nominal catch datasets,
#' preparing them for integration into the Tuna Atlas database, according to specified format requirements.
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
#' @import readr
#' @importFrom stringr str_replace
#' @seealso \code{\link{format_time_db_format}} for converting time format,
#' @export
#' @keywords data harmonization, fisheries, IATTC, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}


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
function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
#packages

  
  
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
filename2 <- entity$data$source[[2]] #structure
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------
   
### Nominal NC

NC <- read.csv(path_to_raw_dataset, header=TRUE, stringsAsFactors=FALSE, strip.white=TRUE)

colToKeep_NC<-c("AnoYear","BanderaFlag","ArteGear","EspeciesSpecies","t")
NC_harm_IATTC<-NC[,colToKeep_NC]
colnames(NC_harm_IATTC)<-c("Year", "Flag","Gear","Species","Catch")

NC_harm_IATTC$AreaName<-"EPO"
NC_harm_IATTC$AreaCWPgrid<-NA
NC_harm_IATTC$School<-"ALL"
NC_harm_IATTC$CatchType<-"ALL"
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
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
NC$time_start <- as.Date(NC$time_start)
NC$time_end <- as.Date(NC$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(
	paste0(format(min(NC$time_start), "%Y"), "-01-01"),
	paste0(format(max(NC$time_end), "%Y"), "-12-31"),
	sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

#@geoflow -> export as csv
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(NC, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)

 

}
