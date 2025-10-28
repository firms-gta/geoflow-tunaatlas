#' Harmonize WCPFC Nominal Catch Datasets
#'
#' This function harmonizes WCPFC nominal catch datasets for integration into the Tuna Atlas database, ensuring data compliance with specified format requirements.
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details This function modifies the nominal catch dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr %>% filter select mutate group_by summarise
#' @seealso \code{\link{format_time_db_format}} for converting WCPFC task 2 data structures.
#' @export
#' @keywords data harmonization, fisheries, WCPFC, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
# Input data sample:
  # yy gear flag fleet alb_mt bet_mt pbf_mt skj_mt yft_mt blm_mt bum_mt mls_mt swo_mt ham_mt mak_mt ocs_mt por_mt fal_mt thr_mt
  # 1950    H   PH            0      0      0      0   1196     32    508      0     19      0      0      0      0      0      0
  # 1950    K   PH            0      0      0   1056   4784      0      0      0      0      0      0      0      0      0      0
  # 1950    L   JP    DW  16713  17463      0      0  12575      0      0      0      0      0      0      0      0      0      0
  # 1950    L   US    HW     27    781      0     34    269      0      0      0      0      0      0      0      0      0      0
  # 1950    O   ID            0      0      0   2645    625      0      0      0      0      0      0      0      0      0      0
  # 1950    O   PH            0      0      0   2782   2314      0      0      0      0      0      0      0      0      0      0
  
  # Catch: final data sample:
  # FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #   AU    L 1985-01-01 1986-01-01    WCPFC    ALL     YFT       ALL         MT     9
  #   AU    L 1986-01-01 1987-01-01    WCPFC    ALL     BET       ALL         MT     1
  #   AU    L 1986-01-01 1987-01-01    WCPFC    ALL     YFT       ALL         MT    13
  #   AU    L 1987-01-01 1988-01-01    WCPFC    ALL     ALB       ALL         MT   129
  #   AU    L 1987-01-01 1988-01-01    WCPFC    ALL     BET       ALL         MT    64
  #   AU    L 1987-01-01 1988-01-01    WCPFC    ALL     BLM       ALL         MT    17
function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/format_time_db_format.R")
#packages

  
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
}
if(!require(lubridate)){
    install.packages("lubridate")
    require(lubridate)
}

  if(!require(data.table)){
    install.packages("data.table")
    require(data.table)
  }  
  if(!require(reshape2)){
    install.packages("reshape2")
    require(reshape2)
  }
 #----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename1 <- entity$data$source[[1]] #WCPFC data
filename2 <- entity$data$source[[2]] #WCPO data
filename3 <- entity$data$source[[3]] #structure
path_to_raw_dataset1 <- entity$getJobDataResource(config, filename1) #WCPFC data
path_to_raw_dataset2 <- entity$getJobDataResource(config, filename2) #WCPO data
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------


### Nominal catches
#from wcpfc
wcpfc_species = c("ALV", "BLM", "BSH", "BTH", "BUM", "FAL", "LMA", "MAK", "OCS", "POR", "PTH", "RHN", "SMA", "SPK", "SPL", "SPN", "SPY", "SPZ", "THR")
NC1<-read.csv(path_to_raw_dataset1)
NC1<-NC1[NC1$SP_CODE %in% wcpfc_species,]
#from wcpo
wcpo_species = c("ALB", "BET", "MLS", "PBF", "SKJ", "SWO", "YFT")
NC2<-read.csv(path_to_raw_dataset2)
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
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(
	paste0(format(min(NC$time_start), "%Y"), "-01-01"),
	paste0(format(max(NC$time_end), "%Y"), "-12-31"),
	sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

#@geoflow -> export as csv
output_name_dataset <- file.path(dirname(filename1), paste0(entity$identifiers$id, "_harmonized.csv"))
write.csv(NC, output_name_dataset, row.names = FALSE)
output_name_codelists <- file.path(dirname(filename1), paste0(entity$identifiers$id, "_codelists.csv"))
file.rename(from = entity$getJobDataResource(config, filename3), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", c(path_to_raw_dataset1, path_to_raw_dataset2))
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)}
