#' Harmonize CCSBT Surface Catch Datasets
#'
#' This function harmonizes the CCSBT Surface catch datasets,
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
#' @import readxl
#' @importFrom stringr str_replace
#' @seealso \code{\link{harmo_time_2}} 
#'          \code{\link{harmo_spatial_5}} 
#'          \code{\link{format_time_db_format}} 
#' @export
#' @keywords data harmonization, fisheries, CCSBT, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
# Input data sample (after importing as data.frame in R):
  # YEAR MONTH COUNTRY_CODE GEAR_CODE CCSBT_STATISTICAL_AREA LATITUDE LONGITUDE NUMBER_OF_HOURS_SEARCHED WEIGHT_(Kg)_OF_SBT_RETAINED
  # 1975    12           AU        PS                      4      -37       150                       17                       59800
  # 1976     8           AU        BB                      4      -36       150                        8                           0
  # 1976     9           AU        BB                      4      -37       150                       14                       36200
  # 1976     9           AU        BB                      4      -35       150                        4                           0
  # 1976     9           AU        BB                      4      -34       151                       21                           0
  # 1976    10           AU        BB                      4      -37       150                       51                       41000
  
  
  # Catch: final data sample:
  # FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #   AU   BB 1976-09-01 1976-10-01  6337150    ALL     SBT       ALL         MT  36.2
  #   AU   BB 1976-10-01 1976-11-01  6337150    ALL     SBT       ALL         MT  41.0
  #   AU   BB 1976-12-01 1977-01-01  6332132    ALL     SBT       ALL         MT 167.5
  #   AU   BB 1976-12-01 1977-01-01  6333134    ALL     SBT       ALL         MT  35.6
  #   AU   BB 1976-12-01 1977-01-01  6334134    ALL     SBT       ALL         MT  56.5
  #   AU   BB 1976-12-01 1977-01-01  6334135    ALL     SBT       ALL         MT  37.0
function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_5.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
  #packages

  

if(!require(readxl)){
	install.packages("readxl")
	require(readxl)
}

#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  CEData_Surface.xlsx, if multiple, this means this function is used for several dataset, keep the same order to match data
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  ccsbt_catch_code_lists.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------  
  
RFMO_CE<-readxl::read_excel(path_to_raw_dataset, sheet = "CEData_Surface", col_names = TRUE, col_types = NULL,na = "")
colnames(RFMO_CE)<-gsub("\r\n", "_", colnames(RFMO_CE))
colnames(RFMO_CE)<-gsub(" ", "_", colnames(RFMO_CE))
colnames(RFMO_CE)<-gsub("\\(", "", colnames(RFMO_CE))
colnames(RFMO_CE)<-gsub("\\)", "", colnames(RFMO_CE))
RFMO_CE<-as.data.frame(RFMO_CE)

#Remove lines that are read in the Excel but that are not real
RFMO_CE<- RFMO_CE[!is.na(RFMO_CE$YEAR),] 
RFMO_CE$WEIGHT_Kg_OF_SBT_RETAINED<-as.numeric(RFMO_CE$WEIGHT_Kg_OF_SBT_RETAINED)
RFMO_CE$NUMBER_OF_HOURS_SEARCHED<-as.numeric(RFMO_CE$NUMBER_OF_HOURS_SEARCHED)

#FishingFleet
RFMO_CE$FishingFleet<-RFMO_CE$COUNTRY_CODE

#Gear
RFMO_CE$Gear<-RFMO_CE$GEAR_CODE
# replace PS by Purse Seine and BB by Pole and Line
RFMO_CE$Gear[RFMO_CE$Gear == "PS"] <- "Purse Seine"
RFMO_CE$Gear[RFMO_CE$Gear == "BB"] <- "Pole and Line"

#Year and period
RFMO_CE<-harmo_time_2(RFMO_CE, "YEAR", "MONTH")
#Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
RFMO_CE<-format_time_db_format(RFMO_CE)

# Area 
RFMO_CE<-harmo_spatial_5(RFMO_CE,"LATITUDE","LONGITUDE",1,5)

#School
RFMO_CE$School<-"ALL"

#Species
RFMO_CE$Species<-"SBF"

#CatchType
RFMO_CE$CatchType<-"ALL"



#Catch
RFMO_CE$Catch<-RFMO_CE$WEIGHT_Kg_OF_SBT_RETAINED/1000

RFMO_CE$CatchUnits<-"t"

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches <-RFMO_CE[,colToKeep_captures]


#remove whitespaces on columns that should not have withespace
catches[,c("AreaName","FishingFleet")]<-as.data.frame(apply(catches[,c("AreaName","FishingFleet")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)

# remove 0 and NA values 
catches <- catches[!is.na(catches$Catch),]
catches <- catches[catches$Catch != 0,]


catches <- aggregate(catches$Catch, FUN = sum,
	by = list(
		FishingFleet = catches$FishingFleet,
		Gear = catches$Gear,
		time_start = catches$time_start,
		time_end = catches$time_end,
		AreaName = catches$AreaName,
		School = catches$School,
		Species = catches$Species,
		CatchType = catches$CatchType,
		CatchUnits = catches$CatchUnits
	)
)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"CCSBT"
catches$measurement_type <- "RC" # Retained catches
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
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".xlsx"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".xlsx"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
