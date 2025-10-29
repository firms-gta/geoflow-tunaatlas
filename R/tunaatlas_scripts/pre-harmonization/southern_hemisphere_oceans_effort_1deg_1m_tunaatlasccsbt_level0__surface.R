#' Harmonize CCSBT Surface Effort Datasets
#'
#' Harmonizes the structure of CCSBT catch-and-effort datasets specifically for 'Surface' effort data.
#' This involves reading the raw dataset, performing various data transformations including renaming,
#' mutating, and selecting columns, and calculating new fields. The output is a harmonized dataset suitable
#' for integration into the Tuna Atlas database.
#'
#' @param action Action parameter, currently unused but reserved for future extension.
#' @param entity An entity object that contains dataset metadata and methods for data resource management.
#' @param config A configuration object that includes settings and utilities, such as logging.
#'
#' @details
#' The function performs the following key steps:
#' - Reads the raw dataset and metadata file from specified paths.
#' - Applies renaming and transformation logic to align with the database schema.
#' - Calculates conversion factors and standardizes time format.
#' - Generates harmonized dataset and metadata files, which are then saved to CSV.
#'
#' The harmonization process is tailored to CCSBT 'Surface' datasets and assumes specific column names
#' and data structure. It enriches the entity with temporal coverage information based on the dataset's
#' date range and exports the harmonized data, metadata, and code lists as CSV files.
#'
#' @return Void. The function does not return a value but writes output files and updates the entity object.
#'
#' @examples
#' \dontrun{
#'   harmonize_ccsbt_surface_effort(action, entity, config)
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise
#' @importFrom readxl read_excel
#' @importFrom lubridate as_date ceiling_date
#' @export
#' 
#' 

function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_spatial_5.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/format_time_db_format.R")
  
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  
  
  if(!require(readxl)){
    install.packages("readxl")
    require(readxl)
  }

# Input data sample (after importing as data.frame in R):
# YEAR MONTH COUNTRY_CODE GEAR_CODE CCSBT_STATISTICAL_AREA LATITUDE LONGITUDE NUMBER_OF_HOURS_SEARCHED WEIGHT_(Kg)_OF_SBT_RETAINED
# 1975    12           AU        PS                      4      -37       150                       17                       59800
# 1976     8           AU        BB                      4      -36       150                        8                           0
# 1976     9           AU        BB                      4      -37       150                       14                       36200
# 1976     9           AU        BB                      4      -35       150                        4                           0
# 1976     9           AU        BB                      4      -34       151                       21                           0
# 1976    10           AU        BB                      4      -37       150                       51                       41000

# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#   AU   BB 1976-08-01 1976-09-01  6336150    ALL       HRSRH      8
#   AU   BB 1976-09-01 1976-10-01  6334151    ALL       HRSRH     21
#   AU   BB 1976-09-01 1976-10-01  6335150    ALL       HRSRH      4
#   AU   BB 1976-09-01 1976-10-01  6337150    ALL       HRSRH     14
#   AU   BB 1976-10-01 1976-11-01  6334151    ALL       HRSRH     21
#   AU   BB 1976-10-01 1976-11-01  6336150    ALL       HRSRH     29

  #----------------------------------------------------------------------------------------------------------------------------
  #@geoflow --> with this script 2 objects are pre-loaded
  #config --> the global config of the workflow
  #entity --> the entity you are managing
  #get data from geoflow current job dir
  filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  CEData_Surface.xlsx
  filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  ccsbt_effort_code_lists.csv
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  

  RFMO_CE<-readxl::read_excel(path_to_raw_dataset, sheet = "CEData_Surface", col_names = TRUE, col_types = NULL,na = "")
  
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  colnames(RFMO_CE)<-gsub("\r\n", "_", colnames(RFMO_CE))
  colnames(RFMO_CE)<-gsub(" ", "_", colnames(RFMO_CE))
  colnames(RFMO_CE)<-gsub("\\(", "", colnames(RFMO_CE))
  colnames(RFMO_CE)<-gsub("\\)", "", colnames(RFMO_CE))
  RFMO_CE<-as.data.frame(RFMO_CE)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  
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
  RFMO_CE$School<-"UNK"
  
  #Species
  RFMO_CE$Species<-"SBF"
  
  #CatchType
  RFMO_CE$CatchType<-"UNK" #not used later as it is no catch

  
efforts<-RFMO_CE

efforts$EffortUnits<-"NUMBER_OF_HOURS_SEARCHED"
efforts$Effort<-efforts$NUMBER_OF_HOURS_SEARCHED
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
efforts <-efforts[colToKeep_efforts]


#remove whitespaces on columns that should not have withespace

efforts[,c("AreaName","FishingFleet")]<-as.data.frame(apply(efforts[,c("AreaName","FishingFleet")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)

# remove 0 and NA values 

efforts <- efforts  %>% 
  filter( ! Effort %in% 0 ) %>%
  filter( ! is.na(Effort)) 

efforts <- efforts %>% 
  dplyr::group_by(FishingFleet,Gear,time_start,time_end,AreaName,School,EffortUnits) %>% 
  dplyr::summarise(Effort = sum(Effort))  
efforts<-as.data.frame(efforts)

config$logger.info(sprintf("colnumbers",ncol(efforts)))

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"CCSBT"
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
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(efforts, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
