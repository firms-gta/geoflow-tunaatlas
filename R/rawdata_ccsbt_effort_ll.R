#' Harmonize CCSBT Longline Effort Datasets
#'
#' This function harmonizes CCSBT Longline effort datasets for integration into the Tuna Atlas database, ensuring data compliance with specified format requirements.
#'
#' @details This function modifies the effort dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @seealso \code{harmo_time_2()}
#'          \code{harmo_spatial_5()}
#'          \code{format_time_db_format()}
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (ccsbt/effort/ll/action_ccsbt_effort_ll.R).
#' @export
gta_rawdata_ccsbt_effort_ll <- function(data) {
  
  

# Input data sample (after importing as data.frame in R):
# YEAR MONTH COUNTRY_CODE TARGET_SPECIES CCSBT_STATISTICAL_AREA LATITUDE LONGITUDE NUMBER_OF_HOOKS NUMBER_OF_SBT_RETAINED
# 1965     1           JP             NA                      1      -15       100            2083                      4
# 1965     1           JP             NA                      1      -15       110            9647                      0
# 1965     1           JP             NA                      1      -15       115           91431                    525
# 1965     1           JP             NA                      1      -10       100           23560                     56
# 1965     1           JP             NA                      1      -10       105           31232                     35
# 1965     1           JP             NA                      1      -10       110            4960                     10

# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#   AU   LL 1986-11-01 1986-12-01  6330150    ALL       HOOKS   3520
#   AU   LL 1986-11-01 1986-12-01  6335150    ALL       HOOKS   5970
#   AU   LL 1986-12-01 1987-01-01  6335150    ALL       HOOKS   5150
#   AU   LL 1987-01-01 1987-02-01  6330150    ALL       HOOKS   1840
#   AU   LL 1987-01-01 1987-02-01  6335150    ALL       HOOKS  14740
#   AU   LL 1987-02-01 1987-03-01  6335150    ALL       HOOKS  17300

  #----------------------------------------------------------------------------------------------------------------------------
  #config --> the global config of the workflow
# Historical name for the dataset at source  CEData_Longline.xlsx
# Historical name for the dataset at source  ccsbt_effort_code_lists.csv
  

  RFMO_CE <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(readxl::read_excel(path_to_raw_dataset, sheet = "CEData_Longline", col_names = TRUE, col_types = NULL,na = "")))
  colnames(RFMO_CE)<-gsub("\r\n", "_", colnames(RFMO_CE))
  colnames(RFMO_CE)<-gsub(" ", "_", colnames(RFMO_CE))
  RFMO_CE<-as.data.frame(RFMO_CE)
  
  #Remove lines that are read in the Excel but that are not real
  RFMO_CE<- RFMO_CE[!is.na(RFMO_CE$YEAR),]
  RFMO_CE$NUMBER_OF_SBT_RETAINED<-as.numeric(RFMO_CE$NUMBER_OF_SBT_RETAINED)
  
  #FishingFleet
  RFMO_CE$FishingFleet<-RFMO_CE$COUNTRY_CODE
  
  #Gear
  RFMO_CE$Gear<-"Longline"
  
  #Year and period
  RFMO_CE<-harmo_time_2(RFMO_CE, "YEAR", "MONTH")
  #Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
  RFMO_CE<-format_time_db_format(RFMO_CE)
  
  # Area 
  RFMO_CE<-harmo_spatial_5(RFMO_CE,"LATITUDE","LONGITUDE",5,6)
  
  #School
  RFMO_CE$School<-"UNK"
  
  #Species
  RFMO_CE$Species<-"SBF"
  
  #CatchType
  RFMO_CE$CatchType<-"UNK" #not used later as it is no catch
  

efforts<-RFMO_CE

efforts$EffortUnits<-"NUMBER_OF_HOOKS"
efforts$Effort<-efforts$NUMBER_OF_HOOKS

colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
efforts <-efforts[colToKeep_efforts]


#remove whitespaces on columns that should not have withespace
efforts[,c("AreaName","FishingFleet")]<-as.data.frame(apply(efforts[,c("AreaName","FishingFleet")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)

# remove 0 and NA values 
efforts <- efforts  %>% 
  dplyr::filter( ! Effort %in% 0 ) %>%
  dplyr::filter( ! is.na(Effort)) 

efforts <- efforts %>% 
  dplyr::group_by(FishingFleet,Gear,time_start,time_end,AreaName,School,EffortUnits) %>% 
  dplyr::summarise(Effort = sum(Effort))  
efforts<-as.data.frame(efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"CCSBT"
efforts$measurement <- "effort" 
efforts$measurement_processing_level <- "unknown" 

#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
  efforts
}
