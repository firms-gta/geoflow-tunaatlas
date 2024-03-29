######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = southern_hemisphere_oceans_effort_5deg_1m_ll_tunaatlasccsbt_level0, title = Harmonize data structure of CCSBT Longline effort datasets, abstract = Harmonize the structure of CCSBT catch-and-effort datasets: 'Longline' (pid of output file = southern_hemisphere_oceans_effort_5deg_1m_ll_tunaatlasccsbt_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/M4kiWy, value = "https://goo.gl/M4kiWy";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Commission for the Conservation of Southern Bluefin Tuna CCSBT tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_ccsbt_ce_Surface}} to convert CCSBT task 2 Surface data structure, \code{\link{convertDSD_ccsbt_nc_annual_catches_by_gear}} to convert CCSBT nominal catch data structure

function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_5.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
  
    
  if(!require(readxl)){
    install.packages("readxl")
    require(readxl)
  }
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  

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

  RFMO_CE<-readxl::read_excel(path_to_raw_dataset, sheet = "CEData_Longline", col_names = TRUE, col_types = NULL,na = "")
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
  RFMO_CE$School<-"ALL"
  
  #Species
  RFMO_CE$Species<-"SBF"
  
  #CatchType
  RFMO_CE$CatchType<-"ALL"
  

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
