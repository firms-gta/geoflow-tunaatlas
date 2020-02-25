######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = southern_hemisphere_oceans_catch_5deg_1m_ll_tunaatlasccsbt_level0, title = Harmonize data structure of CCSBT Longline catch datasets, abstract = Harmonize the structure of CCSBT catch-and-effort datasets: 'Longline' (pid of output file = southern_hemisphere_oceans_catch_5deg_1m_ll_tunaatlasccsbt_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/M4kiWy, value = "https://goo.gl/M4kiWy";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Commission for the Conservation of Southern Bluefin Tuna CCSBT tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_ccsbt_ce_Surface}} to convert CCSBT task 2 Surface data structure, \code{\link{convertDSD_ccsbt_nc_annual_catches_by_gear}} to convert CCSBT nominal catch data structure

#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename <- entity$data$source[[1]]
path_to_raw_dataset <- entity$getJobDataResource(config, filename)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------


if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
}
require(rtunaatlas)
  
  # Input data sample (after importing as data.frame in R):
  # YEAR MONTH COUNTRY_CODE TARGET_SPECIES CCSBT_STATISTICAL_AREA LATITUDE LONGITUDE NUMBER_OF_HOOKS NUMBER_OF_SBT_RETAINED
  # 1965     1           JP             NA                      1      -15       100            2083                      4
  # 1965     1           JP             NA                      1      -15       110            9647                      0
  # 1965     1           JP             NA                      1      -15       115           91431                    525
  # 1965     1           JP             NA                      1      -10       100           23560                     56
  # 1965     1           JP             NA                      1      -10       105           31232                     35
  # 1965     1           JP             NA                      1      -10       110            4960                     10
  
  
  # Catch: final data sample:
  # Flag Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #   AU   LL 1987-03-01 1987-04-01  6330130    ALL     SBF       ALL         NO     6
  #   AU   LL 1987-05-01 1987-06-01  6335150    ALL     SBF       ALL         NO     1
  #   AU   LL 1987-06-01 1987-07-01  6330150    ALL     SBF       ALL         NO     2
  #   AU   LL 1987-06-01 1987-07-01  6335150    ALL     SBF       ALL         NO    47
  #   AU   LL 1987-07-01 1987-08-01  6325150    ALL     SBF       ALL         NO     1
  #   AU   LL 1987-09-01 1987-10-01  6330150    ALL     SBF       ALL         NO    14
  

##Catches

### Reach the catches pivot DSD using a function stored in ICCAT_functions.R
  #RFMO_CE<-read_excel(path_to_raw_dataset, sheet = "CEData_Longline", col_names = TRUE, col_types = NULL,na = "")
  RFMO_CE<-read.csv(path_to_raw_dataset,stringsAsFactors = F)
  
#colnames(RFMO_CE)<-gsub("\r\n", "_", colnames(RFMO_CE))
  #colnames(RFMO_CE)<-gsub(".", "_", colnames(RFMO_CE))

#RFMO_CE<-as.data.frame(RFMO_CE)
#Remove lines that are read in the Excel but that are not real
RFMO_CE<- RFMO_CE[!is.na(RFMO_CE$YEAR),] 
RFMO_CE$NUMBER_OF_SBT_RETAINED<-as.numeric(RFMO_CE$NUMBER_OF_SBT_RETAINED)



#Flag
RFMO_CE$Flag<-RFMO_CE$COUNTRY_CODE

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



#Catch
RFMO_CE$Catch<-RFMO_CE$NUMBER_OF_SBT_RETAINED

RFMO_CE$CatchUnits<-"NO"

colToKeep_captures <- c("Flag","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches <-RFMO_CE[colToKeep_captures]


#remove whitespaces on columns that should not have withespace
catches[,c("AreaName","Flag")]<-as.data.frame(apply(catches[,c("AreaName","Flag")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)

# remove 0 and NA values 
#catches <- catches  %>% 
#  filter( ! Catch %in% 0 ) %>%
#  filter( ! is.na(Catch)) 
catches <- catches[!is.na(catches$Catch),]
catches <- catches[catches$Catch != 0,]

#catches <- catches %>% 
#  group_by(Flag,Gear,time_start,time_end,AreaName,School,Species,CatchType,CatchUnits) %>% 
#  summarise(Catch = sum(Catch))
#catches<-as.data.frame(catches)

catches <- aggregate(catches$Catch, FUN = sum,
	by = list(
		Flag = catches$Flag,
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

colnames(catches)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"CCSBT"
#dataset<-catches


### Compute metadata
#if (path_to_metadata_file!="NULL"){
#  source("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/transform/compute_metadata.R")
#} else {
#  df_metadata<-NULL
#  df_codelists<-NULL
#}

## To check the outputs:
# str(dataset)
# str(df_metadata)
# str(df_codelists)


 
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
catches$time_start <- as.Date(catches$time_start)
catches$time_end <- as.Date(catches$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(as.character(min(catches$time_start)), as.character(max(catches$time_end)), sep = "/")
entity$setTemporalExtent(dataset_temporal_extent)
#if there is any entity relation with name 'codelists' we read the file
df_codelists <- NULL
cl_relations <- entity$relations[sapply(entity$relations, function(x){x$name=="codelists"})]
if(length(cl_relations)>0){
	config$logger.info("Appending codelists to pre-harmonization action output")
	df_codelists <- read.csv(cl_relations[[1]]$link)
}
#@geoflow -> output structure as initially used by https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/workflow_etl/scripts/generate_dataset.R
dataset <- list(
	dataset = catches, 
	additional_metadata = NULL, #nothing here
	codelists = df_codelists #in case the entity was provided with a link to codelists
)
#@geoflow -> export as csv
output_name_dataset <- gsub(filename, paste0(unlist(strsplit(filename,".mdb"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(dataset$dataset, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename, paste0(unlist(strsplit(filename,".mdb"))[1], "_codelists.csv"), path_to_raw_dataset)
write.csv(dataset$codelists, output_name_codelists, row.names = FALSE)
#----------------------------------------------------------------------------------------------------------------------------

