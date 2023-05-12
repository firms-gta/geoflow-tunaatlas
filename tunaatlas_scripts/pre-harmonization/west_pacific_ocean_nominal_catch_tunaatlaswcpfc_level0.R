######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = nominal_catch_wcpfc_level0, title = Harmonize data structure of WCPFC nominal catch, abstract = Harmonize the structure of WCPFC nominal catch dataset (pid of output file = west_pacific_ocean_nominal_catch_tunaatlaswcpfc_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/KfUXpF, value = "https://goo.gl/KfUXpF";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv. , value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

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
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
#packages

  
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
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


### Nominal catches
NC<-read.csv(path_to_raw_dataset)
NC <- as.data.frame(NC)

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

NC$AreaName<-"WCPFC"
NCAreaCWPgrid<-NA
NC$School<-"ALL"
NC$CatchType<-"ALL"
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
#NC <- NC  %>% 
#  filter( ! Catch %in% 0 ) %>%
#  filter( ! is.na(Catch)) 
# remove 0 and NA values 
NC <- NC[!is.na(NC$Catch),]
NC <- NC[NC$Catch != 0,]

#NC <- NC %>% 
#  group_by(FishingFleet,Gear,time_start,time_end,AreaName,School,Species,CatchType,CatchUnits) %>% 
#  summarise(Catch = sum(Catch))
#NC<-as.data.frame(NC)
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


colnames(NC)<-c("fishingfleet","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
NC$source_authority<-"WCPFC"

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
entity$addResource("codelists", output_name_codelists)}
