######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = nominal_catch_ccsbt_level0__bygear, title = Harmonize data structure of CCSBT nominal catch, abstract = Harmonize the structure of CCSBT nominal catch dataset (pid of output file = southern_hemisphere_oceans_nominal_catch_tunaatlasccsbt_level0__bygear). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/033iit, value = "https://goo.gl/033iit";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv. , value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 


#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Commission for the Conservation of Southern Bluefin Tuna CCSBT tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_ccsbt_ce_Longline}} to convert CCSBT task 2 Longline data structure, \code{\link{convertDSD_ccsbt_ce_Surface}} to convert CCSBT task 2 Surface data structure

##Catches
  
  
  # Input data sample (after importing as data.frame in R):
  #`Calendar\r\nYear` Longline `Purse\r\nSeine` `Pole\r\nand\r\nLine`  Trol Handline `Gill\r\nNet` Other
  #               1952      565              264                    NA    NA        0             0     0
  #               1953     3890              509                    NA    NA        0             0     0
  #               1954     2447              424                    NA    NA        0             0     0
  #               1955     1964              322                    NA    NA        0             0     0
  #               1956     9603              964                    NA    NA        0             0     0
  #               1957    22908             1264                    NA    NA        0             0     0
  
  
  # final data sample:
  # FishingFleet     Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #  ALL Gill Net 1982-01-01 1983-01-01    CCSBT    ALL     SBF       ALL         MT    11
  #  ALL Gill Net 1983-01-01 1984-01-01    CCSBT    ALL     SBF       ALL         MT    12
  #  ALL Gill Net 1985-01-01 1986-01-01    CCSBT    ALL     SBF       ALL         MT    67
  #  ALL Gill Net 1986-01-01 1987-01-01    CCSBT    ALL     SBF       ALL         MT    81
  #  ALL Gill Net 1987-01-01 1988-01-01    CCSBT    ALL     SBF       ALL         MT    87
  #  ALL Gill Net 1988-01-01 1989-01-01    CCSBT    ALL     SBF       ALL         MT   234
function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
#packages

  
if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
}
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
filename2 <- entity$data$source[[2]] #structure
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------

CCSBT_NC<-readxl::read_excel(path_to_raw_dataset, sheet = "Catch by Gear", col_names = TRUE, col_types = NULL,na = "",skip=7)
colnames(CCSBT_NC)<-gsub("\r\n", "_", colnames(CCSBT_NC))
colnames(CCSBT_NC)<-gsub(" ", "_", colnames(CCSBT_NC)) 

CCSBT_NC<-as.data.frame(CCSBT_NC)
# CCSBT_NC<-reshape::melt(CCSBT_NC, id.vars="Calendar_Year") 
CCSBT_NC <- CCSBT_NC %>% tidyr::gather(variable, value, -"Calendar_Year")

CCSBT_NC$variable<-as.character(CCSBT_NC$variable)

CCSBT_NC$variable<-gsub("_", " ", CCSBT_NC$variable)

#FishingFleet
CCSBT_NC$FishingFleet<-"ALL"

#Gear
CCSBT_NC$Gear<-CCSBT_NC$variable



#Year and period
CCSBT_NC$Year<-CCSBT_NC$Calendar_Year
CCSBT_NC$MonthStart<-1
CCSBT_NC$Period<-12
#Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
CCSBT_NC<-format_time_db_format(CCSBT_NC)

# Area 
CCSBT_NC$AreaName<-"CCSBT"

#School
CCSBT_NC$School<-"ALL"

#Species
CCSBT_NC$Species<-"SBF"

#CatchType
CCSBT_NC$CatchType<-"ALL"



#Catch
CCSBT_NC$Catch<-CCSBT_NC$value

CCSBT_NC$CatchUnits<-"MT"

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
NC <-CCSBT_NC[,colToKeep_captures]


#remove whitespaces on columns that should not have withespace
NC[,c("AreaName","FishingFleet")]<-as.data.frame(apply(NC[,c("AreaName","FishingFleet")],2,function(x){gsub(" *$","",x)}),stringsAsFactors=FALSE)

# remove 0 and NA values 
NC <- NC[NC$Catch != 0,]
NC <- NC[!is.na(NC$Catch),] 

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
NC$source_authority<-"CCSBT"

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
