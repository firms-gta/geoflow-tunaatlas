######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = catch_1deg_1m_ps_iccat_level0__byschool, title = Harmonize data structure of ICCAT by operation mode catch datasets, abstract = Harmonize the structure of ICCAT catch-and-effort datasets: 'by operation mode' (pid of output file = atlantic_ocean_catch_1deg_1m_ps_tunaatlasiccat_level0__bySchool). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/dDXf5D, value = "https://goo.gl/dDXf5D";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.in: id = keep_fleet_instead_of_flag, type = Boolean, title = By default the column "flag" is kept. By setting this argument to TRUE the column "fleet" will be kept (and "flag" will be removed), value = FALSE;
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Internal Commission for the Conservation of Atlantic Tuna tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iccat_ce_task2}} to convert ICCAT task 2 , \code{\link{convertDSD_iccat_nc}} to convert ICCAT nominal catch data structure

# Input data sample (after importing as data.frame in R):
# DSetID StrataID      Flag      FleetCode GearCode YearC TimePeriodID SquareTypeCode QuadID Lat Lon Eff1  Eff1Type  Eff2  Eff2Type Eff3  Eff3Type Eff4  Eff4Type Eff5  Eff5Type YFTfd ALBfd BETfd BLFfd LTAfd  SKJfd
#   2791   502973 EU.España EU.ESP-ES-ETRO       PS  1991            1            1x1      1   1   3 25.2 FISH.HOUR  50.2 HOURS.SEA 25.2 Hours.STD 6.83 Hours.FAD    0 Hours.FSC  7110     0  9500     0     0  57620
#   2791   502975 EU.España EU.ESP-ES-ETRO       PS  1991            1            1x1      3   0   3 18.7 FISH.HOUR  37.6 HOURS.SEA 18.7 Hours.STD 1.92 Hours.FAD    0 Hours.FSC   720     0  1180     0     0   8540
#   2791   502978 EU.España EU.ESP-ES-ETRO       PS  1991            1            1x1      3   0   9 25.3 FISH.HOUR  50.2 HOURS.SEA 25.3 Hours.STD 2.83 Hours.FAD    0 Hours.FSC  3940     0  5560     0     0  35460
#   2791   502979 EU.España EU.ESP-ES-ETRO       PS  1991            1            1x1      3   0  10 50.5 FISH.HOUR 100.3 HOURS.SEA 50.5 Hours.STD 5.27 Hours.FAD    0 Hours.FSC 10140     0 16610     0     0 119600
#   2791   502980 EU.España EU.ESP-ES-ETRO       PS  1991            1            1x1      3   0  11 25.3 FISH.HOUR  50.2 HOURS.SEA 25.3 Hours.STD 6.17 Hours.FAD    0 Hours.FSC  7960     0 13060     0     0  93970
#   2791   502981 EU.España EU.ESP-ES-ETRO       PS  1991            1            1x1      3   0  12 25.3 FISH.HOUR  50.2 HOURS.SEA 25.3 Hours.STD 6.81 Hours.FAD    0 Hours.FSC  9770     0 16010     0     0 115320
# FRIfd YFTfs ALBfs BETfs BLFfs LTAfs SKJfs FRIfs
#     0     0     0     0     0     0     0     0
#     0     0     0     0     0     0     0     0
#  2090     0     0     0     0     0     0     0
#     0     0     0     0     0     0     0     0
#     0     0     0     0     0     0     0     0
#     0     0     0     0     0     0     0     0
  
  
# Catch: final data sample:
# FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
# Belize   PS 2009-08-01 2009-09-01  5402000     FS     BET         C         MT  9.51
# Belize   PS 2009-08-01 2009-09-01  5402000     FS     YFT         C         MT 98.58
# Belize   PS 2009-09-01 2009-10-01  5202006     LS     BET         C         MT  0.38
# Belize   PS 2009-09-01 2009-10-01  5202006     LS     SKJ         C         MT 15.76
# Belize   PS 2009-09-01 2009-10-01  5202006     LS     YFT         C         MT  2.65
# Belize   PS 2009-09-01 2009-10-01  5202008     LS     BET         C         MT  3.60
function(action, entity, config){
  
#packages

  
if(!require(readr)){
  install.packages("readr")
  require(readr)
}
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
#----------------------------------------------------------------------------------------------------------------------------

keep_fleet_instead_of_flag=FALSE
  
##Catches
#RFMO_CE<-read_excel(path_to_raw_dataset, sheet = "ds_t2cePSbySchool", col_names = TRUE, col_types = NULL,na = "", skip = 6)
#RFMO_CE<-as.data.frame(RFMO_CE)
RFMO_CE<-read.csv(path_to_raw_dataset,stringsAsFactors = F)
names(RFMO_CE)[names(RFMO_CE) == 'Flag'] <- 'FishingFleet'

## If we want in the output dataset the column 'FleetCode' instead of 'flag'
if(keep_fleet_instead_of_flag==TRUE){
RFMO_CE$FishingFleet<-NULL
names(RFMO_CE)[names(RFMO_CE) == 'FleetCode'] <- 'FishingFleet'
}
  
  
ICCAT_CE_species_colnames<-c("YFTfd","ALBfd","BETfd","BLFfd","LTAfd","SKJfd","FRIfd","YFTfs","ALBfs","BETfs","BLFfs","LTAfs","SKJfs","FRIfs")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_catches_ICCAT_CE.R")
catches_pivot_ICCAT<-FUN_catches_ICCAT_CE(RFMO_CE,ICCAT_CE_species_colnames)

#School
catches_pivot_ICCAT$School<-"init" 
index.FS <-grep("fs",catches_pivot_ICCAT$variable)
index.LS <-grep("fd",catches_pivot_ICCAT$variable)

catches_pivot_ICCAT[index.FS,"School"]<- "fs"
catches_pivot_ICCAT[index.LS,"School"]<- "fd"

catches_pivot_ICCAT$variable<-gsub("fs","",catches_pivot_ICCAT$variable)
catches_pivot_ICCAT$variable<-gsub("fd","",catches_pivot_ICCAT$variable)

#Catchunit
catches_pivot_ICCAT$CatchUnits<-"MT"

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/ICCAT_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-ICCAT_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_ICCAT,colToKeep_captures)

colnames(catches)<-c("fishingfleet","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"ICCAT"


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
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)

}
