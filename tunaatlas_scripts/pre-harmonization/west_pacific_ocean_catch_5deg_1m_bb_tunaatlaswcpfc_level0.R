######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = west_pacific_ocean_catch_5deg_1m_bb_tunaatlaswcpfc_level0, title = Harmonize data structure of WCPFC Pole-and-line catch datasets, abstract = Harmonize the structure of WCPFC catch-and-effort datasets: 'Pole-and-line' (pid of output file = west_pacific_ocean_catch_5deg_1m_bb_tunaatlaswcpfc_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/niIjsk, value = "https://goo.gl/niIjsk";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' This script works with any dataset that has the first 5 columns named and ordered as follow: {YY|MM|LAT5|LON5|DAYS} followed by a list of columns specifing the species codes with "_N" for catches expressed in number and "_T" for catches expressed in tons
#' 
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Western and Central Pacific Fisheries Commission WCPFC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_wcpfc_ce_Driftnet}} to convert WCPFC task 2 Drifnet data structure, \code{\link{convertDSD_wcpfc_ce_Longline}} to convert WCPFC task 2 Longline data structure, \code{\link{convertDSD_wcpfc_ce_Pole_and_line}} to convert WCPFC task 2 Pole-and-line data structure, \code{\link{convertDSD_wcpfc_ce_PurseSeine}} to convert WCPFC task 2 Purse seine data structure, \code{\link{convertDSD_wcpfc_nc}} to convert WCPFC task 1 data structure  

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



if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
}
if(!require(foreign)){
  install.packages("foreign")
}

require(rtunaatlas)
require(foreign)

#wd<-getwd()
#download.file(path_to_raw_dataset,destfile=paste(wd,"/dbf_file.DBF",sep=""), method='auto', quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
#path_to_raw_dataset=paste(wd,"/dbf_file.DBF",sep="")


# Input data sample:
# YY MM LAT5 LON5 DAYS SKJ_C YFT_C OTH_C
# 1950  1  30N 135E    0     0     0     0
# 1950  1  30N 140E    0     0     0     0
# 1950  1  35N 140E    0     0     0     0
# 1950  1  40N 140E    0     0     0     0
# 1950  1  40N 145E    0     0     0     0
# 1950  2  30N 135E    0     0     0     0

# Catch: pivot data sample:
# YY MM LAT5 LON5 Effort Species  value CatchUnits School EffortUnits Gear
# 1970  3  05S 150E     82     SKJ 279.16         MT    ALL        DAYS    P
# 1970  4  05S 150E     74     SKJ 336.61         MT    ALL        DAYS    P
# 1970  5  05S 150E     82     SKJ 361.80         MT    ALL        DAYS    P
# 1970  6  05S 150E     81     SKJ 438.48         MT    ALL        DAYS    P
# 1970  7  05S 150E     75     SKJ 472.75         MT    ALL        DAYS    P
# 1970 12  05S 150E     56     SKJ 215.05         MT    ALL        DAYS    P


# Catch: final data sample:
# Flag Gear time_start   time_end AreaName School Species CatchType CatchUnits  Catch
#  ALL    P 1970-03-01 1970-04-01  6200150    ALL     OTH       ALL         MT   0.01
#  ALL    P 1970-03-01 1970-04-01  6200150    ALL     SKJ       ALL         MT 279.16
#  ALL    P 1970-03-01 1970-04-01  6200150    ALL     YFT       ALL         MT  27.81
#  ALL    P 1970-04-01 1970-05-01  6200150    ALL     OTH       ALL         MT   0.14
#  ALL    P 1970-04-01 1970-05-01  6200150    ALL     SKJ       ALL         MT 336.61
#  ALL    P 1970-04-01 1970-05-01  6200150    ALL     YFT       ALL         MT  11.34


##Catches

### Reach the catches pivot DSD using a function stored in WCPFC_functions.R
catches_pivot_WCPFC<-FUN_catches_WCPFC_CE_allButPurseSeine (path_to_raw_dataset)
catches_pivot_WCPFC$Gear<-"P"

# Catchunits
index.kg <- which( catches_pivot_WCPFC[,"CatchUnits"] == "C" )
catches_pivot_WCPFC[index.kg,"CatchUnits"]<- "MT"

index.nr <- which( catches_pivot_WCPFC[,"CatchUnits"] == "N" )
catches_pivot_WCPFC[index.nr,"CatchUnits"]<- "NO" 

# School
catches_pivot_WCPFC$School<-"ALL"

### Reach the catches harmonized DSD using a function in WCPFC_functions.R
colToKeep_captures <- c("Flag","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches<-WCPFC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_WCPFC,colToKeep_captures)

colnames(catches)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"WCPFC"

#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
catches$time_start <- as.Date(catches$time_start)
catches$time_end <- as.Date(catches$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(as.character(min(catches$time_start)), as.character(max(catches$time_end)), sep = "/")
entity$setTemporalExtent(dataset_temporal_extent)

#@geoflow -> export as csv
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".DBF"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".DBF"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
