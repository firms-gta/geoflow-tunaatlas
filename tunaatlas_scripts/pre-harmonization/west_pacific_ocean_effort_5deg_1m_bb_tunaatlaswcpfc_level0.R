######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = west_pacific_ocean_effort_5deg_1m_bb_tunaatlaswcpfc_level0, title = Harmonize data structure of WCPFC Pole-and-line effort datasets, abstract = Harmonize the structure of WCPFC catch-and-effort datasets: 'Pole-and-line' (pid of output file = west_pacific_ocean_effort_5deg_1m_bb_tunaatlaswcpfc_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
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

function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD.R")

  #packages
  
    
  if(!require(foreign)){
    install.packages("foreign")
    require(foreign)
  }
  
  if(!require(reshape)){
    install.packages("reshape")
    require(reshape)
  }
  
  
  if(!require(tidyr)){
    install.packages("tidyr")
    require(tidyr)
  }
  
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }

# wd<-getwd()
# download.file(path_to_raw_dataset,destfile=paste(wd,"/dbf_file.DBF",sep=""), method='auto', quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
# path_to_raw_dataset=paste(wd,"/dbf_file.DBF",sep="")


  # Input data sample:
  # YY MM LAT5 LON5 DAYS SKJ_C YFT_C OTH_C
  # 1950  1  30N 135E    0     0     0     0
  # 1950  1  30N 140E    0     0     0     0
  # 1950  1  35N 140E    0     0     0     0
  # 1950  1  40N 140E    0     0     0     0
  # 1950  1  40N 145E    0     0     0     0
  # 1950  2  30N 135E    0     0     0     0
  
  # Effort: pivot data sample:
  # YY MM LAT5 LON5 Effort EffortUnits School Gear
  # 1950  1  30N 135E      0        DAYS    ALL    P
  # 1950  1  30N 140E      0        DAYS    ALL    P
  # 1950  1  35N 140E      0        DAYS    ALL    P
  # 1950  1  40N 140E      0        DAYS    ALL    P
  # 1950  1  40N 145E      0        DAYS    ALL    P
  # 1950  2  30N 135E      0        DAYS    ALL    P
  
  # Effort: final data sample:
  # Flag Gear time_start   time_end AreaName School EffortUnits Effort
  #  ALL    P 1970-03-01 1970-04-01  6200150    ALL        DAYS     82
  #  ALL    P 1970-04-01 1970-05-01  6200150    ALL        DAYS     74
  #  ALL    P 1970-05-01 1970-06-01  6200150    ALL        DAYS     82
  #  ALL    P 1970-06-01 1970-07-01  6200150    ALL        DAYS     81
  #  ALL    P 1970-07-01 1970-08-01  6200150    ALL        DAYS     75
  #  ALL    P 1970-12-01 1971-01-01  6200150    ALL        DAYS     56

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
  
##Efforts

# # Reach the efforts pivot DSD using a function in WCPFC_functions.R
# efforts_pivot_WCPFC<-FUN_efforts_WCPFC_CE (path_to_raw_dataset)
# efforts_pivot_WCPFC$Gear<-"P"

## Reach the catches pivot DSD using a function stored in WCPFC_functions.R
#catches_pivot_WCPFC<-FUN_catches_WCPFC_CE_allButPurseSeine (path_to_raw_dataset)
#202-11-13 @eblondel changes for Tuna atlas updates
#Changes
#	- change from dbf to csv
#	- switch to upper colnames
#	- toupper applied to Species/CatchUnits
#--------------------------------------------------

### Reach the catches pivot DSD using a function stored in WCPFC_functions.R
#catches_pivot_WCPFC<-FUN_catches_WCPFC_CE_allButPurseSeine (path_to_raw_dataset)
#-------------
#2020-22-13 @eblondel - pickup code from rtunaatlas::FUN_catches_WCPFC_CE_allButPurseSeine
#Changes:
#	- switch to csv
#	- switch to upper colnames
#	- CWP grid (removed for the timebeing to apply rtunaatlas codes)
#	- toupper applied to Species/CatchUnits
DF <- read.csv(path_to_raw_dataset)
colnames(DF) <- toupper(colnames(DF))
DF$CWP_GRID <- NULL #@eblondel CWP grid (removed for the timebeing to apply rtunaatlas codes)
# DF <- melt(DF, id = c(colnames(DF[1:5]))) #@juldebar error with melt function from reshape package
# DF<-melt(as.data.table(DF), id=c(colnames(DF[1:5])))
DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:5])))

DF <- DF %>% dplyr::filter(!value %in% 0) %>% dplyr::filter(!is.na(value))
DF$variable <- as.character(DF$variable)
colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
DF$CatchUnits <- substr(DF$Species, nchar(DF$Species), nchar(DF$Species))
DF$CatchUnits <- toupper(DF$CatchUnits) #@eblondel to upper
DF$Species <- toupper(DF$Species) #@eblondel to upper
DF$Species <- sub("_C", "", DF$Species)
DF$Species <- sub("_N", "", DF$Species)
DF$School <- "OTH"
DF$EffortUnits <- colnames(DF[5])
colnames(DF)[5] <- "Effort"
efforts_pivot_WCPFC <- DF; rm(DF)
#-------------

#Gear
efforts_pivot_WCPFC$Gear<-"P"

# Catchunits
index.kg <- which( efforts_pivot_WCPFC[,"CatchUnits"] == "C" )
efforts_pivot_WCPFC[index.kg,"CatchUnits"]<- "t"

index.nr <- which( efforts_pivot_WCPFC[,"CatchUnits"] == "N" )
efforts_pivot_WCPFC[index.nr,"CatchUnits"]<- "no" 

# School
efforts_pivot_WCPFC$School<-"ALL"

# Reach the efforts harmonized DSD using a function in ICCAT_functions.R
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
efforts<-WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_WCPFC,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"WCPFC"

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
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".DBF"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(efforts, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".DBF"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
