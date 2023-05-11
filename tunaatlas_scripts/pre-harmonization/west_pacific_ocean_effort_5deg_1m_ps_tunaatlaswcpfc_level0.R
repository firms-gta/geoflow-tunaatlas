######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = west_pacific_ocean_effort_5deg_1m_ps_tunaatlaswcpfc_level0, title = Harmonize data structure of WCPFC Purse Seine effort datasets, abstract = Harmonize the structure of WCPFC catch-and-effort datasets: 'Purse Seine' (pid of output file = west_pacific_ocean_effort_5deg_1m_ps_tunaatlaswcpfc_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/d203JL, value = "https://goo.gl/d203JL";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' 
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Western and Central Pacific Fisheries Commission WCPFC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_wcpfc_ce_Driftnet}} to convert WCPFC task 2 Drifnet data structure, \code{\link{convertDSD_wcpfc_ce_Longline}} to convert WCPFC task 2 Longline data structure, \code{\link{convertDSD_wcpfc_ce_Pole_and_line}} to convert WCPFC task 2 Pole-and-line data structure, \code{\link{convertDSD_wcpfc_ce_PurseSeine}} to convert WCPFC task 2 Purse seine data structure, \code{\link{convertDSD_wcpfc_nc}} to convert WCPFC task 1 data structure  

function(action, entity, config){
  
  #packages
  
    
      
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
# YY MM LAT5 LON5 DAYS SETS_UNA SETS_LOG SETS_DFAD SETS_AFAD SETS_OTH SKJ_C_UNA YFT_C_UNA BET_C_UNA OTH_C_UNA SKJ_C_LOG YFT_C_LOG BET_C_LOG OTH_C_LOG SKJ_C_DFAD
# 1967  2  30N 135E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  2  30N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  2  35N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  2  40N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  2  40N 145E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  3  30N 135E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# YFT_C_DFAD BET_C_DFAD OTH_C_DFAD SKJ_C_AFAD YFT_C_AFAD BET_C_AFAD OTH_C_AFAD SKJ_C_OTH YFT_C_OTH BET_C_OTH OTH_C_OTH
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0


# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  ALL    S 1970-01-01 1970-02-01  6100135    ALL        DAYS     53
#  ALL    S 1970-01-01 1970-02-01  6100135    LOG        SETS     55
#  ALL    S 1970-02-01 1970-03-01  6100125    ALL        DAYS     42
#  ALL    S 1970-02-01 1970-03-01  6100125    LOG        SETS     42
#  ALL    S 1970-02-01 1970-03-01  6100135    ALL        DAYS     57
#  ALL    S 1970-02-01 1970-03-01  6100135    LOG        SETS     50


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


##Efforts

DF <- read.csv(path_to_raw_dataset)
colnames(DF) <- toupper(colnames(DF))
DF$CWP_GRID <- NULL

# DF <- melt(DF, id = c(colnames(DF[1:10])))  #@juldebar error with melt function from reshape package
# DF <- melt(as.data.table(DF), id=c(colnames(DF[1:10])))
DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:10])))

DF <- DF %>% dplyr::filter(!value %in% 0) %>% dplyr::filter(!is.na(value))
DF$variable <- as.character(DF$variable)
colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
DF$School <- substr(DF$Species, 7, nchar(DF$Species))
DF$Species <- sub("_C_UNA", "", DF$Species)
DF$Species <- sub("_C_LOG", "", DF$Species)
DF$Species <- sub("_C_DFAD", "", DF$Species)
DF$Species <- sub("_C_AFAD", "", DF$Species)
DF$Species <- sub("_C_OTH", "", DF$Species)
DF$CatchUnits <- "t"
DF$EffortUnits <- colnames(DF[5])
colnames(DF)[5] <- "Effort"
efforts_pivot_WCPFC <- DF; rm(DF)

#-----------------------------------------------------------
#Gear
efforts_pivot_WCPFC$Gear<-"S"

# Reach the efforts harmonized DSD using a function in WCPFC_functions.R
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD.R")
efforts<-WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_WCPFC,colToKeep_efforts)

colnames(efforts)<-c("fishingfleet","gear","time_start","time_end","geographic_identifier","schooltype","unit","value")
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
