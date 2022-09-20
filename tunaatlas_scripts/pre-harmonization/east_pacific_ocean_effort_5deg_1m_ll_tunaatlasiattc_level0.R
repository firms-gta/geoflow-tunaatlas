######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = east_pacific_ocean_effort_5deg_1m_ll_tunaatlasiattc_level0, title = Harmonize data structure of IATTC LL (longline) effort datasets, abstract = Harmonize the structure of IATTC catch-and-effort datasets: 'Shark' and 'Tuna_Billfish' (pid of output file = east_pacific_ocean_effort_5deg_1m_ll_tunaatlasIATTC_level0__shark or east_pacific_ocean_effort_5deg_1m_ll_tunaatlasIATTC_level0__tuna_billfish). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/U0zyWa, value = "https://goo.gl/U0zyWa";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 


#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Inter-American-Tropical-Tuna-Commission IATTC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iattc_nc}} to convert IATTC nominal catch data structure, code{\link{convertDSD_iattc_ce_LLTunaBillfish_LLShark}} to convert IATTC task 2 LLTunaBillfish and LLShark data structure, \code{\link{convertDSD_iattc_ce_LPTunaFlag}} to convert IATTC task 2 LPTunaFlag data structure, \code{\link{convertDSD_iattc_ce_LLOrigFormat}} to convert IATTC task 2 Longline original format data structure, \code{\link{convertDSD_iattc_ce_PSSharkSetType}} to convert IATTC task 2 'PublicPSSharkSetType' data structure, \code{\link{convertDSD_iattc_ce_PSSharkFlag}} to convert IATTC task 2 'PublicPSSharkFlag' data structure, \code{\link{convertDSD_iattc_ce_PSSharkFlag}} to convert IATTC task 2 'PublicPSBillfishSetType' and 'PublicPSSharkSetType' and 'PublicPSTunaSetType' data structure, \code{\link{convertDSD_iattc_ce_PSFlag}} to convert IATTC task 2 'PublicPSBillfishFlag' and 'PublicPSSharkFlag' and 'PublicPSTunaFlag' data structure
#'
function(action, entity, config){
  

if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
}

if(!require(dplyr)){
  install.packages("dplyr")
}

# Effort input data sample:
# Record Year Month FlagAbv  Lat    Lon  Hooks
#  28642 1954    10     JPN  7.5 -142.5  13485
#  29009 1954    10     JPN  7.5 -137.5  18914
#  36788 1954    11     JPN 32.5 -142.5   2344
#  24011 1954    11     JPN  2.5 -142.5   3626
#  29040 1954    11     JPN  7.5 -137.5 258455
#  29399 1954    11     JPN  7.5 -132.5 127331
  
# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  BLZ   LL 2009-01-01 2009-02-01  6400100    ALL       Hooks 122400
#  BLZ   LL 2009-01-01 2009-02-01  6400110    ALL       Hooks  22900
#  BLZ   LL 2009-01-01 2009-02-01  6405100    ALL       Hooks  45000
#  BLZ   LL 2009-01-01 2009-02-01  6405105    ALL       Hooks  24300
#  BLZ   LL 2009-01-01 2009-02-01  6405110    ALL       Hooks  37050
#  BLZ   LL 2009-01-01 2009-02-01  6405115    ALL       Hooks  39000
  
  require(dplyr)
  require(rtunaatlas)

  filename_catch <- entity$data$source[[1]] #catch data
  filename_effort <- entity$data$source[[2]] #effort data
  filename_str <- entity$data$source[[3]] #structure
  path_to_raw_dataset_catch <- entity$getJobDataResource(config, filename_catch)
  path_to_raw_dataset_effort <- entity$getJobDataResource(config, filename_effort)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  
efforts<-read.csv(path_to_raw_dataset,stringsAsFactors = F)

##Efforts

# Reach the efforts pivot DSD using a function in IATTC_functions.R
efforts$Record<-NULL

colnames(efforts)<-c("Year","Month","Flag","Lat","Lon","Effort")
efforts$SquareSize<-5
efforts$CodeSquareSize<-6
efforts$Gear<-"LL"
efforts$EffortUnits<-"Hooks"
efforts$SetType<-"ALL"


# Reach the efforts harmonized DSD using a function in IATTC_functions.R
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
efforts<-IATTC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts,colToKeep_efforts)

colnames(efforts)<-c("fishingfleet","gear","time_start","time_end","geographic_identifier","schooltype","unit","value")
efforts$source_authority<-"IATTC"

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
output_name_dataset <- gsub(filename_catch, paste0(unlist(strsplit(filename_catch,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset_catch)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename_catch, paste0(unlist(strsplit(filename_catch,".csv"))[1], "_codelists.csv"), path_to_raw_dataset_catch)
file.rename(from = entity$getJobDataResource(config, filename_str), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset_catch)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)

}

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

