######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = east_pacific_ocean_catch_1deg_1m_bb_tunaatlasiattc_level0__byflag, title = Harmonize data structure of IATTC LP (Pole-and-line) catch datasets, abstract = Harmonize the structure of IATTC catch-and-effort datasets: 'LPTunaFlag' (pid of output file = east_pacific_ocean_catch_1deg_1m_bb_tunaatlasIATTC_level0__tuna_byFlag). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/nl6Q0m, value = "https://goo.gl/nl6Q0m";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

# '# This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|Flag|LatC1|LonC1|NumSets} followed by a list of columns specifing the species
#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Inter-American-Tropical-Tuna-Commission IATTC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iattc_nc}} to convert IATTC nominal catch data structure, code{\link{convertDSD_iattc_ce_LLTunaBillfish_LLShark}} to convert IATTC task 2 LLTunaBillfish and LLShark data structure, \code{\link{convertDSD_iattc_ce_LPTunaFlag}} to convert IATTC task 2 LPTunaFlag data structure, \code{\link{convertDSD_iattc_ce_LLOrigFormat}} to convert IATTC task 2 Longline original format data structure, \code{\link{convertDSD_iattc_ce_PSSharkSetType}} to convert IATTC task 2 'PublicPSSharkSetType' data structure, \code{\link{convertDSD_iattc_ce_PSSharkFlag}} to convert IATTC task 2 'PublicPSSharkFlag' data structure, \code{\link{convertDSD_iattc_ce_PSSharkFlag}} to convert IATTC task 2 'PublicPSBillfishSetType' and 'PublicPSSharkSetType' and 'PublicPSTunaSetType' data structure, \code{\link{convertDSD_iattc_ce_PSFlag}} to convert IATTC task 2 'PublicPSBillfishFlag' and 'PublicPSSharkFlag' and 'PublicPSTunaFlag' data structure
#'

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
if(!require(reshape)){
  install.packages("reshape")
}
if(!require(dplyr)){
  install.packages("dplyr")
}

  # Input data sample:
  # Year Month Flag LatC1  LonC1 NumSets ALB BET BKJ BZX PBF   SKJ TUN  YFT
  # 1978     1  USA   3.5  -79.5       2   0   0   0   0   0  6.05   0 4.74
  # 1978     1  USA  20.5 -114.5       2   0   0   0   0   0  3.53   0 2.76
  # 1978     1  USA  23.5 -111.5       2   0   0   0   0   0 20.80   0 4.50
  # 1978     1  USA  23.5 -109.5       1   0   0   0   0   0  0.00   0 0.90
  # 1978     1  USA  24.5 -111.5       1   0   0   0   0   0  1.51   0 1.18
  # 1978     1  USA  25.5 -114.5       2   0   0   0   0   0  5.00   0 3.60
  
  # Catch: final data sample:
  # Flag Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #  USA   LL 1992-07-01 1992-08-01  6425135    ALL     BSH       ALL         NO     4
  #  USA   LL 1993-04-01 1993-05-01  6425135    ALL     BSH       ALL         NO    75
  #  USA   LL 1993-04-01 1993-05-01  6430135    ALL     BSH       ALL         NO    15
  #  USA   LL 1993-05-01 1993-06-01  6425135    ALL     BSH       ALL         NO    24
  #  USA   LL 1994-03-01 1994-04-01  6425135    ALL     BSH       ALL         NO    14
  #  USA   LL 1994-03-01 1994-04-01  6430135    ALL     BSH       ALL         NO     4
  
  ##Catches
  
  # Reach the catches pivot DSD using a function stored in IATTC_functions.R
  catches_pivot_IATTC <-FUN_catches_IATTC_CE_Flag_or_SetType(path_to_raw_dataset,"Flag","LP")
  catches_pivot_IATTC$NumSets<-NULL
  
  colToKeep_captures <- c("Flag","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
  catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IATTC,colToKeep_captures)
  
  colnames(catches)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
  catches$source_authority<-"IATTC"
  
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
output_name_dataset <- gsub(filename, paste0(unlist(strsplit(filename,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(dataset$dataset, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename, paste0(unlist(strsplit(filename,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
write.csv(dataset$codelists, output_name_codelists, row.names = FALSE)
#----------------------------------------------------------------------------------------------------------------------------  
