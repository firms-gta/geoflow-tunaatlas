######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = indian_ocean_catch_tunaatlasiotc_level0__surface, title = Harmonize data structure of IOTC Surface catch datasets, abstract = Harmonize the structure of IOTC catch-and-effort datasets: 'Surface' (pid of output file = indian_ocean_catch_tunaatlasiotc_level0__surface). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/bSsmaK, value = "https://goo.gl/bSsmaK";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' This script works with any dataset that has the first 12 columns named and ordered as follow: {Fleet|Gear|Year|MonthStart|MonthEnd|iGrid|Grid|Effort|EffortUnits|QualityCode|Source|CatchUnits} followed by a list of columns specifing the species codes with ".FS" for catches on free schools and ".LS" for catches for catches on log schools and ".UNCL" for catches on unclassied schools
#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Indian Ocean Tuna Commission IOTC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iotc_ce_LonglineCoastal}} to convert IOTC task 2 CECoastal and CELongline data structure, \code{\link{convertDSD_iotc_nc}} to convert IOTC nominal catch data structure

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
if(!require(data.table)){
  install.packages("data.table")
}

require(rtunaatlas)
require(data.table)

  
  # Input data sample:
  # Fleet Gear Year MonthStart MonthEnd      iGrid    Grid Effort EffortUnits QualityCode Source CatchUnits YFT.FS YFT.LS YFT.UNCL BET.FS BET.LS BET.UNCL SKJ.FS SKJ.LS SKJ.UNCL ALB.FS ALB.LS ALB.UNCL SBF.FS SBF.LS
  # EUESP     PS 1996          9        9 5100043    5100043   36.2      FHOURS           3     LO         MT     NA  13.36       NA     NA   5.58       NA     NA  20.26       NA     NA     NA       NA     NA     NA
  # EUESP     PS 2004          8        8 5100043    5100043   12.1      FHOURS           3     LO         MT     NA   8.74       NA     NA   2.66       NA     NA  32.97       NA     NA     NA       NA     NA     NA
  # NEIPS     PS 1992         10       10 5100043    5100043   12.1      FHOURS           3   RFRI         MT     NA   4.53       NA     NA   3.00       NA     NA  37.47       NA     NA     NA       NA     NA     NA
  # EUESP     PS 1991          8        8 5100044    5100044   12.1      FHOURS           3     LO         MT  76.26     NA       NA   9.95     NA       NA  68.14     NA       NA     NA     NA       NA     NA     NA
  # EUESP     PS 1995          6        6 5100044    5100044   12.1      FHOURS           3     LO         MT     NA   0.79       NA     NA   0.68       NA     NA   4.93       NA     NA     NA       NA     NA     NA
  # EUESP     PS 1995          9        9 5100044    5100044   12.1      FHOURS           3     LO                NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA
  # SBF.UNCL LOT.FS LOT.LS LOT.UNCL FRZ.FS FRZ.LS FRZ.UNCL KAW.FS KAW.LS KAW.UNCL COM.FS COM.LS COM.UNCL TUX.FS TUX.LS TUX.UNCL FAL.FS FAL.LS FAL.UNCL OCS.FS OCS.LS OCS.UNCL SKH.FS SKH.LS SKH.UNCL NTAD.FS NTAD.LS
  #       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA      NA      NA
  #       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA      NA      NA
  #       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA      NA      NA
  #       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA      NA      NA
  #       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA      NA      NA
  #       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA     NA     NA       NA      NA      NA
  # NTAD.UNCL
  #        NA
  #        NA
  #        NA
  #        NA
  #        NA
  #        NA
  
  # Catch: final data sample:
  # Flag Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #  AUS   BB 1992-01-01 1992-02-01  6230130    ALL     SBF       ALL         MT 573.5
  #  AUS   BB 1992-02-01 1992-03-01  6230130    ALL     ALB       ALL         MT   3.3
  #  AUS   BB 1992-02-01 1992-03-01  6230130    ALL     SBF       ALL         MT 272.1
  #  AUS   BB 1992-02-01 1992-03-01  6230135    ALL     SBF       ALL         MT  24.7
  #  AUS   BB 1992-02-01 1992-03-01  6235115    ALL     SBF       ALL         MT   2.5
  #  AUS   BB 1992-03-01 1992-04-01  6230130    ALL     ALB       ALL         MT   3.0

  
  ##Catches
  
  ### Reach the catches pivot DSD using a function stored in IOTC_functions.R
  catches_pivot_IOTC<-FUN_catches_IOTC_CE(path_to_raw_dataset,last_column_not_catch_value=12,"Surface")
  
  ### Reach the catches harmonized DSD using a function in IOTC_functions.R
  colToKeep_captures <- c("Flag","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
  catches<-IOTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IOTC,colToKeep_captures)
  
colnames(catches)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"IOTC"
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