######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = west_pacific_ocean_catch_5deg_1m_ps_tunaatlaswcpfc_level0, title = Harmonize data structure of WCPFC Purse Seine catch datasets, abstract = Harmonize the structure of WCPFC catch-and-effort datasets: 'Purse Seine' (pid of output file = west_pacific_ocean_catch_5deg_1m_ps_tunaatlaswcpfc_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/d203JL, value = "https://goo.gl/d203JL";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

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
  require(rtunaatlas)
}
if(!require(foreign)){
  install.packages("foreign")
  require(foreign)
}




#wd<-getwd()
#download.file(path_to_raw_dataset,destfile=paste(wd,"/dbf_file.DBF",sep=""), method='auto', quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
#path_to_raw_dataset=paste(wd,"/dbf_file.DBF",sep="")


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
  
  
  # Catch: final data sample:
  # Flag Gear time_start   time_end AreaName School Species CatchType CatchUnits   Catch
  #  ALL    S 1970-01-01 1970-02-01  6100135    LOG     BET       ALL         MT  12.181
  #  ALL    S 1970-01-01 1970-02-01  6100135    LOG     SKJ       ALL         MT  84.587
  #  ALL    S 1970-01-01 1970-02-01  6100135    LOG     YFT       ALL         MT 110.307
  #  ALL    S 1970-02-01 1970-03-01  6100125    LOG     BET       ALL         MT   5.943
  #  ALL    S 1970-02-01 1970-03-01  6100125    LOG     SKJ       ALL         MT  35.133
  #  ALL    S 1970-02-01 1970-03-01  6100125    LOG     YFT       ALL         MT  53.466
  

##Catches

### Reach the catches pivot DSD using a function stored in WCPFC_functions.R
catches_pivot_WCPFC<-FUN_catches_WCPFC_CE_Purse_Seine_2016 (path_to_raw_dataset)
catches_pivot_WCPFC$Gear<-"S"

# Catchunits
# Check data that exist both in number and weight

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
