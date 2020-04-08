######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = indian_ocean_catch_tunaatlasiotc_level0__coastal_longline, title = Harmonize data structure of IOTC Coastal and Longline catch datasets, abstract = Harmonize the structure of IOTC catch-and-effort datasets: 'Coastal' and 'Longline' (pid of output file = indian_ocean_catch_tunaatlasIOTC_level0__coastal or indian_ocean_catch_ll_tunaatlasIOTC_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/nElWy8, value = "https://goo.gl/nElWy8";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' This script works with any dataset that has the first 11 columns named and ordered as follow: {Fleet|Gear|Year|MonthStart|MonthEnd|iGrid|Grid|Effort|EffortUnits|QualityCode|Source} followed by a list of columns specifing the species codes with ".NO" for catches expressed in number and ".MT" for catches expressed in tons
#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Indian Ocean Tuna Commission IOTC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iotc_ce_Surface}} to convert IOTC task 2 CESurface data structure, \code{\link{convertDSD_iotc_nc}} to convert IOTC nominal catch data structure

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
require(rtunaatlas)

  # Input data sample:
  # Fleet Gear Year MonthStart MonthEnd      iGrid    Grid Effort EffortUnits QualityCode Source YFT.NO YFT.MT BET.NO BET.MT SKJ.NO SKJ.MT ALB.NO ALB.MT SBF.NO SBF.MT LOT.NO LOT.MT FRZ.NO FRZ.MT KAW.NO KAW.MT
  # MDV     LLCO 2014         12       12 5100067    5100067   6000       HOOKS           0     LO     NA   0.25     NA   0.55     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
  # MDV     HAND 2013          2        2 5100069    5100069      2       FDAYS           0     LO     NA  74.60     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
  # MDV     HAND 2013          3        3 5100069    5100069      1       FDAYS           0     LO     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
  # MDV     HAND 2013          4        4 5100069    5100069      5       FDAYS           0     LO     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
  # MDV     HAND 2013          5        5 5100069    5100069      1       FDAYS           0     LO     NA   0.18     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
  # MDV     HAND 2013          7        7 5100069    5100069      4       FDAYS           0     LO     NA   3.14     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
  # COM.NO COM.MT SWO.NO SWO.MT BILL.NO BILL.MT TUX.NO TUX.MT SKH.NO SKH.MT NTAD.NO NTAD.MT
  #     NA     NA     NA   1.07      NA    0.02     NA     NA     NA     NA      NA    0.01
  #     NA     NA     NA     NA      NA      NA     NA     NA     NA     NA      NA      NA
  #     NA     NA     NA     NA      NA      NA     NA     NA     NA     NA      NA      NA
  #     NA     NA     NA     NA      NA      NA     NA     NA     NA     NA      NA      NA
  #     NA     NA     NA     NA      NA      NA     NA     NA     NA     NA      NA      NA
  #     NA     NA     NA     NA      NA      NA     NA     NA     NA     NA      NA      NA
  
  
  
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
catches_pivot_IOTC<-FUN_catches_IOTC_CE(path_to_raw_dataset,last_column_not_catch_value=11,"CoastalLongline")

### Reach the catches harmonized DSD using a function in IOTC_functions.R
colToKeep_captures <- c("Flag","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches<-IOTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IOTC,colToKeep_captures)

colnames(catches)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"IOTC"
  
  
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
catches$time_start <- as.Date(catches$time_start)
catches$time_end <- as.Date(catches$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(as.character(min(catches$time_start)), as.character(max(catches$time_end)), sep = "/")
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