######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = indian_ocean_effort_tunaatlasiotc_level0__coastal_longline, title = Harmonize data structure of IOTC Coastal and Longline effort datasets, abstract = Harmonize the structure of IOTC catch-and-effort datasets: 'Coastal' and 'Longline' (pid of output file = indian_ocean_effort_tunaatlasIOTC_level0__coastal or indian_ocean_effort_ll_tunaatlasIOTC_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/nElWy8, value = "https://goo.gl/nElWy8";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' This script works with any dataset that has the first 11 columns named and ordered as follow: {Fleet|Gear|Year|MonthStart|MonthEnd|iGrid|Grid|Effort|EffortUnits|QualityCode|Source} followed by a list of columns specifing the species codes with ".NO" for catches expressed in number and ".MT" for catches expressed in tons
#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' 
#' @keywords Indian Ocean Tuna Commission IOTC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iotc_ce_Surface}} to convert IOTC task 2 CESurface data structure, \code{\link{convertDSD_iotc_nc}} to convert IOTC nominal catch data structure

function(action, entity, config){
  
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }

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

# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  AUS HAND 2001-04-01 2001-05-01  5221114    IND        DAYS      4
#  AUS HAND 2001-04-01 2001-05-01  5229114    IND        DAYS      1
#  AUS HAND 2001-05-01 2001-06-01  5221114    IND        DAYS      3
#  AUS HAND 2001-06-01 2001-07-01  5221114    IND        DAYS      9
#  AUS HAND 2001-07-01 2001-08-01  5221114    IND        DAYS      3
#  AUS HAND 2001-07-01 2001-08-01  5225113    IND        DAYS      1

  filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  IOTC-DATASETS-2023-04-24-CE-Coastal_1950-2021.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
# Historical name for the dataset at source  IOTC-DATASETS-2023-04-24-CE-Longline_1950-2021.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
  filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iotc_effort_code_lists_2023.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
# Historical name for the dataset at source  iotc_effort_code_lists_2023.csv, if multiple, this means this function is used for several dataset, keep the same order to match data
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  
##Efforts

# Reach the efforts pivot DSD using a function in ICCAT_functions.R
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_efforts_IOTC_CE.R")
efforts_pivot_IOTC<-FUN_efforts_IOTC_CE(path_to_raw_dataset,11)

# Reach the efforts harmonized DSD using a function in ICCAT_functions.R
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/IOTC_CE_effort_pivotDSD_to_harmonizedDSD.R")
efforts<-IOTC_CE_effort_pivotDSD_to_harmonizedDSD(efforts_pivot_IOTC,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"IOTC"


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
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(efforts, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
