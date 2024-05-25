#' Harmonize data structure of ICCAT by operation mode effort datasets
#'
#' This function harmonizes the structure of ICCAT catch-and-effort datasets
#' based on operation modes. It adapts the dataset structure for compatibility with
#' the Tuna Atlas database, considering only specific fields as mandatory. The
#' function also handles optional metadata integration if provided.
#'
#' @param action The action context from geoflow, typically involving workflow control.
#' @param entity The entity context from geoflow, managing specific dataset handling.
#' @param config The configuration context from geoflow, used for managing global settings.
#' @param keep_fleet_instead_of_flag Logical, defaults to FALSE. Determines whether to
#'        replace the 'flag' column with the 'fleet' column in the output dataset.
#'
#' @return None; this function performs data manipulation and outputs files directly.
#'
#' @import dplyr
#' @importFrom stringr str_detect str_replace
#' @importFrom readr read_csv write_csv
#' @seealso \code{\link{FUN_efforts_ICCAT_CE_keep_all_efforts}} for converting ICCAT task 2,
#' @export
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' @keywords ICCAT, tuna, fisheries, data harmonization
function(action, entity, config){
  

keep_fleet_instead_of_flag=FALSE

#packages

  
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  t2ce_bySchool.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iccat_effort_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")

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

# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
# Belize   PS 2009-08-01 2009-09-01  5402000     FS   FISH.HOUR  36.50
# Belize   PS 2009-08-01 2009-09-01  5402000     FS   Hours.FSC   3.12
# Belize   PS 2009-08-01 2009-09-01  5402000     FS   HOURS.SEA  72.00
# Belize   PS 2009-08-01 2009-09-01  5402000     FS   Hours.STD  37.10
# Belize   PS 2009-09-01 2009-10-01  5202006     LS   FISH.HOUR  12.10


## Catches

RFMO_CE<-read.csv(path_to_raw_dataset,stringsAsFactors = F)
names(RFMO_CE)[names(RFMO_CE) == 'Flag'] <- 'FishingFleet'

## If we want in the output dataset the column 'FleetCode' instead of 'flag'

if(keep_fleet_instead_of_flag==TRUE){
  RFMO_CE$Flag<-NULL
  names(RFMO_CE)[names(RFMO_CE) == 'Fishingfleet'] <- 'Flag'
}

##Efforts

last_column_not_catch_value=22
RFMO_CE<-RFMO_CE[,-(last_column_not_catch_value:ncol(RFMO_CE))] 

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_efforts_ICCAT_CE_keep_all_efforts.R")
efforts_pivot_ICCAT<-FUN_efforts_ICCAT_CE_keep_all_efforts(RFMO_CE,c("Eff1","Eff2","Eff3","Eff4","Eff5"),c("Eff1Type","Eff2Type","Eff3Type","Eff4Type","Eff5Type"))

# School
# when the unit of effort is Hours.FAD: then schooltype=fd
# when the unit of effort is Hours.FSC: then schooltype=fs
# when the units of efforts are different that Hours.FAD or Hours.FSC: then schooltype=ALL. 

index_school_fs<-which(efforts_pivot_ICCAT$EffortUnits == "Hours.FSC")
index_school_fd<-which(efforts_pivot_ICCAT$EffortUnits == "Hours.FAD")
index_school_all<-which(efforts_pivot_ICCAT$EffortUnits %in% setdiff(unique(efforts_pivot_ICCAT$EffortUnits),c("Hours.FAD","Hours.FSC")))

efforts_pivot_ICCAT$School<-"ALL"
efforts_pivot_ICCAT$School[index_school_fs]<-"fs"
efforts_pivot_ICCAT$School[index_school_fd]<-"fd"


colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/ICCAT_CE_effort_pivotDSD_to_harmonizedDSD.R")
efforts<-ICCAT_CE_effort_pivotDSD_to_harmonizedDSD(efforts_pivot_ICCAT,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"ICCAT"
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
entity$addResource("codelists", output_name_codelists)}
