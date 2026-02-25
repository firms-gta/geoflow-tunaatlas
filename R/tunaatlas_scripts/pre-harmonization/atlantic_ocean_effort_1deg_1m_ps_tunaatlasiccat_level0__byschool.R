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
# A tibble: 6 × 33
# DSetID StrataID FlagName  FleetCode      GearCode YearC Decade TimePeriodID GeoStrataCode QuadID   Lat   Lon  xLon  yLat FishMode  Eff1 Eff1Type   Eff2 Eff2Type   Eff3 Eff3Type   Eff4 Eff4Type   Eff5 Eff5Type    YFT   ALB   BET   BLF   LTA   SKJ   FRI TOTAL
# <dbl>    <dbl> <chr>     <chr>          <chr>    <dbl>  <dbl>        <dbl> <chr>          <dbl> <dbl> <dbl> <dbl> <dbl> <chr>    <dbl> <chr>     <dbl> <chr>     <dbl> <chr>     <dbl> <chr>     <dbl> <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1   2797   509067 EU-España EU.ESP-ES-ETRO PS        1994   1990            1 1x1                1     0     1   1.5   0.5 FAD       13   FISH.HOUR  25.9 HOURS.SEA  13   Hours.STD     0 Hours.FAD  0    Hours.FSC     0     0     0     0     0     0     0     0
# 2   2797   509068 EU-España EU.ESP-ES-ETRO PS        1994   1990            1 1x1                1     1     0   0.5   1.5 FAD       13   FISH.HOUR  25.9 HOURS.SEA  13   Hours.STD     0 Hours.FAD  0    Hours.FSC     0     0     0     0     0     0     0     0
# 3   2797   509069 EU-España EU.ESP-ES-ETRO PS        1994   1990            1 1x1                1     1     2   2.5   1.5 FAD       13   FISH.HOUR  25.9 HOURS.SEA  13   Hours.STD     0 Hours.FAD  0    Hours.FSC     0     0     0     0     0     0     0     0
# 4   2797   509070 EU-España EU.ESP-ES-ETRO PS        1994   1990            1 1x1                2     0     0   0.5  -0.5 FAD       13.1 FISH.HOUR  25.9 HOURS.SEA  13.1 Hours.STD     0 Hours.FAD  0    Hours.FSC     0     0     0     0     0     0     0     0
# 5   2797   509071 EU-España EU.ESP-ES-ETRO PS        1994   1990            1 1x1                2     0     1   1.5  -0.5 FAD       26.1 FISH.HOUR  51.8 HOURS.SEA  26.1 Hours.STD     0 Hours.FAD  2.21 Hours.FSC     0     0     0     0     0     0     0     0
# 6   2797   509072 EU-España EU.ESP-ES-ETRO PS        1994   1990            1 1x1                2     0     2   2.5  -0.5 FAD       12.1 FISH.HOUR  24   HOURS.SEA  12.1 Hours.STD     0 Hours.FAD  0    Hours.FSC     0     0     0     0     0     0     0     0

# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
# Belize   PS 2009-08-01 2009-09-01  5402000     FS   FISH.HOUR  36.50
# Belize   PS 2009-08-01 2009-09-01  5402000     FS   Hours.FSC   3.12
# Belize   PS 2009-08-01 2009-09-01  5402000     FS   HOURS.SEA  72.00
# Belize   PS 2009-08-01 2009-09-01  5402000     FS   Hours.STD  37.10
# Belize   PS 2009-09-01 2009-10-01  5202006     LS   FISH.HOUR  12.10


## Catches
# RFMO_CE<-read.csv(path_to_raw_dataset,stringsAsFactors = F)
RFMO_CE <- read_excel(path_to_raw_dataset,
                sheet = "Data")

RFMO_CE$FleetCode_short <- sub("-.*", "", RFMO_CE$FleetCode) # fleet code only what is after the '-'

names(RFMO_CE)[names(RFMO_CE) == 'FleetCode_short'] <- 'FishingFleet'
RFMO_CE <- RFMO_CE[, c("FishingFleet", setdiff(names(RFMO_CE), "FishingFleet"))] # oput flag in first position


# ## If we want in the output dataset the column 'FleetCode' instead of 'flag'
# 
# if(keep_fleet_instead_of_flag==TRUE){
#   RFMO_CE$Flag<-NULL
#   names(RFMO_CE)[names(RFMO_CE) == 'Fishingfleet'] <- 'Flag'
# }

##Efforts

last_column_not_catch_value=27
RFMO_CE<-RFMO_CE[,-(last_column_not_catch_value:ncol(RFMO_CE))] 

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/FUN_efforts_ICCAT_CE_keep_all_efforts.R")
efforts_pivot_ICCAT<-FUN_efforts_ICCAT_CE_keep_all_efforts(RFMO_CE,c("Eff1","Eff2","Eff3","Eff4","Eff5"),c("Eff1Type","Eff2Type","Eff3Type","Eff4Type","Eff5Type"))

# School
# The format changed, the school is now in the FishMode column

efforts_pivot_ICCAT <- efforts_pivot_ICCAT %>% dplyr::mutate(FishMode = ifelse(FishMode == "n/a", "UNK", FishMode)) 
efforts_pivot_ICCAT<- efforts_pivot_ICCAT %>% dplyr::rename("School" = "FishMode")
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/ICCAT_CE_effort_pivotDSD_to_harmonizedDSD.R")
efforts_pivot_ICCAT <- efforts_pivot_ICCAT %>% dplyr::rename(SquareTypeCode = GeoStrataCode) # to match definition in ICCAT_CE_effort_pivotDSD_to_harmonizedDSD
efforts_pivot_ICCAT$Lat <- floor(abs(efforts_pivot_ICCAT$Lat)) # we put floor as independently of the quadrant the floor always correspond to the cwp
efforts_pivot_ICCAT$Lon <- floor(abs(efforts_pivot_ICCAT$Lon))
efforts<-ICCAT_CE_effort_pivotDSD_to_harmonizedDSD(efforts_pivot_ICCAT,colToKeep_efforts)
efforts$AreaName <- as.character(as.integer(efforts$AreaName))
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
efforts$geographic_identifier <- format(as.integer(efforts$geographic_identifier), scientific = FALSE, trim = TRUE)

#@geoflow -> export as csv
base1 <- tools::file_path_sans_ext(basename(filename1))
#@geoflow -> export as csv
# output in same folder as path_to_raw_dataset 
output_name_dataset   <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_harmonized.csv"))
output_name_codelists <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_codelists.csv"))

write.csv(efforts, output_name_dataset, row.names = FALSE)

file.rename(  from = entity$getJobDataResource(config, filename2),  to   = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)}
