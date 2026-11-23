#' Harmonize data structure of ICCAT by operation mode effort datasets (geoflow action)
#'
#' geoflow action of the entity: finds its files, runs the core function gta_rawdata_iccat_effort_ps_byschool()
#' (R/rawdata_iccat_effort_ps_byschool.R), writes the outputs and declares the resources of
#' the entity. The processing itself is in the core function, usable without geoflow.
#'
#' @param action The action context from geoflow.
#' @param entity The entity context from geoflow.
#' @param config The configuration context from geoflow.
function(action, entity, config){



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

efforts <- gta_rawdata_iccat_effort_ps_byschool(path_to_raw_dataset)

dataset_temporal_extent <- paste(
  paste0(format(min(efforts$time_start), "%Y"), "-01-01"),
  paste0(format(max(efforts$time_end), "%Y"), "-12-31"),
  sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

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
entity$addResource("codelists", output_name_codelists)
}
