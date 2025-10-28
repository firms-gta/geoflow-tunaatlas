#' Harmonize IOTC Surface Effort Datasets
#'
#' This function processes and harmonizes Indian Ocean Tuna Commission (IOTC) surface effort datasets.
#' It prepares the data for integration into the Tuna Atlas database by ensuring compliance with
#' data standardization requirements and optionally includes metadata if the dataset is intended for database loading.
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details This function modifies the dataset to include only essential fields, performs any necessary calculations
#'          for effort units, and standardizes the format for date fields and geographical identifiers.
#'          Metadata integration is contingent on the final use of the dataset within the Tuna Atlas database.
#'
#' @importFrom dplyr filter mutate
#' @importFrom readr read_csv write_csv
#' @seealso \code{\link{FUN_efforts_IOTC_CE}} for initial effort data processing,
#'          \code{\link{IOTC_CE_effort_pivotDSD_to_harmonizedDSD}} for converting effort data to a standardized structure.
#' @export
#' @keywords IOTC, tuna, fisheries, data harmonization, effort data
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
function(action, entity, config){
  
if(!require(readr)){
  install.packages("readr")
}

require(readr)
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }


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


# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  AUS   BB 1992-01-01 1992-02-01  6230130    IND       HRSRH    403
#  AUS   BB 1992-02-01 1992-03-01  6230130    IND       HRSRH    366
#  AUS   BB 1992-02-01 1992-03-01  6230135    IND       HRSRH     30
#  AUS   BB 1992-02-01 1992-03-01  6235115    IND       HRSRH     23
#  AUS   BB 1992-03-01 1992-04-01  6230130    IND       HRSRH    221
#  AUS   BB 1992-03-01 1992-04-01  6235130    IND       HRSRH     68

  #----------------------------------------------------------------------------------------------------------------------------
  #@geoflow --> with this script 2 objects are pre-loaded
  #config --> the global config of the workflow
  #entity --> the entity you are managing
  #get data from geoflow current job dir
  filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  IOTC-DATASETS-2023-04-24-CE-Surface_1950-2021.csv , IOTC-DATASETS-2025-03-13-CEOthers.csv
  filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iotc_effort_code_lists_2023.csv
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")  
  
##Efforts

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/FUN_efforts_IOTC_CE.R")
efforts_pivot_IOTC<-FUN_efforts_IOTC_CE(path_to_raw_dataset,12)
efforts_pivot_IOTC$CatchUnits<-NULL
efforts_pivot_IOTC$Source<-NULL

colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/IOTC_CE_effort_pivotDSD_to_harmonizedDSD.R")
efforts<-IOTC_CE_effort_pivotDSD_to_harmonizedDSD(efforts_pivot_IOTC,colToKeep_efforts)


colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"IOTC"
efforts$measurement <- "efforts"

# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/split_measurement_multimonth_if_needed.R")
# efforts <- split_measurement_multimonth_if_needed(efforts)

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
