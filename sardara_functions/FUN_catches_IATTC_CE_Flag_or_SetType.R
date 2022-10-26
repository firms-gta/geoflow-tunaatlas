FUN_catches_IATTC_CE_Flag_or_SetType = function (Path_to_IATTC_CE, aggregation_dimension, GearCode) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/create_additional_columns_IATTC_CE.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_catches_IATTC_CE_Flag_or_SetType.R")
  IATTC_CE <- read.table(Path_to_IATTC_CE, sep = ",", header = TRUE, 
                         stringsAsFactors = FALSE, strip.white = TRUE)
  IATTC_CE <- data.table(IATTC_CE)
  IATTC_CE <- melt(IATTC_CE, id.vars = c("Year", "Month", 
                                         aggregation_dimension, "LatC1", "LonC1", "NumSets"))
  IATTC_CE <- as.data.frame(IATTC_CE)
  IATTC_CE <- IATTC_CE %>% dplyr::filter(!value %in% 0) %>% 
    dplyr::filter(!is.na(value))
  IATTC_CE$variable <- as.character(IATTC_CE$variable)
  IATTC_CE <- create_additional_columns_IATTC_CE(IATTC_CE, 
                                                 "LatC1", "LonC1", 1, GearCode)
  IATTC_CE$CatchUnits <- "MT"
  if (aggregation_dimension == "Flag") {
    IATTC_CE$SetType <- "ALL"
  }
  if (aggregation_dimension == "SetType") {
    IATTC_CE$Flag <- "ALL"
  }
  colnames(IATTC_CE)[colnames(IATTC_CE) == "Flag"] <- "FishingFleet"
  return(IATTC_CE)
}
