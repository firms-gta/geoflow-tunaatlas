FUN_efforts_IATTC_CE_allbutLLTunaBillfish = function (Path_to_IATTC_CE, ColEffortUnits, aggregation_dimension, 
          GearCode) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/create_additional_columns_IATTC_CE.R")
  IATTC_CE <- read.table(Path_to_IATTC_CE, sep = ",", header = TRUE, 
                         stringsAsFactors = FALSE, strip.white = TRUE)
  IATTC_CE <- IATTC_CE[, 1:6]
  IATTC_CE <- create_additional_columns_IATTC_CE(IATTC_CE, 
                                                 "LatC1", "LonC1", 1, GearCode)
  IATTC_CE[, "EffortUnits"] <- colnames(IATTC_CE[which(colnames(IATTC_CE) == 
                                                         ColEffortUnits)])
  colnames(IATTC_CE)[which(colnames(IATTC_CE) == ColEffortUnits)] <- "Effort"
  if (aggregation_dimension == "Flag") {
    IATTC_CE$SetType <- "ALL"
  }
  if (aggregation_dimension == "SetType") {
    IATTC_CE$Flag <- "ALL"
  }
  IATTC_CE <- IATTC_CE %>% dplyr::filter(!Effort %in% 0) %>% 
    dplyr::filter(!is.na(Effort))
  IATTC_CE <- unique(IATTC_CE)
  colnames(IATTC_CE)[colnames(IATTC_CE) == "Flag"] <- "FishingFleet"
  return(IATTC_CE)
}
