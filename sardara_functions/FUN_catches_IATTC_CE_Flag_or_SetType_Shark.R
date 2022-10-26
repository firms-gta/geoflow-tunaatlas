FUN_catches_IATTC_CE_Flag_or_SetType_Shark = function (Path_to_IATTC_CE, aggregation_dimension, GearCode) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_catches_IATTC_CE_Flag_or_SetType.R")
  IATTC_CE <- FUN_catches_IATTC_CE_Flag_or_SetType(Path_to_IATTC_CE, 
                                                   aggregation_dimension, GearCode)
  IATTC_CE$CatchUnits <- "init"
  index.mt <- grep("mt", IATTC_CE$variable)
  index.n <- grep("n", IATTC_CE$variable)
  IATTC_CE[index.mt, "CatchUnits"] <- "MT"
  IATTC_CE[index.n, "CatchUnits"] <- "NO"
  IATTC_CE$variable <- gsub("n", "", IATTC_CE$variable)
  IATTC_CE$variable <- gsub("mt", "", IATTC_CE$variable)
  IATTC_CE <- unique(IATTC_CE)
  return(IATTC_CE)
}
