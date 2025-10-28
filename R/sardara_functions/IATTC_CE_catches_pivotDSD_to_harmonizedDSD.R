IATTC_CE_catches_pivotDSD_to_harmonizedDSD= function (IATTC_CE_catches_df_pivotDSD, colToKeep_captures) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_spatial_4.R")
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  
  IATTC_CE_catches_df_pivotDSD$RFMO <- "IATTC"
  IATTC_CE_catches_df_pivotDSD$Ocean <- "PAC_E"
  index.fishingfleet.other <- which(IATTC_CE_catches_df_pivotDSD[, 
                                                                 "FishingFleet"] == "Other")
  IATTC_CE_catches_df_pivotDSD[index.fishingfleet.other, "FishingFleet"] <- "OTR"
  IATTC_CE_catches_df_pivotDSD <- harmo_time_2(IATTC_CE_catches_df_pivotDSD, 
                                               "Year", "Month")
  IATTC_CE_catches_df_pivotDSD <- harmo_spatial_4(IATTC_CE_catches_df_pivotDSD, 
                                                  "Lat", "Lon", "SquareSize", "CodeSquareSize")
  colnames(IATTC_CE_catches_df_pivotDSD)[which(colnames(IATTC_CE_catches_df_pivotDSD) == 
                                                 "SetType")] <- "School"
  colnames(IATTC_CE_catches_df_pivotDSD)[which(colnames(IATTC_CE_catches_df_pivotDSD) == 
                                                 "variable")] <- "Species"
  IATTC_CE_catches_df_pivotDSD$CatchType <- "ALL"
  IATTC_CE_catches_df_pivotDSD$Catch <- IATTC_CE_catches_df_pivotDSD$value
  catches <- IATTC_CE_catches_df_pivotDSD[colToKeep_captures]
  rm(IATTC_CE_catches_df_pivotDSD)
  catches[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(catches[, 
                                                                          c("AreaName", "FishingFleet")], 2, function(x) {
                                                                            gsub(" *$", "", x)
                                                                          }), stringsAsFactors = FALSE)
  catches <- catches %>% dplyr::filter(!Catch %in% 0) %>% 
    dplyr::filter(!is.na(Catch))
  catches <- catches %>% group_by(FishingFleet, Gear, time_start, 
                                  time_end, AreaName, School, Species, CatchType, CatchUnits) %>% 
    dplyr::summarise(Catch = sum(Catch))
  catches <- as.data.frame(catches)
  return(catches)
}
