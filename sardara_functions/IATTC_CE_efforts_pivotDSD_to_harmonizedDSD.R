IATTC_CE_efforts_pivotDSD_to_harmonizedDSD = function (IATTC_CE_efforts_df_pivotDSD, colToKeep_efforts) 
{
  IATTC_CE_efforts_df_pivotDSD$RFMO <- "IATTC"
  IATTC_CE_efforts_df_pivotDSD$Ocean <- "PAC_E"
  index.fishingfleet.other <- which(IATTC_CE_efforts_df_pivotDSD[, 
                                                                 "FishingFleet"] == "Other")
  IATTC_CE_efforts_df_pivotDSD[index.fishingfleet.other, "FishingFleet"] <- "OTR"
  IATTC_CE_efforts_df_pivotDSD <- harmo_time_2(IATTC_CE_efforts_df_pivotDSD, 
                                               "Year", "Month")
  IATTC_CE_efforts_df_pivotDSD <- harmo_spatial_4(IATTC_CE_efforts_df_pivotDSD, 
                                                  "Lat", "Lon", "SquareSize", "CodeSquareSize")
  colnames(IATTC_CE_efforts_df_pivotDSD)[which(colnames(IATTC_CE_efforts_df_pivotDSD) == 
                                                 "SetType")] <- "School"
  efforts <- IATTC_CE_efforts_df_pivotDSD[colToKeep_efforts]
  rm(IATTC_CE_efforts_df_pivotDSD)
  efforts[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(efforts[, 
                                                                          c("AreaName", "FishingFleet")], 2, function(x) {
                                                                            gsub(" *$", "", x)
                                                                          }), stringsAsFactors = FALSE)
  efforts <- efforts %>% dplyr::filter(!Effort %in% 0) %>% 
    dplyr::filter(!is.na(Effort))
  efforts <- efforts %>% group_by(FishingFleet, Gear, time_start, 
                                  time_end, AreaName, School, EffortUnits) %>% summarise(Effort = sum(Effort))
  efforts <- as.data.frame(efforts)
  return(efforts)
}
