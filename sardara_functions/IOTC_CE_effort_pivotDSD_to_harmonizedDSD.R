IOTC_CE_effort_pivotDSD_to_harmonizedDSD = function (efforts_pivot_IOTC, colToKeep_efforts) 
{
  efforts_pivot_IOTC <- efforts_pivot_IOTC %>% filter(!Effort %in% 
                                                        0) %>% filter(!is.na(Effort))
  efforts_pivot_IOTC$RFMO <- "IOTC"
  efforts_pivot_IOTC$Ocean <- "IND"
  efforts_pivot_IOTC$FishingFleet <- efforts_pivot_IOTC$Fleet
  efforts_pivot_IOTC$Gear <- efforts_pivot_IOTC$Gear
  efforts_pivot_IOTC <- harmo_time_3(efforts_pivot_IOTC, "Year", 
                                     "MonthStart", "MonthEnd")
  efforts_pivot_IOTC$AreaCWPgrid <- efforts_pivot_IOTC$Grid
  efforts_pivot_IOTC$AreaName <- efforts_pivot_IOTC$iGrid
  efforts_pivot_IOTC <- harmo_spatial_2(efforts_pivot_IOTC, 
                                        "AreaName")
  efforts_pivot_IOTC$School <- efforts_pivot_IOTC$SchoolEffort
  efforts_pivot_IOTC$EffortUnits <- efforts_pivot_IOTC$EffortUnits
  efforts_pivot_IOTC$Effort <- efforts_pivot_IOTC$Effort
  efforts <- efforts_pivot_IOTC[colToKeep_efforts]
  rm(efforts_pivot_IOTC)
  efforts[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(efforts[, 
                                                                          c("AreaName", "FishingFleet")], 2, function(x) {
                                                                            gsub(" *$", "", x)
                                                                          }), stringsAsFactors = FALSE)
  efforts <- efforts %>% filter(!Effort %in% 0) %>% filter(!is.na(Effort))
  efforts <- efforts %>% group_by(FishingFleet, Gear, time_start, 
                                  time_end, AreaName, School, EffortUnits) %>% summarise(Effort = sum(Effort))
  efforts <- as.data.frame(efforts)
  return(efforts)
}
