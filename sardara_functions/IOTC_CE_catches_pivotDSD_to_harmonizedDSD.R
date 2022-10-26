IOTC_CE_catches_pivotDSD_to_harmonizedDSD = function (catches_pivot_IOTC, colToKeep_captures) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_time_3.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_2.R")
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  
  catches_pivot_IOTC$RFMO <- "IOTC"
  catches_pivot_IOTC$Ocean <- "IND"
  catches_pivot_IOTC$FishingFleet <- catches_pivot_IOTC$Fleet
  catches_pivot_IOTC$Gear <- catches_pivot_IOTC$Gear
  catches_pivot_IOTC <- harmo_time_3(catches_pivot_IOTC, "Year", 
                                     "MonthStart", "MonthEnd")
  catches_pivot_IOTC$AreaCWPgrid <- catches_pivot_IOTC$Grid
  catches_pivot_IOTC$AreaName <- catches_pivot_IOTC$iGrid
  catches_pivot_IOTC <- harmo_spatial_2(catches_pivot_IOTC, 
                                        "AreaName")
  catches_pivot_IOTC$School <- catches_pivot_IOTC$School
  catches_pivot_IOTC$Species <- catches_pivot_IOTC$Species
  catches_pivot_IOTC$CatchUnits <- catches_pivot_IOTC$CatchUnits
  catches_pivot_IOTC$CatchType <- "ALL"
  catches_pivot_IOTC$Catch <- catches_pivot_IOTC$value
  catches <- catches_pivot_IOTC[colToKeep_captures]
  rm(catches_pivot_IOTC)
  catches[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(catches[, 
                                                                          c("AreaName", "FishingFleet")], 2, function(x) {
                                                                            gsub(" *$", "", x)
                                                                          }), stringsAsFactors = FALSE)
  catches <- catches %>% dplyr::filter(!Catch %in% 0) %>% dplyr::filter(!is.na(Catch))
  catches <- catches %>% dplyr::group_by(FishingFleet, Gear, time_start, 
                                  time_end, AreaName, School, Species, CatchType, CatchUnits) %>% 
    dplyr::summarise(Catch = sum(Catch))
  catches <- as.data.frame(catches)
  return(catches)
}
