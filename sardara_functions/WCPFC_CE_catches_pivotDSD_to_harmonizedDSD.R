WCPFC_CE_catches_pivotDSD_to_harmonizedDSD = function (catches_pivot_WCPFC, colToKeep_captures) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_3.R")
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  
  catches_pivot_WCPFC$RFMO <- "WCPFC"
  catches_pivot_WCPFC$Ocean <- "PAC_W"
  catches_pivot_WCPFC$FishingFleet <- "ALL"
  catches_pivot_WCPFC <- harmo_time_2(catches_pivot_WCPFC, 
                                      "YY", "MM")
  catches_pivot_WCPFC <- harmo_spatial_3(catches_pivot_WCPFC, 
                                         "LAT5", "LON5", 5, 6)
  catches_pivot_WCPFC$CatchType <- "ALL"
  catches_pivot_WCPFC$Catch <- catches_pivot_WCPFC$value
  catches <- catches_pivot_WCPFC[colToKeep_captures]
  rm(catches_pivot_WCPFC)
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
