WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD = function (efforts_pivot_WCPFC, colToKeep_efforts) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_spatial_3.R")
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  
  efforts_pivot_WCPFC$RFMO <- "WCPFC"
  efforts_pivot_WCPFC$Ocean <- "PAC_W"
  efforts_pivot_WCPFC$FishingFleet <- "ALL"
  efforts_pivot_WCPFC <- harmo_time_2(efforts_pivot_WCPFC, 
                                      "YY", "MM")
  efforts_pivot_WCPFC <- harmo_spatial_3(efforts_pivot_WCPFC, 
                                         "LAT5", "LON5", 5, 6)
  efforts <- efforts_pivot_WCPFC[colToKeep_efforts]
  rm(efforts_pivot_WCPFC)
  efforts[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(efforts[, 
                                                                          c("AreaName", "FishingFleet")], 2, function(x) {
                                                                            gsub(" *$", "", x)
                                                                          }), stringsAsFactors = FALSE)
  efforts <- efforts %>% filter(!Effort %in% 0) %>% filter(!is.na(Effort))
  efforts <- efforts %>% dplyr::group_by(FishingFleet, Gear, time_start, 
                                  time_end, AreaName, School, EffortUnits) %>% dplyr::summarise(Effort = sum(Effort))
  efforts <- as.data.frame(efforts)
  return(efforts)
}
