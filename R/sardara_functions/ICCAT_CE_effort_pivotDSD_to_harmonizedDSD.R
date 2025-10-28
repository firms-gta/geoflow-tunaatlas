ICCAT_CE_effort_pivotDSD_to_harmonizedDSD = function (efforts_pivot_ICCAT, colToKeep_efforts) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_spatial_1.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_time_1.R")  
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  
  efforts_pivot_ICCAT <- efforts_pivot_ICCAT %>% dplyr::filter(!Effort %in% 
                                                                 0) %>% dplyr::filter(!is.na(Effort)) %>% dplyr::filter(!EffortUnits %in% 
                                                                                                                          "NULL")
  efforts_pivot_ICCAT$RFMO <- "ICCAT"
  efforts_pivot_ICCAT$Ocean <- "ATL"
  efforts_pivot_ICCAT$Gear <- efforts_pivot_ICCAT$GearCode
  efforts_pivot_ICCAT <- harmo_time_1(efforts_pivot_ICCAT, 
                                      "YearC", "TimePeriodID")
  efforts_pivot_ICCAT <- harmo_spatial_1(efforts_pivot_ICCAT, 
                                         "Lon", "Lat", "QuadID", "SquareTypeCode", NULL)
  efforts <- efforts_pivot_ICCAT[colToKeep_efforts]
  rm(efforts_pivot_ICCAT)
  efforts[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(efforts[, 
                                                                          c("AreaName", "FishingFleet")], 2, function(x) {
                                                                            gsub(" *$", "", x)
                                                                          }), stringsAsFactors = FALSE)
  efforts <- efforts %>% dplyr::filter(!Effort %in% 0) %>% 
    dplyr::filter(!is.na(Effort))
  efforts <- efforts %>% dplyr::group_by(FishingFleet, Gear, time_start, 
                                  time_end, AreaName, School, EffortUnits) %>% dplyr::summarise(Effort = sum(Effort))
  efforts <- as.data.frame(efforts)
  return(efforts)
}
