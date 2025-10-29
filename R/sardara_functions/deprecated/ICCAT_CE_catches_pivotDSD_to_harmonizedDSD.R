ICCAT_CE_catches_pivotDSD_to_harmonizedDSD = function (catches_pivot_ICCAT, colToKeep_captures) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_time_1.R")  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_spatial_1.R")  
  catches_pivot_ICCAT$RFMO <- "ICCAT"
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  
  catches_pivot_ICCAT$Ocean <- "ATL"
  catches_pivot_ICCAT$Gear <- catches_pivot_ICCAT$GearCode
  catches_pivot_ICCAT <- harmo_time_1(catches_pivot_ICCAT, 
                                      "YearC", "TimePeriodID")
  catches_pivot_ICCAT <- harmo_spatial_1(catches_pivot_ICCAT, 
                                         "Lon", "Lat", "QuadID", "SquareTypeCode", NULL)
  catches_pivot_ICCAT$Species <- catches_pivot_ICCAT$variable
  catches_pivot_ICCAT$CatchType <- "C"
  catches_pivot_ICCAT$Catch <- catches_pivot_ICCAT$value
  index.kg <- which(catches_pivot_ICCAT[, "CatchUnits"] == 
                      "MT" | catches_pivot_ICCAT[, "CatchUnits"] == "MTNO")
  catches_pivot_ICCAT[index.kg, "Catch"] <- catches_pivot_ICCAT[index.kg, 
                                                                "Catch"]/1000
  catches <- catches_pivot_ICCAT[colToKeep_captures]
  rm(catches_pivot_ICCAT)
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
