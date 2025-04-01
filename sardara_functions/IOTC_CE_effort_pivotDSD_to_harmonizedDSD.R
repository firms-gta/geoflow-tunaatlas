#' Convert IOTC effort pivot DSD to harmonized DSD
#'
#' @param efforts_pivot_IOTC A data frame in pivot DSD format
#' @param colToKeep_efforts Vector of column names to keep in the final output
#'
#' @return A harmonized effort data frame
#' @export
IOTC_CE_effort_pivotDSD_to_harmonizedDSD <- function(efforts_pivot_IOTC, colToKeep_efforts) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  
  efforts_pivot_IOTC <- efforts_pivot_IOTC %>%
    dplyr::filter(!Effort %in% 0) %>%
    dplyr::filter(!is.na(Effort)) %>%
    dplyr::mutate(
      RFMO = "IOTC",
      Ocean = "IND",
      FishingFleet = Fleet,
      Gear = Gear,
      AreaCWPgrid = Grid,
      AreaName = iGrid,
      School = SchoolEffort,
      EffortUnits = EffortUnits,
      Effort = Effort
    )
  
  efforts_pivot_IOTC <- efforts_pivot_IOTC %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      time_start = as.Date(sprintf("%04d-%02d-01", Year, MonthStart)),
      time_end = ifelse(
        MonthEnd == MonthStart,
        time_start + lubridate::months(1),
        as.Date(sprintf("%04d-%02d-01", Year, MonthEnd)) + lubridate::months(1)
      )
    ) %>%
    dplyr::ungroup()
  
  if (!exists("harmo_spatial_2")) {
    source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_2.R")
  }
  efforts_pivot_IOTC <- harmo_spatial_2(efforts_pivot_IOTC, "AreaName")
  
  # Sélection et nettoyage final
  efforts <- efforts_pivot_IOTC[colToKeep_efforts]
  rm(efforts_pivot_IOTC)
  
  efforts[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(efforts[, 
                                                                          c("AreaName", "FishingFleet")], 2, function(x) {
                                                                            gsub(" *$", "", x)
                                                                          }), stringsAsFactors = FALSE)
  
  efforts <- efforts %>%
    dplyr::filter(!Effort %in% 0) %>%
    dplyr::filter(!is.na(Effort)) %>%
    dplyr::group_by(FishingFleet, Gear, time_start, time_end, AreaName, School, EffortUnits) %>%
    dplyr::summarise(Effort = sum(Effort), .groups = "drop")
  
  return(as.data.frame(efforts))
}
