#' Harmonize IATTC PS BySchool Effort Datasets
#'
#' This function harmonizes the effort data for the IATTC PS BySchool datasets,
#' preparing them for integration into the Tuna Atlas database, following the required format specifications.
#' This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|SetType|LatC1|LonC1|NumSets}
#'
#' @details This function modifies the dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @import dplyr
#' @import readr
#' @seealso \code{\link{FUN_efforts_IATTC_CE_allbutLLTunaBillfish}} for initial effort data preprocessing,
#'          \code{\link{IATTC_CE_efforts_pivotDSD_to_harmonizedDSD}} for converting effort data to a standardized structure.
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (iattc/effort/ps_byschool/action_iattc_effort_ps_byschool.R).
gta_rawdata_iattc_effort_ps_byschool <- function(data) {
  path_to_raw_dataset <- gta_path(data)
efforts_pivot_IATTC <-FUN_efforts_IATTC_CE_allbutLLTunaBillfish(path_to_raw_dataset,"NumSets","SetType","PS")

colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
efforts<-IATTC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_IATTC,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"IATTC"
efforts$measurement <- "effort"
efforts$measurement_processing_level <- "unknown" 
efforts <- efforts %>% dplyr::mutate(fishing_mode = ifelse(fishing_mode == "ALL", "OTH", fishing_mode))

#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
  efforts
}
