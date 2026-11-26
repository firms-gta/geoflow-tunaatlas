#' Harmonize IATTC PS BySchool Catch Datasets
#'
#' Harmonizes the structure of IATTC PS (Purse Seine) catch datasets by type of school (set type). This function is designed
#' to adjust catch data to fit standardized formats required for integration into the Tuna Atlas database.
#' The process involves reformatting the data and possibly integrating metadata and code lists if they
#' will be loaded into the Tuna Atlas database.
#' This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|SetType|LatC1|LonC1|NumSets}
#'
#' @details The function reads raw data, processes it according to specified stratifications such as
#'          'PublicPSBillfishSetType', 'PublicPSTunaSetType', and 'PublicPSSharkSetType', and then outputs a
#'          harmonized dataset. It can conditionally include metadata and code lists based on whether the
#'          data is intended for database loading.
#'
#' @seealso \code{convertDSD_iattc_ce()} for other conversions related to IATTC data,
#'          \code{format_time_db_format()} to standardize time formatting for database integration.
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (iattc/catch/ps_byschool/action_iattc_catch_ps_byschool.R).
#' @export
gta_rawdata_iattc_catch_ps_byschool <- function(data) {
  path_to_raw_dataset <- gta_path(data)
catches_pivot_IATTC <-FUN_catches_IATTC_CE_Flag_or_SetType(path_to_raw_dataset,"SetType","PS")
catches_pivot_IATTC$NumSets<-NULL

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IATTC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"IATTC"
catches$measurement_type <- "RC" # Retained catches
catches$measurement <- "catch"
catches$measurement_processing_level <- "unknown"
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
catches$time_start <- as.Date(catches$time_start)
catches$time_end <- as.Date(catches$time_end)
  catches
}
