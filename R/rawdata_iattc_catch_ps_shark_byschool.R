#' Harmonize IATTC PS Shark BySchool Catch Datasets
#'
#' This function harmonizes the structure of IATTC PS (Purse Seine) shark catch datasets by type of school (set type),
#' preparing them for integration into the Tuna Atlas database. It ensures that only the essential
#' fields are retained and that metadata is included if the dataset is destined for the database.
#'
#' @details The function processes the data based on set type categories specifically for shark species.
#'          It involves cleaning, restructuring, and harmonizing data fields to meet specified
#'          standards. Metadata integration is optional and contingent on the destination of the dataset.
#'
#' @seealso \code{FUN_catches_IATTC_CE_Flag_or_SetType_Shark()} for the specific processing of shark catches by type of school (set type),
#'          \code{IATTC_CE_catches_pivotDSD_to_harmonizedDSD()} for general data structuring.
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (iattc/catch/ps_shark_byschool/action_iattc_catch_ps_shark_byschool.R).
#' @export
gta_rawdata_iattc_catch_ps_shark_byschool <- function(data) {
  # Samples of the input and output data (from the former script)
  # '# This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|SetType|LatC1|LonC1|NumSets}
  path_to_raw_dataset <- gta_path(data)
catches_pivot_IATTC <-FUN_catches_IATTC_CE_Flag_or_SetType_Shark(path_to_raw_dataset,"SetType","PS")
catches_pivot_IATTC$NumSets<-NULL

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IATTC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"IATTC"
catches$measurement_type <- "RC" # Retained catches
catches$measurement <- "catch"
catches$measurement_processing_level <- "original_sample" # as collected by observers
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
catches$time_start <- as.Date(catches$time_start)
catches$time_end <- as.Date(catches$time_end)
  catches
}
