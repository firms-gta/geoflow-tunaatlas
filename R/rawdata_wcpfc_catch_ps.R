#' Harmonize WCPFC Purse Seine Catch Datasets
#'
#' Converts the WCPFC purse seine catch dataset (5 degrees, monthly; file named
#' `WCPFC_S_PUBLIC_BY_YY_MM.csv` at the source) to the harmonized data structure
#' of the Tuna Atlas.
#'
#' @param data The raw WCPFC dataset: a data frame or the path to the CSV file.
#' @return The harmonized catches: one row per fishing fleet, gear, period,
#'   area, fishing mode and species, with `source_authority`, `measurement`,
#'   `measurement_type`, `measurement_unit`, `measurement_value` and
#'   `measurement_processing_level`.
#'
#' @details Input data sample:
#' \preformatted{
#' YY MM LAT5 LON5 DAYS SETS_UNA SETS_LOG ... SKJ_C_UNA YFT_C_UNA BET_C_UNA OTH_C_UNA ...
#' 1967  2  30N 135E    0        0        0 ...         0         0         0         0 ...
#' }
#' Output data sample:
#' \preformatted{
#' fishing_fleet gear_type time_start time_end geographic_identifier fishing_mode species ...
#' ALL           S         1970-01-01 1970-02-01 6100135             LOG          BET     ...
#' }
#'
#' @seealso \code{WCPFC_CE_catches_pivotDSD_to_harmonizedDSD} (R/sardara_functions).
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
gta_rawdata_wcpfc_catch_ps <- function(data) {
  DF <- gta_data(data, reader = gta_read_csv_utf8)

  ## Catches: reach the catches pivot data structure
  # - to upper colnames
  # - remove the CWP grid code
  colnames(DF) <- toupper(colnames(DF))
  DF$CWP_GRID <- NULL

  DF <- tidyr::gather(DF, variable, value, -c(colnames(DF[1:10])))

  DF <- dplyr::filter(DF, !value %in% 0)
  DF <- dplyr::filter(DF, !is.na(value))
  DF$variable <- as.character(DF$variable)
  colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
  DF$School <- substr(DF$Species, 7, nchar(DF$Species))
  DF$Species <- sub("_C_UNA", "", DF$Species)
  DF$Species <- sub("_C_LOG", "", DF$Species)
  DF$Species <- sub("_C_DFAD", "", DF$Species)
  DF$Species <- sub("_C_AFAD", "", DF$Species)
  DF$Species <- sub("_C_OTH", "", DF$Species)
  DF$CatchUnits <- "t"
  DF$EffortUnits <- colnames(DF[5])
  colnames(DF)[5] <- "Effort"
  catches_pivot_WCPFC <- DF; rm(DF)

  # Gear
  catches_pivot_WCPFC$Gear <- "S"

  ## Reach the catches harmonized data structure
  colToKeep_captures <- c("FishingFleet", "Gear", "time_start", "time_end", "AreaName",
                          "School", "Species", "CatchType", "CatchUnits", "Catch")
  catches <- WCPFC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_WCPFC, colToKeep_captures)

  colnames(catches) <- c("fishing_fleet", "gear_type", "time_start", "time_end",
                         "geographic_identifier", "fishing_mode", "species",
                         "measurement_type", "measurement_unit", "measurement_value")
  catches$source_authority <- "WCPFC"
  catches$measurement_type <- "RC" # Retained catches
  catches$measurement <- "catch"
  catches$measurement_processing_level <- "raised"
  catches$time_start <- as.Date(catches$time_start)
  catches$time_end <- as.Date(catches$time_end)
  catches
}
