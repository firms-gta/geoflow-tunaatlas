#' FUN_catches_IATTC_CE_Flag_or_SetType
#'
#' Downloads and processes catch data from the Inter-American Tropical Tuna Commission (IATTC), 
#' based on the flag of the vessel or the type of fishing set. It aggregates the data according 
#' to a specified dimension, applies filters to exclude null or missing values, and adds additional 
#' columns for latitude, longitude, and fishing gear code. This function requires the `data.table`, 
#' `reshape2`, and `dplyr` packages to be installed and loaded in the R environment.
#'
#' @param Path_to_IATTC_CE Path to the CSV file containing IATTC catch data.
#' @param aggregation_dimension String specifying the data aggregation dimension ('Flag' for vessel flag, 
#' 'SetType' for type of fishing set).
#' @param GearCode The gear code used to filter and process the data.
#'
#' @return A `data.frame` containing the aggregated and processed data.
#'
#' @details The function begins by loading necessary functions from online scripts, then reads and transforms 
#' the specified CSV file into a `data.frame`. The data is then melted for easier aggregation, and filters are 
#' applied to remove records with null or missing values. Additional columns are added to enrich the data with 
#' geographical and gear information. Depending on the chosen aggregation dimension, default values are assigned 
#' to the `SetType` or `Flag` columns, and the name of the `Flag` column is changed to `FishingFleet`.
#'
#' @examples
#' # Example of function usage with a fictional file path
#' FUN_catches_IATTC_CE_Flag_or_SetType("path/to/IATTC_data.csv", "Flag", "PS")
#'
#' @import data.table
#' @importFrom dplyr filter
#' @importFrom reshape2 melt
#' @export
#'
#' @seealso \code{\link{create_additional_columns_IATTC_CE}} for details on the additional column creation function

FUN_catches_IATTC_CE_Flag_or_SetType = function (Path_to_IATTC_CE, aggregation_dimension, GearCode) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/create_additional_columns_IATTC_CE.R")
  IATTC_CE <- read.table(Path_to_IATTC_CE, sep = ",", header = TRUE, 
                         stringsAsFactors = FALSE, strip.white = TRUE)
  IATTC_CE <- data.table(IATTC_CE)
  IATTC_CE <- melt(IATTC_CE, id.vars = c("Year", "Month", 
                                         aggregation_dimension, "LatC1", "LonC1", "NumSets"))
  IATTC_CE <- as.data.frame(IATTC_CE)
  IATTC_CE <- IATTC_CE %>% dplyr::filter(!value %in% 0) %>% 
    dplyr::filter(!is.na(value))
  IATTC_CE$variable <- as.character(IATTC_CE$variable)
  IATTC_CE <- create_additional_columns_IATTC_CE(IATTC_CE, 
                                                 "LatC1", "LonC1", 1, GearCode)
  IATTC_CE$CatchUnits <- "MT"
  if (aggregation_dimension == "Flag") {
    IATTC_CE$SetType <- "ALL"
  }
  if (aggregation_dimension == "SetType") {
    IATTC_CE$Flag <- "ALL"
  }
  colnames(IATTC_CE)[colnames(IATTC_CE) == "Flag"] <- "FishingFleet"
  return(IATTC_CE)
}
