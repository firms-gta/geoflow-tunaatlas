#' FUN_catches_IATTC_CE_Flag_or_SetType_Shark
#'
#' Processes shark catch data from the Inter-American Tropical Tuna Commission (IATTC) dataset, specifically for
#' aggregating by the flag of the vessel or the type of fishing set and adjusting catch units. This function builds
#' upon `FUN_catches_IATTC_CE_Flag_or_SetType` by initially processing the data using the specified parameters and
#' then modifies the catch units based on the variables related to metric tonnes or numbers.
#'
#' @param Path_to_IATTC_CE The path to the CSV file containing IATTC catch data.
#' @param aggregation_dimension A string specifying the dimension for data aggregation ('Flag' for vessel flag, 'SetType'
#' for type of fishing set).
#' @param GearCode The gear code used to filter and process the data specific to sharks.
#'
#' @return A unique `data.frame` containing the processed shark catch data with updated catch units.
#'
#' @details After processing the initial dataset with `FUN_catches_IATTC_CE_Flag_or_SetType`, this function further
#' customizes the data for shark catches. It initializes catch units to a default value, then updates these units
#' based on the presence of specific indicators ('mt' for metric tonnes, 'n' for numbers) in the variable names.
#' These indicators are also removed from the variable names to standardize them. Finally, it ensures the uniqueness
#' of the dataset to prevent duplicate records.
#'
#' @examples
#' # Example usage with a hypothetical file path
#' FUN_catches_IATTC_CE_Flag_or_SetType_Shark("path/to/IATTC_shark_data.csv", "Flag", "PS")
#'
#' @export
#' @seealso \code{\link{FUN_catches_IATTC_CE_Flag_or_SetType}} for initial data processing.

FUN_catches_IATTC_CE_Flag_or_SetType_Shark = function (Path_to_IATTC_CE, aggregation_dimension, GearCode) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/FUN_catches_IATTC_CE_Flag_or_SetType.R")
  IATTC_CE <- FUN_catches_IATTC_CE_Flag_or_SetType(Path_to_IATTC_CE, 
                                                   aggregation_dimension, GearCode)
  IATTC_CE$CatchUnits <- "init"
  index.mt <- grep("mt", IATTC_CE$variable)
  index.n <- grep("n", IATTC_CE$variable)
  IATTC_CE[index.mt, "CatchUnits"] <- "MT"
  IATTC_CE[index.n, "CatchUnits"] <- "NO"
  IATTC_CE$variable <- gsub("n", "", IATTC_CE$variable)
  IATTC_CE$variable <- gsub("mt", "", IATTC_CE$variable)
  IATTC_CE <- unique(IATTC_CE)
  return(IATTC_CE)
}
