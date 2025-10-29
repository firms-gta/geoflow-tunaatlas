#' Create Additional Columns for IATTC CE Data
#'
#' Adds additional columns to a given dataset from the Inter-American Tropical Tuna Commission (IATTC) to 
#' facilitate further analysis. Specifically, it renames latitude and longitude columns to 'Lat' and 'Lon', 
#' adds a column indicating the square size of geographical cells, assigns a code to the square size, and 
#' adds a column for the gear code.
#'
#' @param input_IATTC_CE_file A data frame containing the IATTC CE data.
#' @param ColLat The name of the column in the dataset that contains the latitude values.
#' @param ColLon The name of the column in the dataset that contains the longitude values.
#' @param SquareSizeDegrees The size of the square (in degrees) used for geographical aggregation, this follow the cwp code structuration for geographical data
#' @param GearCode The gear code to be added to the dataset.
#'
#' @return The modified data frame with additional columns.
#'
#' @examples
#' # Example usage:
#' create_additional_columns_IATTC_CE(my_data, "LatitudeColumn", "LongitudeColumn", 5, "PS")
#'
#' @export

create_additional_columns_IATTC_CE = function (input_IATTC_CE_file, ColLat, ColLon, SquareSizeDegrees, 
          GearCode) 
{
  colnames(input_IATTC_CE_file)[which(colnames(input_IATTC_CE_file) == 
                                        ColLat)] <- "Lat"
  colnames(input_IATTC_CE_file)[which(colnames(input_IATTC_CE_file) == 
                                        ColLon)] <- "Lon"
  input_IATTC_CE_file$SquareSize <- SquareSizeDegrees
  if (SquareSizeDegrees == 5) {
    input_IATTC_CE_file$CodeSquareSize <- 6
  }
  if (SquareSizeDegrees == 1) {
    input_IATTC_CE_file$CodeSquareSize <- 5
  }
  input_IATTC_CE_file$Gear <- GearCode
  return(input_IATTC_CE_file)
}
