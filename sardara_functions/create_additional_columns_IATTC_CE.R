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
