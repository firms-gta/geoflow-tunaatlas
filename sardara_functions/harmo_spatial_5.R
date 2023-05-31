harmo_spatial_5 = function (df_input, colname_latitude, colname_longitude, SquareSize, 
          CodeSquareSize) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_1.R")
  indice.quad.4 <- which(df_input[, colname_longitude] >= 
                           -180 & df_input[, colname_longitude] <= 0 & df_input[, 
                                                                                colname_latitude] >= 0 & df_input[, colname_latitude] <= 
                           180)
  indice.quad.3 <- which(df_input[, colname_longitude] >= 
                           -180 & df_input[, colname_longitude] <= 0 & df_input[, 
                                                                                colname_latitude] >= -180 & df_input[, colname_latitude] <= 
                           0)
  indice.quad.2 <- which(df_input[, colname_longitude] >= 
                           0 & df_input[, colname_longitude] <= 180 & df_input[, 
                                                                               colname_latitude] >= -180 & df_input[, colname_latitude] <= 
                           0)
  indice.quad.1 <- which(df_input[, colname_longitude] >= 
                           0 & df_input[, colname_longitude] <= 180 & df_input[, 
                                                                               colname_latitude] >= 0 & df_input[, colname_latitude] <= 
                           180)
  df_input$quadrant <- 9
  df_input[indice.quad.1, "quadrant"] <- 1
  df_input[indice.quad.2, "quadrant"] <- 2
  df_input[indice.quad.3, "quadrant"] <- 3
  df_input[indice.quad.4, "quadrant"] <- 4
  df_input$SquareSize <- CodeSquareSize
  
  # the minimum longitude of the CCSBT data is -180. As the point described in lat/long is in the top left corner of the grid, but 
  # the cwp grid (in the third quadrant is on the top right)  we need to shift the longitude data by the size of the grid
  
  if (length(indice.quad.1)) {
    df_input[indice.quad.1, colname_latitude] <- df_input[indice.quad.1, 
                                                          colname_latitude] - SquareSize
  }

  # indice.quad.2 not to changed
  
    if (length(indice.quad.3)) {
    df_input[indice.quad.3, colname_longitude] <- df_input[indice.quad.3, 
                                                           colname_longitude] + SquareSize
  }
  if (length(indice.quad.4)) {
    df_input[indice.quad.4, colname_longitude] <- df_input[indice.quad.4, 
                                                           colname_longitude] + SquareSize
    df_input[indice.quad.4, colname_latitude] <- df_input[indice.quad.4, 
                                                          colname_latitude] - SquareSize
  }
  
  df_input[, colname_longitude] <- abs(df_input[, colname_longitude])
  df_input[, colname_latitude] <- abs(df_input[, colname_latitude])
  df_input <- harmo_spatial_1(df_input, colname_longitude, 
                              colname_latitude, "quadrant", "SquareSize")
  return(df_input)
}
