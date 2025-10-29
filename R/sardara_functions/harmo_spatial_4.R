harmo_spatial_4 = function (df_input, colname_latitude, colname_longitude, colname_squaresize, 
          ColCodeSquareSize) 
{
  # function to create CWP grid from lat long provided by IATTC.
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/harmo_spatial_1.R")
  indice.quad.4 <- which(df_input[, colname_longitude] > -180 & df_input[, colname_longitude] < 0 & df_input[, colname_latitude] > 0 & df_input[, colname_latitude] < 90)
  indice.quad.3 <- which(df_input[, colname_longitude] > -180 & df_input[, colname_longitude] < 0 & df_input[, colname_latitude] > -90 & df_input[, colname_latitude] < 0)
  indice.quad.2 <- which(df_input[, colname_longitude] > 0 & df_input[, colname_longitude] < 180 & df_input[, colname_latitude] > -90 & df_input[, colname_latitude] < 0)
  indice.quad.1 <- which(df_input[, colname_longitude] > 0 & df_input[, colname_longitude] < 180 & df_input[, colname_latitude] > 0 & df_input[, colname_latitude] < 90)
  df_input$quadrant <- 9
  df_input[indice.quad.1, "quadrant"] <- 1
  df_input[indice.quad.2, "quadrant"] <- 2
  df_input[indice.quad.3, "quadrant"] <- 3
  df_input[indice.quad.4, "quadrant"] <- 4
  
  # IATTC provide lat/lon of the centroid of the grid, we need to change them before creating CWP grid code corresponding
  if (length(indice.quad.4)) {
    df_input[indice.quad.4, colname_longitude] <- df_input[indice.quad.4, colname_longitude] + df_input[indice.quad.4, colname_squaresize]/2
    df_input[indice.quad.4, colname_latitude] <- df_input[indice.quad.4, colname_latitude] - df_input[indice.quad.4, colname_squaresize]/2
  }
  if (length(indice.quad.2)) {
    df_input[indice.quad.2, colname_longitude] <- df_input[indice.quad.2, colname_longitude] - df_input[indice.quad.2, colname_squaresize]/2
    df_input[indice.quad.2, colname_latitude] <- df_input[indice.quad.2, colname_latitude] + df_input[indice.quad.2, colname_squaresize]/2
  }
  if (length(indice.quad.3)) {
    df_input[indice.quad.3, colname_longitude] <- df_input[indice.quad.3, colname_longitude] + df_input[indice.quad.3, colname_squaresize]/2
    df_input[indice.quad.3, colname_latitude] <- df_input[indice.quad.3, colname_latitude] + df_input[indice.quad.3, colname_squaresize]/2
  }
  if (length(indice.quad.1)) {
    df_input[indice.quad.1, colname_longitude] <- df_input[indice.quad.1, colname_longitude] - df_input[indice.quad.1, colname_squaresize]/2
    df_input[indice.quad.1, colname_latitude] <- df_input[indice.quad.1, colname_latitude] - df_input[indice.quad.1, colname_squaresize]/2
  }
  df_input[, colname_longitude] <- abs(df_input[, colname_longitude])
  df_input[, colname_latitude] <- abs(df_input[, colname_latitude])
  df_input <- harmo_spatial_1(df_input, colname_longitude, colname_latitude, "quadrant", ColCodeSquareSize)
  return(df_input)
}
