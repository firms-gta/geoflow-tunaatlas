harmo_spatial_3 = function (df_input, colname_latitude, colname_longitude, SquareSize, 
          CodeSquareSize) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_1.R")
  df_input$Lon_W_or_E <- substr(df_input[, colname_longitude], 
                                nchar(df_input[, colname_longitude]), nchar(df_input[, 
                                                                                     colname_longitude]))
  df_input$Lat_N_or_S <- substr(df_input[, colname_latitude], 
                                nchar(df_input[, colname_latitude]), nchar(df_input[, 
                                                                                    colname_latitude]))
  df_input[, colname_longitude] <- sub("W", "", df_input[, 
                                                         colname_longitude])
  df_input[, colname_longitude] <- sub("E", "", df_input[, 
                                                         colname_longitude])
  df_input[, colname_latitude] <- sub("N", "", df_input[, 
                                                        colname_latitude])
  df_input[, colname_latitude] <- sub("S", "", df_input[, 
                                                        colname_latitude])
  indice.quad.1 <- which(df_input$Lon_W_or_E == "E" & df_input$Lat_N_or_S == 
                           "N")
  indice.quad.2 <- which(df_input$Lon_W_or_E == "E" & df_input$Lat_N_or_S == 
                           "S")
  indice.quad.3 <- which(df_input$Lon_W_or_E == "W" & df_input$Lat_N_or_S == 
                           "S")
  indice.quad.4 <- which(df_input$Lon_W_or_E == "W" & df_input$Lat_N_or_S == 
                           "N")
  df_input$quadrant <- 9
  df_input[indice.quad.1, "quadrant"] <- 1
  df_input[indice.quad.2, "quadrant"] <- 2
  df_input[indice.quad.3, "quadrant"] <- 3
  df_input[indice.quad.4, "quadrant"] <- 4
  df_input[, colname_latitude] <- as.numeric(df_input[, colname_latitude])
  df_input[, colname_longitude] <- as.numeric(df_input[, colname_longitude])
  if (length(indice.quad.4)) {
    df_input[indice.quad.4, colname_longitude] <- df_input[indice.quad.4, 
                                                           colname_longitude] - SquareSize
  }
  if (length(indice.quad.2)) {
    df_input[indice.quad.2, colname_latitude] <- df_input[indice.quad.2, 
                                                          colname_latitude] - SquareSize
  }
  if (length(indice.quad.3)) {
    df_input[indice.quad.3, colname_latitude] <- df_input[indice.quad.3, 
                                                          colname_latitude] - SquareSize
    df_input[indice.quad.3, colname_longitude] <- df_input[indice.quad.3, 
                                                           colname_longitude] - SquareSize
  }
  df_input$SquareSize <- CodeSquareSize
  df_input <- harmo_spatial_1(df_input, colname_longitude, 
                              colname_latitude, "quadrant", "SquareSize")
  return(df_input)
}
