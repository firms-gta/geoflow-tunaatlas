harmo_spatial_5 = function (df_input, colname_latitude, colname_longitude, SquareSize, 
          CodeSquareSize) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_1.R")
  indice.quad.3 <- which(df_input[, colname_longitude] <= 
                           0)
  indice.quad.2 <- which(df_input[, colname_longitude] > 0)
  df_input$quadrant <- 9
  df_input[indice.quad.2, "quadrant"] <- 2
  df_input[indice.quad.3, "quadrant"] <- 3
  df_input$SquareSize <- CodeSquareSize
  df_input[, colname_longitude] <- abs(df_input[, colname_longitude])
  df_input[, colname_latitude] <- abs(df_input[, colname_latitude])
  df_input <- harmo_spatial_1(df_input, colname_longitude, 
                              colname_latitude, "quadrant", "SquareSize")
  return(df_input)
}
