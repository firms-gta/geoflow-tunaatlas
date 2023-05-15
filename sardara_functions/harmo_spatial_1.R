harmo_spatial_1 = function (df_input, colname_longitude, colname_latitude, colname_quadrant, 
                            colname_squaresize, colname_samplingareacode) 
{
  df_input <- as.data.frame(df_input)
  df_input$SquareSizeCode <- NULL
  indice.5 <- which(df_input[, colname_squaresize] == "1x1")
  indice.6 <- which(df_input[, colname_squaresize] == "5x5")
  indice.7 <- which(df_input[, colname_squaresize] == "10x10")
  indice.8 <- which(df_input[, colname_squaresize] == "20x20")
  indice.9 <- which(df_input[, colname_squaresize] == "30x30")
  indice.2 <- which(df_input[, colname_squaresize] == "10x20")
  indice.1 <- which(df_input[, colname_squaresize] == "5x10")
  indice.none <- which(df_input[, colname_squaresize] == "none")
  indice.ICCAT <- which(df_input[, colname_squaresize] == 
                          "ICCAT")
  indice.LatLon <- which(df_input[, colname_squaresize] == 
                           "LatLon")
  df_input[indice.5, colname_squaresize] <- 5
  df_input[indice.6, colname_squaresize] <- 6
  df_input[indice.7, colname_squaresize] <- 7
  df_input[indice.8, colname_squaresize] <- 8
  df_input[indice.9, colname_squaresize] <- 9
  df_input[indice.1, colname_squaresize] <- 1
  df_input[indice.2, colname_squaresize] <- 2
  df_input[indice.none, colname_squaresize] <- 99
  df_input[indice.ICCAT, colname_squaresize] <- 98
  df_input[indice.LatLon, colname_squaresize] <- 98
  indice.col <- which((colnames(df_input) == colname_longitude) | 
                        (colnames(df_input) == colname_latitude) | (colnames(df_input) == 
                                                                      colname_quadrant))
  indice.longi10 <- which(df_input[, colname_longitude] < 
                            10)
  indice.longi100 <- which((as.numeric(df_input[, colname_longitude]) < 
                              100) & (as.numeric(df_input[, colname_longitude]) >= 
                                        10))
  if (length(indice.longi10)) {
    df_input[indice.longi10, colname_longitude] <- paste("00", 
                                                         df_input[indice.longi10, colname_longitude], sep = "")
  }
  if (length(indice.longi100)) {
    df_input[indice.longi100, colname_longitude] <- paste("0", 
                                                          df_input[indice.longi100, colname_longitude], sep = "")
  }
  indice.lati10 <- which(as.numeric(df_input[, colname_latitude]) < 
                           10)
  if (length(indice.lati10)) {
    df_input[indice.lati10, colname_latitude] <- paste("0", 
                                                       df_input[indice.lati10, colname_latitude], sep = "")
  }
  df_input$AreaCWPgrid <- as.numeric(paste(df_input[, colname_squaresize], 
                                           df_input[, colname_quadrant], df_input[, colname_latitude], 
                                           df_input[, colname_longitude], sep = ""))
  df_input$AreaName <- df_input[, "AreaCWPgrid"]
  df_input$AreaType <- df_input[, colname_squaresize]
  index.indet <- which(df_input[, colname_squaresize] == "99")
  if (length(index.indet)) {
    df_input[index.indet, "AreaName"] <- "ALL"
    df_input[index.indet, "AreaCWPgrid"] <- NA
  }
  index.Get_SampAreaCode_instead_of_SquareCodeList <- which(df_input[, 
                                                                     colname_squaresize] == "98")
  if (length(index.Get_SampAreaCode_instead_of_SquareCodeList)) {
    df_input[index.Get_SampAreaCode_instead_of_SquareCodeList, 
             "AreaName"] <- df_input[index.Get_SampAreaCode_instead_of_SquareCodeList, 
                                     colname_samplingareacode]
    df_input[index.Get_SampAreaCode_instead_of_SquareCodeList, 
             "AreaCWPgrid"] <- NA
  }
  return(df_input)
}
