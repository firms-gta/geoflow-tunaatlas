harmo_spatial_2 = function (df_input, ColIrrArea) 
{
  df_input$AreaType <- as.numeric(substr(df_input[, ColIrrArea], 
                                         0, 1))
  indice.na <- which(is.na(df_input[, "AreaType"]))
  df_input[indice.na, "AreaType"] <- 99
  indice.3_to_7 <- which(df_input[, "AreaType"] == 3)
  df_input[indice.3_to_7, "AreaType"] <- 7
  df_input[indice.3_to_7, "AreaCWPgrid"] <- as.numeric(paste("7", 
                                                             substr(df_input[indice.3_to_7, "AreaCWPgrid"], 2, 7), 
                                                             sep = ""))
  df_input[indice.3_to_7, "AreaName"] <- paste("7", substr(df_input[indice.3_to_7, 
                                                                    "AreaCWPgrid"], 2, 7), sep = "")
  indice.4_to_8 <- which(df_input[, "AreaType"] == 4)
  df_input[indice.4_to_8, "AreaType"] <- 8
  df_input[indice.4_to_8, "AreaCWPgrid"] <- as.numeric(paste("8", 
                                                             substr(df_input[indice.4_to_8, "AreaCWPgrid"], 2, 7), 
                                                             sep = ""))
  df_input[indice.4_to_8, "AreaName"] <- paste("8", substr(df_input[indice.4_to_8, 
                                                                    "AreaCWPgrid"], 2, 7), sep = "")
  return(df_input)
}
