harmo_time_3 = function (df_input, colname_year, colname_monthstart, colname_monthstop) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_time_1.R")  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
  df_input$Period <- NA
  index1 <- which(df_input[, colname_monthstart] == df_input[, 
                                                             colname_monthstop])
  index17 <- which(df_input[, colname_monthstop] - df_input[, 
                                                            colname_monthstart] == 11)
  index13 <- which((df_input[, colname_monthstop] - df_input[, 
                                                             colname_monthstart] == 2) & (df_input[, colname_monthstart] == 
                                                                                            1))
  index14 <- which((df_input[, colname_monthstop] - df_input[, 
                                                             colname_monthstart] == 2) & (df_input[, colname_monthstart] == 
                                                                                            4))
  index15 <- which((df_input[, colname_monthstop] - df_input[, 
                                                             colname_monthstart] == 2) & (df_input[, colname_monthstart] == 
                                                                                            7))
  index16 <- which((df_input[, colname_monthstop] - df_input[, 
                                                             colname_monthstart] == 2) & (df_input[, colname_monthstart] == 
                                                                                            10))
  index18 <- which((df_input[, colname_monthstop] - df_input[, 
                                                             colname_monthstart] == 5) & (df_input[, colname_monthstart] == 
                                                                                            1))
  index19 <- which((df_input[, colname_monthstop] - df_input[, 
                                                             colname_monthstart] == 5) & (df_input[, colname_monthstart] == 
                                                                                            7))
  if (length(index1) > 1) {
    df_input[index1, "Period"] <- df_input[index1, colname_monthstart]
  }
  if (length(index13) > 1) {
    df_input[index13, "Period"] <- 13
  }
  if (length(index14) > 1) {
    df_input[index14, "Period"] <- 14
  }
  if (length(index15) > 1) {
    df_input[index15, "Period"] <- 15
  }
  if (length(index16) > 1) {
    df_input[index16, "Period"] <- 16
  }
  if (length(index17) > 1) {
    df_input[index17, "Period"] <- 17
  }
  if (length(index18) > 1) {
    df_input[index18, "Period"] <- 18
  }
  if (length(index19) > 1) {
    df_input[index19, "Period"] <- 19
  }
  colnames(df_input)[which(colnames(df_input) == colname_year)] <- "Year"
  df_input <- harmo_time_1(df_input, "Year", "Period")
  df_input <- format_time_db_format(df_input)
  return(df_input)
}
