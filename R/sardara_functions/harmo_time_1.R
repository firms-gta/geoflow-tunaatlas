harmo_time_1 = function (df_input, colname_year, colname_timeperiod) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/format_time_db_format.R")
  df_input[, "MonthStart"] <- NA
  index1to12 <- which(df_input[, colname_timeperiod] >= 1 & 
                        df_input[, colname_timeperiod] <= 12)
  if (length(index1to12) > 1) {
    df_input[index1to12, "MonthStart"] <- df_input[index1to12, 
                                                   colname_timeperiod]
    df_input[index1to12, colname_timeperiod] <- 1
  }
  index17 <- which(df_input[, colname_timeperiod] == 17)
  if (length(index17) > 1) {
    df_input[index17, "MonthStart"] <- 1
    df_input[index17, colname_timeperiod] <- 12
  }
  index13 <- which(df_input[, colname_timeperiod] == 13)
  if (length(index13) > 1) {
    df_input[index13, "MonthStart"] <- 1
    df_input[index13, colname_timeperiod] <- 3
  }
  index14 <- which(df_input[, colname_timeperiod] == 14)
  if (length(index14) > 1) {
    df_input[index14, "MonthStart"] <- 4
    df_input[index14, colname_timeperiod] <- 3
  }
  index15 <- which(df_input[, colname_timeperiod] == 15)
  if (length(index15) > 1) {
    df_input[index15, "MonthStart"] <- 7
    df_input[index15, colname_timeperiod] <- 3
  }
  index16 <- which(df_input[, colname_timeperiod] == 16)
  if (length(index16) > 1) {
    df_input[index16, "MonthStart"] <- 10
    df_input[index16, colname_timeperiod] <- 3
  }
  index18 <- which(df_input[, colname_timeperiod] == 18)
  if (length(index18) > 1) {
    df_input[index18, "MonthStart"] <- 1
    df_input[index18, colname_timeperiod] <- 6
  }
  index19 <- which(df_input[, colname_timeperiod] == 19)
  if (length(index19) > 1) {
    df_input[index19, "MonthStart"] <- 7
    df_input[index19, colname_timeperiod] <- 6
  }
  colnames(df_input)[which(colnames(df_input) == colname_year)] <- "Year"
  colnames(df_input)[which(colnames(df_input) == colname_timeperiod)] <- "Period"
  df_input <- format_time_db_format(df_input)
  return(df_input)
}
