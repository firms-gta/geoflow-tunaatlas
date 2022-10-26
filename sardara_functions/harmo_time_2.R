harmo_time_2 = function (df_input, colname_year, colname_month) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
  colnames(df_input)[which(colnames(df_input) == colname_year)] <- "Year"
  colnames(df_input)[which(colnames(df_input) == colname_month)] <- "MonthStart"
  df_input$Period <- 1
  index_na <- which(is.na(df_input$MonthStart))
  if (length(index_na) > 0) {
    df_input$Period[index_na] = 12
    df_input$MonthStart[index_na] = 1
  }
  df_input <- format_time_db_format(df_input)
  return(df_input)
}
