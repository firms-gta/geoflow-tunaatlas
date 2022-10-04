format_time_db_format = function (df_input) 
{
  df_input$time_start <- as.Date(paste(df_input[, "Year"], 
                                       "-", df_input[, "MonthStart"], "-01", sep = ""))
  df_input$time_end <- df_input$time_start + months(df_input[, 
                                                             "Period"]) - days(1)
  df_input$time_start <- as.character(df_input$time_start)
  df_input$time_end <- as.character(df_input$time_end)
  return(df_input)
}
