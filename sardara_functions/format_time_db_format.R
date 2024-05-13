format_time_db_format = function (df_input) 
{
  if(!require(lubridate)){
    install.packages("lubridate")
    require(lubridate)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  
  df_input$time_start <- as.Date(paste(df_input$Year, "-", df_input$MonthStart, "-01", sep = ""))
  df_input$time_end <- as.Date(df_input$time_start + months(1) - days(1))
  df_input$time_start <- as.character(df_input$time_start)
  df_input$time_end <- as.character(df_input$time_end)
  return(df_input)
}
