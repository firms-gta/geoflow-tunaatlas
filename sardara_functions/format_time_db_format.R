format_time_db_format = function (df_input) 
{
  if(!require(lubridate)){install.packages("lubridate")
    require(lubridate)}
  if(!require(dplyr)){install.packages("dplyr")
    require(dplyr)}
  
  df_input <- df_input %>%rowwise() %>% dplyr::mutate(time_start = as.Date(paste(Year, "-", MonthStart, "-01", sep = "")))
  df_input <- df_input %>%rowwise() %>% dplyr::mutate(time_end = as.Date(time_start + months(Period) - lubridate::days(1)))
  df_input$time_start <- as.character(df_input$time_start)
  df_input$time_end <- as.character(df_input$time_end)
  return(df_input)
}
