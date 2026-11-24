format_time_db_format <- function(df_input) {
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    install.packages("lubridate")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(lubridate)
  library(dplyr)
  
  df_input <- df_input %>%
    dplyr::mutate(
      time_start = as.Date(paste(Year, MonthStart, "01", sep = "-")),
      time_end = as.Date(time_start %m+% months(Period)) - days(1),
      time_start = as.character(time_start),
      time_end = as.character(time_end)
    )
  
  return(df_input)
}
