raise_get_rf <- function (df_input_incomplete, df_input_total, x_raising_dimensions) 
{
  if ("year" %in% x_raising_dimensions) {
    df_input_incomplete$year <- as.numeric(substr(df_input_incomplete$time_start, 
                                                  0, 4))
    df_input_total$year <- as.numeric(substr(df_input_total$time_start, 
                                             0, 4))
  }
  if (length(setdiff(x_raising_dimensions, colnames(df_input_incomplete))) != 
      0 | length(setdiff(x_raising_dimensions, colnames(df_input_total))) != 
      0) {
    stop("one of the dataframes as input does not have the dimensions set in the dimensions to consider for the raising")
  }
  
  partial <- df_input_incomplete %>%
    dplyr::group_by(across(all_of(x_raising_dimensions))) %>%
    dplyr::summarise(
      sum_value_df_input_incomplete = sum(measurement_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  total <- df_input_total %>%
    dplyr::group_by(across(all_of(x_raising_dimensions))) %>%
    dplyr::summarise(
      sum_value_df_input_total = sum(measurement_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  DFPartialInfo_rf <- full_join(partial, total, by = x_raising_dimensions) %>%
    dplyr::mutate(
      rf = sum_value_df_input_total / sum_value_df_input_incomplete
    )
  
  DFPartialInfo_rf
}