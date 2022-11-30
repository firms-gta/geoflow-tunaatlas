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
  DFPartialInfo_ByEachRaisingDimension <- group_by_(df_input_incomplete, 
                                                    .dots = x_raising_dimensions) %>% summarise(value = sum(value))
  DFTotalInfo_ByEachRaisingDimension <- group_by_(df_input_total, 
                                                  .dots = x_raising_dimensions) %>% summarise(value = sum(value))
  DFPartialInfo_rf <- merge(DFPartialInfo_ByEachRaisingDimension, 
                            DFTotalInfo_ByEachRaisingDimension, by = x_raising_dimensions, 
                            all = TRUE)
  colnames(DFPartialInfo_rf)[which(colnames(DFPartialInfo_rf) == 
                                     "value.x")] <- "sum_value_df_input_incomplete"
  colnames(DFPartialInfo_rf)[which(colnames(DFPartialInfo_rf) == 
                                     "value.y")] <- "sum_value_df_input_total"
  DFPartialInfo_rf$rf <- DFPartialInfo_rf$sum_value_df_input_total/DFPartialInfo_rf$sum_value_df_input_incomplete
  cat(paste0("raise_get_rf file output DFPartialInfo_rf has", 
             nrow(DFPartialInfo_rf), "rows \n"))
  return(DFPartialInfo_rf)
}
