raise_incomplete_dataset_to_total_dataset <- function (df_input_incomplete, df_input_total, df_rf, x_raising_dimensions, 
          decrease_when_rf_inferior_to_one = TRUE, threshold_rf = NULL) 
{
  df_input_incomplete$year <- as.numeric(substr(df_input_incomplete$time_start, 
                                                0, 4))
  df_input_total$year <- as.numeric(substr(df_input_total$time_start, 
                                           0, 4))
  if (length(setdiff(x_raising_dimensions, colnames(df_input_incomplete))) != 
      0 | length(setdiff(x_raising_dimensions, colnames(df_input_total))) != 
      0) {
    stop("one of the dataframes as input does not have the dimensions set in the dimensions to consider for the raising")
  }
  colnames_input_dataset <- colnames(df_input_incomplete)
  sum_df_total <- df_input_total %>% group_by(measurement_unit) %>% summarise(sum_df_total = sum(measurement_value))
  sum_df_incomplete_before_raising <- df_input_incomplete %>% 
    group_by(measurement_unit) %>% summarise(sum_df_incomplete_before_raising = sum(measurement_value))
  sum_df_total_do_not_exist_in_df_incomplete <- left_join(df_input_total, 
                                                          df_rf) %>% filter(!is.na(sum_value_df_input_incomplete)) %>% 
    group_by(measurement_unit) %>% summarise(sum_df_total_do_not_exist_in_df_incomplete = sum(measurement_value))
  index.na.rf <- which(is.na(df_rf$rf))
  if (length(index.na.rf) > 0) {
    df_rf <- df_rf[-index.na.rf, ]
  }
  df_rf <- df_rf[!is.infinite(df_rf$rf), ]
  df_input_incomplete <- left_join(df_input_incomplete, df_rf, 
                                   by = x_raising_dimensions)
  sum_df_incomplete_do_not_exist_in_df_total <- df_input_incomplete %>% 
    filter(is.na(sum_value_df_input_total)) %>% group_by(measurement_unit) %>% 
    summarise(sum_df_incomplete_do_not_exist_in_df_total = sum(measurement_value))
  df_input_incomplete$value_raised <- df_input_incomplete[, 
                                                          "measurement_value"]
  if (decrease_when_rf_inferior_to_one == TRUE) {
    index.rfNotNa <- which(!is.na(df_input_incomplete[, 
                                                      "rf"]))
    df_input_incomplete$value_raised[index.rfNotNa] <- df_input_incomplete[index.rfNotNa, 
                                                                           "measurement_value"] * df_input_incomplete[index.rfNotNa, "rf"]
  }
  else {
    index.rfNotNa <- which(!is.na(df_input_incomplete[, 
                                                      "rf"]) & df_input_incomplete[, "rf"] >= 1)
    df_input_incomplete$value_raised[index.rfNotNa] <- df_input_incomplete[index.rfNotNa, 
                                                                           "measurement_value"] * df_input_incomplete[index.rfNotNa, "rf"]
  }
  if (!is.null(threshold_rf)) {
    index.threshold <- which(df_input_incomplete$rf > threshold_rf)
    df_input_incomplete <- df_input_incomplete[-index.threshold, 
    ]
  }
  dataset_to_return <- df_input_incomplete
  dataset_to_return$measurement_value <- NULL
  colnames(dataset_to_return)[which(names(dataset_to_return) == 
                                      "value_raised")] <- "measurement_value"
  dataset_to_return <- dataset_to_return[colnames_input_dataset]
  dataset_to_return$year <- NULL
  sum_df_incomplete_after_raising <- dataset_to_return %>% 
    group_by(measurement_unit) %>% summarise(sum_df_incomplete_after_raising = sum(measurement_value))
  stats <- sum_df_total %>% full_join(sum_df_incomplete_before_raising) %>% 
    full_join(sum_df_incomplete_after_raising) %>% full_join(sum_df_incomplete_do_not_exist_in_df_total) %>% 
    full_join(sum_df_total_do_not_exist_in_df_incomplete)
  stats[is.na(stats)] <- 0
  stats$perc_df_total_do_not_exist_in_df_incomplete <- stats$sum_df_total_do_not_exist_in_df_incomplete/stats$sum_df_total * 
    100
  stats$perc_df_incomplete_do_not_exist_in_df_total <- stats$sum_df_incomplete_do_not_exist_in_df_total/stats$sum_df_total * 
    100
  stats$perc_df_incomplete_over_df_total_after_raising <- stats$sum_df_incomplete_after_raising/stats$sum_df_total * 
    100
  stats$perc_df_incomplete_over_df_total_before_raising <- stats$sum_df_incomplete_before_raising/stats$sum_df_total * 
    100
  return(list(df = dataset_to_return, stats = stats))
}
