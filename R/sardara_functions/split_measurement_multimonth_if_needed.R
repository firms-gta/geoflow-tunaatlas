#' Detect rows that span more than one calendar month (robust version)
detect_multimonth_rows_cleaned <- function(df) {
  if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  
  df <- df %>%
    dplyr::mutate(
      time_start = as.Date(time_start),
      time_end = as.Date(time_end)
    )
  
  !(
    lubridate::floor_date(df$time_start, "month") ==
      lubridate::floor_date(df$time_end - 1, "month")
  )
}


#' Expand multi-month rows into one row per month
expand_multimonth_rows <- function(df) {
  if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  
  df <- df %>%
    dplyr::mutate(
      time_start = as.Date(time_start),
      time_end = as.Date(time_end)
    )
  
  df_expanded <- df %>%
    dplyr::rowwise() %>%
    dplyr::do({
      row <- .
      months_seq <- seq(
        from = lubridate::floor_date(row$time_start, "month"),
        to = lubridate::floor_date(row$time_end - 1, "month"),
        by = "1 month"
      )
      n_months <- length(months_seq)
      dplyr::bind_rows(lapply(months_seq, function(month_start) {
        month_end <- seq(month_start, by = "1 month", length.out = 2)[2] - 1
        row$time_start <- month_start
        row$time_end <- month_end
        row$measurement_value <- row$measurement_value / n_months
        row
      }))
    }) %>%
    dplyr::ungroup()
  
  return(as.data.frame(df_expanded))
}


#' Main function: detect, expand, recombine, and log
split_measurement_multimonth_if_needed <- function(df) {
  if (!requireNamespace("futile.logger", quietly = TRUE)) install.packages("futile.logger")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  
  futile.logger::flog.info("Starting full split_measurement_multimonth_if_needed on %d rows.", nrow(df))
  
  multi_month_mask <- detect_multimonth_rows_cleaned(df)
  df_to_split <- df[multi_month_mask, ]
  df_unchanged <- df[!multi_month_mask, ]
  
  futile.logger::flog.info("Found %d multi-month rows to split.", nrow(df_to_split))
  futile.logger::flog.info("Keeping %d rows unchanged.", nrow(df_unchanged))
  
  df_expanded <- expand_multimonth_rows(df_to_split)
  final_df <- dplyr::bind_rows(df_unchanged, df_expanded)
  
  futile.logger::flog.info("Final dataframe has %d rows.", nrow(final_df))
  
  # Afficher les lignes modifiées uniquement
  print("Head of rows that were expanded from multi-month ranges:")
  print(utils::head(df_expanded))
  
  return(final_df)
}


# Appliquer le traitement
# b_cleaned <- split_measurement_multimonth_if_needed(b)
