compare_temporal_differences_dygraphs <- function(parameter_time_dimension, init, final, titre_1, titre_2, unique_analyse = FALSE) {
  require(dplyr)
  require(dygraphs)
  require(xts)
  
  # Combine and group data across time dimensions
  Groupped_all_time <- data.frame()
  for (i in parameter_time_dimension) {
    temporaire <- fonction_groupement(i, init, final)
    Groupped_all_time <- rbind(Groupped_all_time, temporaire)
  }
  
  # Function to create a dygraph for each measurement unit
  create_dygraph_for_unit <- function(data, filtering_unit, measurement_unit) {
    filtered_data <- data %>%
      filter(Dimension == filtering_unit, measurement_unit == measurement_unit) %>%
      dplyr::mutate(Time = as.Date(Precision)) %>% 
      arrange(Time) %>%  # Make sure data is sorted by time
      distinct(Time, .keep_all = TRUE)  # Ensure unique time values
    
    time_values <- filtered_data$Time
    diff_values <- filtered_data$`Difference (in %)`

    # Create xts object for dygraph
    diff_xts <- xts(diff_values, order.by = time_values)
    colnames(diff_xts) <- paste("Difference (in %)", filtering_unit, measurement_unit)

    # Create dygraph
    dygraph(diff_xts, main = paste("Temporal Difference (%) for", filtering_unit, "-", measurement_unit)) %>%
      dyAxis("y", label = "Difference (in %)") %>%
      dyRangeSelector() %>%
      dyLegend(show = "always")
  }

  # Generate dygraphs for each measurement unit and time dimension
  timediffplot <- lapply(parameter_time_dimension, function(filtering_unit) {
    measurement_units <- unique(Groupped_all_time$measurement_unit[Groupped_all_time$Dimension == filtering_unit])
    
    # Create a list of dygraphs for each measurement unit
    lapply(measurement_units, function(unit) {
      create_dygraph_for_unit(Groupped_all_time, filtering_unit, unit)
    })
  })
  
  titles <- paste0("Difference in percent of value for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " dataset")

  return(list(plots = timediffplot, titles = titles))
}
