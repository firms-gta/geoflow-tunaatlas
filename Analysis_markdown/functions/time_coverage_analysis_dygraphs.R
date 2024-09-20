#' Calculate and Visualize Time Coverage Using dygraphs
#'
#' This function calculates the time coverage for different dimensions and provides interactive visualizations
#' of the values over time for each dataset using dygraphs.
#'
#' @param time_dimension_list_groupped A list of data frames, each containing time dimension data.
#' @param parameter_time_dimension The time dimension parameter.
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset.
#' @param unique_analyse Logical value indicating whether the analysis is unique.
#'
#' @return A list containing dygraphs objects for visualizing the time coverage.
#' @examples
#' \dontrun{
#' time_coverage_analysis_dygraphs(time_dimension_list_groupped, "Year", "Dataset1", "Dataset2", FALSE)
#' }
#' @export
time_coverage_analysis_dygraphs <- function(time_dimension_list_groupped, parameter_time_dimension, titre_1, titre_2, unique_analyse = FALSE) {
  library(dplyr)
  library(dygraphs)
  library(xts)
  
  # Generate titles based on whether it's a unique analysis
  titles_time <- if (unique_analyse) {
    paste0("Evolutions of values for the dimension ", parameter_time_dimension, " for ", titre_1, " dataset ")
  } else {
    paste0("Evolutions of values for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " datasets ")
  }
  
  # Prepare the data for dygraphs
  time_dimension_list_groupped_diff <- lapply(time_dimension_list_groupped, function(x) {
    x %>%
      dplyr::mutate(Time = as.Date(Precision)) %>%  # Convert time to Date for dygraphs
      dplyr::rename(`Values dataset 1` = "value_sum_1", `Values dataset 2` = "value_sum_2") %>%
      pivot_longer(cols = c(`Values dataset 1`, `Values dataset 2`), names_to = "Dataset", values_to = "Values") %>%
      dplyr::mutate(Dataset = dplyr::case_when(Dataset == "Values dataset 1" ~ titre_1, TRUE ~ titre_2)) %>%
      dplyr::distinct()
  })
  
  # Filter non-zero values if unique_analyse is TRUE
  if (unique_analyse) {
    time_dimension_list_groupped_diff <- lapply(time_dimension_list_groupped_diff, function(x) {
      x %>% filter(Values != 0)
    })
  }
  
  # Create dygraphs for each time dimension
  time_dimension_list_groupped_diff_image <- lapply(time_dimension_list_groupped_diff, function(x) {
    # Extract relevant data for dygraphs
    time_values <- x$Time
    dataset1_values <- x %>% filter(Dataset == titre_1) %>% select(Values)
    dataset2_values <- x %>% filter(Dataset == titre_2) %>% select(Values)
    
    # Create xts object for the first dataset
    first_xts <- xts(dataset1_values, order.by = time_values)
    colnames(first_xts) <- titre_1
    
    if (!unique_analyse) {
      # Create xts object for the second dataset
      second_xts <- xts(dataset2_values, order.by = time_values)
      colnames(second_xts) <- titre_2
      
      # Merge the two xts objects
      combined_xts <- merge(first_xts, second_xts, join = "inner")
      
      # Create the dygraph for both datasets
      dygraph <- dygraph(combined_xts, main = paste("Time Coverage for", parameter_time_dimension)) %>%
        dyAxis("y", label = "Values") %>%
        dyLegend(show = "always") %>%
        dyRangeSelector()  # Add interactive range selector
    } else {
      # Create the dygraph for the single dataset
      dygraph <- dygraph(first_xts, main = paste("Time Coverage for", parameter_time_dimension)) %>%
        dyAxis("y", label = "Values") %>%
        dyLegend(show = "always") %>%
        dyRangeSelector()  # Add interactive range selector
    }
    
    return(dygraph)
  })
  
  return(list(titles = titles_time, plots = time_dimension_list_groupped_diff_image))
}
