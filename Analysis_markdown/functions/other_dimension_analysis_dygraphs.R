#' Calculate and Visualize Data Distribution for Various Dimensions Using dygraphs
#'
#' This function calculates and visualizes the data distribution for various dimensions
#' using interactive time-series plots with dygraphs for two datasets or a single dataset.
#'
#' @param Other_dimensions A vector of dimensions to analyze.
#' @param init Initial dataset.
#' @param final Final dataset (optional).
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset (optional).
#' @param unique_analyse Logical indicating whether the analysis is unique.
#' @param fig.path Path to save the figures (optional).
#' @param topn The number of top categories to include in the chart.
#'
#' @return A list containing the dygraphs for each dimension.
#' @examples
#' \dontrun{
#' data_dimension_analysis_dygraphs(c("Dimension1", "Dimension2"), init, final, "Dataset1", "Dataset2", FALSE)
#' }
#' @export
other_dimension_analysis_dygraphs <- function(Other_dimensions, init, final = NULL, titre_1 = "Dataset1", titre_2 = "Dataset2", unique_analyse = FALSE, fig.path = NULL, topn = 7) {
  library(dplyr)
  library(dygraphs)
  library(xts)
  
  # Check if final dataset is empty, meaning a unique analysis is needed
  if (is.null(final) || nrow(final) == 0) {
    unique_analyse <- TRUE
  }
  
  # Function to create a dygraph for a given dimension
  dygraph_function <- function(dimension, first, second = NULL, titre_1 = "first", titre_2 = "second", topn = 5) {
    # Process the first dataset
    provisoire_i <- first %>%
      dplyr::group_by(dplyr::across(c(dimension, "measurement_unit"))) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Time = as.Date(measurement_unit)) %>%
      dplyr::select(Time, measurement_value)
    
    # Create xts object for the first dataset
    first_xts <- xts(provisoire_i$measurement_value, order.by = provisoire_i$Time)
    colnames(first_xts) <- titre_1
    
    if (!is.null(second)) {
      # Process the second dataset
      provisoire_t <- second %>%
        dplyr::group_by(dplyr::across(c(dimension, "measurement_unit"))) %>%
        dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Time = as.Date(measurement_unit)) %>%
        dplyr::select(Time, measurement_value)
      
      # Create xts object for the second dataset
      second_xts <- xts(provisoire_t$measurement_value, order.by = provisoire_t$Time)
      colnames(second_xts) <- titre_2
      
      # Merge the two xts objects
      combined_xts <- merge(first_xts, second_xts, join = "inner")
      
      # Create the dygraph for both datasets
      dygraph <- dygraph(combined_xts, main = paste("Comparison of", dimension)) %>%
        dyAxis("y", label = "Measurement Value") %>%
        dyLegend(show = "always") %>%
        dyRangeSelector()  # Add interactive range selector
      
    } else {
      # Create the dygraph for the single dataset
      dygraph <- dygraph(first_xts, main = paste("Distribution of", dimension)) %>%
        dyAxis("y", label = "Measurement Value") %>%
        dyLegend(show = "always") %>%
        dyRangeSelector()  # Add interactive range selector
    }
    
    return(dygraph)
  }
  
  # Generate dygraphs for all dimensions
  if (!unique_analyse) {
    figures <- lapply(Other_dimensions, FUN = dygraph_function, first = init, second = final, titre_1 = titre_1, titre_2 = titre_2, topn = topn)
  } else {
    figures <- lapply(Other_dimensions, FUN = dygraph_function, first = init, titre_1 = titre_1, topn = topn)
  }
  
  dimension_title_subfigures <- gsub("_", ".", paste0("Distribution for the dimension: ", Other_dimensions))
  
  return(list(
    figures = figures,
    dimension_title_subfigures = dimension_title_subfigures
  ))
}
