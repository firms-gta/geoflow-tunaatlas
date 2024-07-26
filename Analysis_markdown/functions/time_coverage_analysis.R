#' Calculate and Visualize Time Coverage
#'
#' This function calculates the time coverage for different dimensions and provides visualizations
#' of the values over time for each dataset.
#'
#' @param time_dimension_list_groupped A list of data frames, each containing time dimension data.
#' @param parameter_time_dimension The time dimension parameter.
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset.
#' @param unique_analyse Logical value indicating whether the analysis is unique.
#'
#' @return A list containing ggplot objects for visualizing the time coverage.
#' @examples
#' \dontrun{
#' time_coverage_analysis(time_dimension_list_groupped, "Year", "Dataset1", "Dataset2", FALSE, "path/to/save")
#' }
#' @export
#' @author
#' Bastien Grasset, \email{bastien.grasset@@ird.fr}
time_coverage_analysis <- function(time_dimension_list_groupped, parameter_time_dimension, titre_1, titre_2, unique_analyse = FALSE) {
  library(dplyr)
  library(ggplot2)
  
  titles_time <- if (unique_analyse) {
    paste0("Evolutions of values for the dimension ", parameter_time_dimension, " for ", titre_1, " dataset ")
  } else {
    paste0("Evolutions of values for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " dataset ")
  }
  
  time_dimension_list_groupped_diff <- lapply(time_dimension_list_groupped, function(x) {
    x %>% dplyr::mutate(Time = as.Date(Precision)) %>%
      dplyr::rename(`Values dataset 1` = "value_sum_1", `Values dataset 2` = "value_sum_2") %>%
      pivot_longer(cols = c(`Values dataset 1`, `Values dataset 2`), names_to = "Dataset", values_to = "Values") %>%
      dplyr::mutate(Dataset = dplyr::case_when(Dataset == "Values dataset 1" ~ titre_1, TRUE ~ titre_2)) %>%
      dplyr::distinct()
  })
  
  if (unique_analyse) {
    time_dimension_list_groupped_diff <- lapply(time_dimension_list_groupped_diff, function(x) {
      x %>% filter(Values != 0)
    })
  }
  
  time_dimension_list_groupped_diff_image <- lapply(time_dimension_list_groupped_diff, function(x) {
    ggplot(x) +
      aes(x = Time, y = Values, fill = Dataset, colour = Dataset, group = Dataset) +
      geom_line(size = 0.5) +
      scale_fill_hue(direction = 1) +
      scale_color_hue(direction = 1) +
      theme_bw() +
      theme(legend.position = "top") +
      facet_wrap(vars(measurement_unit), nrow = 2L) +
      labs(x = unique(x$Dimension), y = "Values") +
      facet_grid("measurement_unit", scales = "free_y")
  })
  
  return(list(titles = titles_time, plots = time_dimension_list_groupped_diff_image))
}
