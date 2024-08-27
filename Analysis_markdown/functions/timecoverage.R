#' Perform Time Coverage Analysis
#'
#' This function analyzes the time coverage of values for specified time dimensions and generates plots of the differences.
#'
#' @param unique_analyse A boolean flag indicating whether to perform a unique analysis. Defaults to FALSE.
#' @param parameter_time_dimension A string representing the time dimension.
#' @param titre_1 A string representing the title for the initial dataset.
#' @param titre_2 A string representing the title for the final dataset.
#' @param time_dimension_list_groupped A list of data frames representing the grouped time dimensions.
#' @param fig.path A string representing the path to save the figures.
#'
#' @return A list containing the following elements:
#' \item{time_dimension_list_groupped_diff_image_knit}{A list of knitted plots for time dimension differences.}
#'
#' @examples
#' timecoverage(unique_analyse, parameter_time_dimension, titre_1, titre_2, time_dimension_list_groupped, fig.path)
timecoverage <- function(parameter_time_dimension, unique_analyse, titre_1 = "titre_1", titre_2 = "titre_2", time_dimension_list_groupped, fig.path = "figures") {
  # titles-time-cov chunk
  if (unique_analyse) {
    titles_time <- paste0("Evolutions of values for the dimension ", parameter_time_dimension, " for ", titre_1, " dataset ")
  } else {
    titles_time <- paste0("Evolutions of values for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " dataset ")
  }
  
  # timeanalysisplotcreation chunk
  time_dimension_list_groupped_diff <- lapply(time_dimension_list_groupped, function(x) {
    x %>%
      dplyr::mutate(Time = as.Date(Precision)) %>%
      dplyr::rename(`Values dataset 1` = "value_sum_1", `Values dataset 2` = "value_sum_2") %>%
      pivot_longer(cols = c(`Values dataset 1`, `Values dataset 2`), names_to = "Dataset", values_to = "Values") %>%
      dplyr::mutate(Dataset = dplyr::case_when(Dataset == "Values dataset 1" ~ titre_1, TRUE ~ titre_2)) %>%
      dplyr::distinct()
  })
  
  if (unique_analyse) {
    time_dimension_list_groupped_diff <- lapply(time_dimension_list_groupped_diff, function(x) {x %>% filter(Values != 0)})
  }
  
  time_dimension_list_groupped_diff_image <- lapply(time_dimension_list_groupped_diff, function(x) {
    ggplot(x) +
      aes(
        x = Time,
        y = Values,
        fill = Dataset,
        colour = Dataset,
        group = Dataset
      ) +
      geom_line(size = 0.5) +
      scale_fill_hue(direction = 1) +
      scale_color_hue(direction = 1) +
      theme_bw() +
      theme(legend.position = "top") +
      facet_wrap(vars(measurement_unit), nrow = 2L) +
      labs(x = unique(x$Dimension), y = "Values") +
      facet_grid("measurement_unit", scales = "free_y")
  })
  
  # temporalcoveragelistplot chunk
  time_dimension_list_groupped_diff_image_knit <- mapply(FUN = knitting_plots_subfigures,
                                                         plot = time_dimension_list_groupped_diff_image,
                                                         title = titles_time,
                                                         MoreArgs = list(folder = "Temporal", fig.pathinside = fig.path))
  
  # Return the images for display
  list(
    time_dimension_list_groupped_diff_image_knit = time_dimension_list_groupped_diff_image_knit
  )
}
