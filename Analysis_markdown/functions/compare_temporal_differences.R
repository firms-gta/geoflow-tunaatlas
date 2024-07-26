#' Calculate and Visualize Temporal Data Differences
#'
#' This function calculates the differences in temporal data between two datasets
#' and provides visualizations of the differences in percent for each year.
#'
#' @param parameter_time_dimension A list of time dimensions to be analyzed.
#' @param init Data frame containing initial data.
#' @param final Data frame containing final data.
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset.
#' @param unique_analyse Logical value indicating whether the analysis is unique.
#' @param fig.path Path to save the figures.
#'
#' @return A list containing ggplot objects for visualizing the temporal differences.
#' @examples
#' \dontrun{
#' compare_temporal_differences(c("Year"), init, final, "Dataset1", "Dataset2", FALSE, "path/to/save")
#' }
#' @export
compare_temporal_differences <- function(parameter_time_dimension, init, final, titre_1, titre_2, unique_analyse = FALSE) {
  require(ggplot2)
  require(dplyr)
  require(tmap)
  Groupped_all_time <- data.frame()
  for (i in parameter_time_dimension) {
    temporaire <- fonction_groupement(i, init, final)
    assign(paste0("Groupped", i), temporaire)
    
    Groupped_all_time <- rbind(Groupped_all_time, temporaire)
  }
  
  timediffplot <- lapply(parameter_time_dimension, function(filtering_unit, dataframe) {
    ggplot(dataframe %>% dplyr::filter(Dimension == filtering_unit) %>% dplyr::mutate(Time = as.Date(Precision))) +
      aes(x = Time, y = `Difference (in %)`) +
      geom_line(size = 0.5) +
      scale_fill_hue(direction = 1) +
      theme(legend.position = "top") +
      facet_wrap(vars(measurement_unit), nrow = 2L) + theme_bw() +
      labs(x = filtering_unit) +
      facet_grid("measurement_unit", scales = "free_y")
  }, dataframe = Groupped_all_time)
  
  titles <- paste0("Difference in percent of value for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " dataset ")
  
  return(list(plots = timediffplot, titles = titles ))
}
