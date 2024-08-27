#' Calculate and Visualize Data Distribution for Other Dimensions
#'
#' This function calculates and visualizes the data distribution for various dimensions
#' using pie charts and bar charts.
#'
#' @param Other_dimensions A vector of dimensions to analyze.
#' @param init Initial dataset.
#' @param final Final dataset.
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset.
#' @param unique_analyse Logical indicating whether the analysis is unique.
#' @param fig.path Path to save the figures.
#'
#' @return A list containing the pie charts and bar charts for each dimension.
#' @examples
#' \dontrun{
#' other_dimension_analysis(c("Dimension1", "Dimension2"), init, final, "Dataset1", "Dataset2", FALSE, "path/to/save")
#' }
#' @export
#' @author
#' Bastien Grasset, \email{bastien.grasset@@ird.fr}
other_dimension_analysis <- function(Other_dimensions, init, final, titre_1, titre_2, unique_analyse = FALSE, fig.path,topn = 7) {
  library(dplyr)
  if(nrow(final) == 0){
    unique_analyse <- TRUE
  }
  if (!unique_analyse) {
    figures <- lapply(Other_dimensions, FUN = pie_chart_2_default, first = init, second = final, topn = topn, titre_1 = titre_1, titre_2 = titre_2)
  } else {
    figures <- lapply(Other_dimensions, FUN = pie_chart_2_default, first = init, topn = topn, title_yes_no = FALSE, titre_1 = titre_1)
  }
  
  dimension_title_subfigures <- gsub("_", ".", paste0("Distribution in value for the dimension: ", Other_dimensions))

  if (!unique_analyse) {
    # barplots <- lapply(Other_dimensions, FUN = bar_plot_default, first = init, second = final, topn = topn, titre_1 = titre_1, titre_2 = titre_2)
    barplots <- NULL #to fix
  } else {
    barplots <- NULL #to fix
    # barplots <- lapply(Other_dimensions, FUN = bar_plot_default, first = init, topn = topn, titre_1 = titre_1, titre_2 = NULL)
  }
  
  return(list(
    figures = figures,
    dimension_title_subfigures = dimension_title_subfigures,
    # pie_chart_knit = pie_chart_knit,
    barplots = barplots
    # bar_chart_knit = bar_chart_knit
  ))
}
