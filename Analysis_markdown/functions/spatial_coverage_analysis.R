#' Calculate and Visualize Spatial Coverage
#'
#' This function calculates the spatial coverage for different units and provides visualizations
#' of the values over spatial regions for each dataset.
#'
#' @param units A vector of spatial units to analyze.
#' @param init Initial dataset.
#' @param final Final dataset.
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset.
#' @param shapefile.fix Shapefile for fixing spatial data.
#' @param plotting_type Type of plotting to be used.
#' @param continent Data frame of continent shapes for reference.
#' @param print_map Logical indicating whether to print the map.
#' @param GrouppedGRIDTYPE Data frame containing grouped GRIDTYPE data.
#' @param fig.path Path to save the figures.
#'
#' @return A list containing the spatial coverage maps and related information.
#' @examples
#' \dontrun{
#' spatial_coverage_analysis(units, init, final, "Dataset1", "Dataset2", shapefile.fix, "plot", continent, TRUE, GrouppedGRIDTYPE, "path/to/save")
#' }
#' @export
#' @author
#' Bastien Grasset, \email{bastien.grasset@@ird.fr}
spatial_coverage_analysis <- function(init, final, titre_1 = "Dataset 1", titre_2 = "Dataset 2", shapefile.fix, plotting_type, continent,print_map = TRUE, GrouppedGRIDTYPE, savingimages) {
  library(dplyr)
  library(knitr)
  units <- unique(c(unique(init$measurement_unit), unique(final$measurement_unit)))
  map_unit <- lapply(units, FUN = fonction_empreinte_spatiale, initial_dataset = init, final_dataset = final, titre_1 = titre_1, titre_2 = titre_2, shapefile.fix = shapefile.fix, plotting_type = plotting_type, continent = continent)
  titles <- paste0("Distribution in value for the unit: ", units)
  
    number_of_GRIDTYPE <- length(unique(GrouppedGRIDTYPE$Precision))
    GRIDTYPE <- paste(as.list(unique(GrouppedGRIDTYPE$Precision)), collapse = " ; ")
  
  child_text <- sprintf('We represent spatial coverage, faceted by geographical category. The geographical category depends on the area of the geographic polygon. In this case there are %d categories which are %s.', 
            number_of_GRIDTYPE, paste(GRIDTYPE, collapse = ", "))
  return(list(
    child_text = child_text,
    plots = map_unit,
    titles = titles
  ))
}
