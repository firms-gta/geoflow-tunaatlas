#' Perform Geographic Differences Analysis
#'
#' This function analyzes geographic differences between initial and final datasets and generates a map of the differences.
#'
#' @param parameter_geographical_dimension A string representing the geographical dimension.
#' @param parameter_geographical_dimension_groupping A string representing the geographical dimension grouping.
#' @param init A data frame representing the initial dataset.
#' @param final A data frame representing the final dataset.
#' @param shapefile.fix A spatial object representing the shapefile.
#' @param continent A spatial object representing the continent boundaries.
#' @param titre_1 A string representing the title for the initial dataset.
#' @param titre_2 A string representing the title for the final dataset.
#' @param plotting_type A string representing the type of plotting ("plot" or "other"). Defaults to "plot".
#' @param outputonly A boolean flag indicating whether to output only. Defaults to FALSE.
#'
#' @return A list containing the following elements:
#' \item{image}{A tmap object representing the map of geographic differences.}
#'
#' @examples
#' geographicdiff(parameter_geographical_dimension, parameter_geographical_dimension_groupping, init, final, shapefile.fix, continent, titre_1, titre_2)
geographicdiff <- function(parameter_geographical_dimension, parameter_geographical_dimension_groupping, init, final, shapefile.fix, continent, titre_1, titre_2, plotting_type = "plot", outputonly = FALSE) {
  # breaks-for-map chunk
  geographic_dimension <- fonction_groupement(list(parameter_geographical_dimension, parameter_geographical_dimension_groupping), init = init, final = final) %>%
    dplyr::filter(value_sum_1 != 0 | value_sum_2 != 0)
  
  breaks <- dplyr::inner_join(shapefile.fix %>% dplyr::select(-GRIDTYPE), geographic_dimension, by = c("cwp_code" = "Precision")) %>%
    dplyr::mutate(`Impact on the data` = dplyr::case_when(
      `Difference (in %)` == Inf ~ "Appearing data",
      Inf > `Difference (in %)` & `Difference (in %)` >= 100 ~ "Gain (more than double)",
      100 > `Difference (in %)` & `Difference (in %)` > 0 ~ "Gain",
      `Difference (in %)` == 0 ~ "No differences",
      0 > `Difference (in %)` & -100 < `Difference (in %)` ~ "Loss",
      `Difference (in %)` == -100 ~ "All data lost"
    ))
  
  breaks <- breaks %>% ungroup()
  
  breaks$`Impact on the data` <- factor(breaks$`Impact on the data`, levels = c("Appearing data", "Gain (more than double)", "Gain", "No differences", "Loss", "All data lost"))
  
  image <- tm_shape(breaks) + tm_fill("Impact on the data", palette = "-PiYG", id = "name") + tm_facets(by = c("measurement_unit", "GRIDTYPE"), free.scales = FALSE, free.coords = TRUE) + tm_layout(legend.outside = TRUE)
  
  if (plotting_type == "plot") {
    image <- image + tm_shape(continent) + tm_borders()
  }
  
  # spatialdifferencessaving chunk
  save_image(title = paste0("Spatial differences between ", titre_1, " and ", titre_2, " dataset"), plott = image, folder = "Spatialdiffmap", find_and_print = outputonly)
  
  # Return the image object for display
  list(image = image)
}
