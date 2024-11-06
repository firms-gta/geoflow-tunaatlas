#' @title Geographic Differences
#' @description This function analyzes geographic data differences between two datasets.
#' @param init Initial dataset
#' @param final Final dataset
#' @param shapefile_fix Shapefile to be used
#' @param parameter_geographical_dimension Parameter for geographical dimension
#' @param parameter_geographical_dimension_groupping Parameter for geographical dimension grouping
#' @param continent Shapefile of the continent
#' @param plotting_type Type of plotting ("plot" or other)
#' @param titre_1 Title for the first dataset
#' @param titre_2 Title for the second dataset
#' @param outputonly Boolean to specify if output should be saved only
#' @return A list containing the geographic differences and a saved image
#' @export
#' @import dplyr
#' @import tmap
#' @importFrom tmap tm_shape tm_fill tm_facets tm_layout tm_borders
#' @importFrom ggplot2 ggsave
#' @importFrom sf st_as_sf
geographic_diff <- function(init, final, shapefile_fix, parameter_geographical_dimension, 
                            parameter_geographical_dimension_groupping, continent, plotting_type, 
                            titre_1, titre_2, outputonly) {
  
  geographic_dimension <- fonction_groupement(c(parameter_geographical_dimension, parameter_geographical_dimension_groupping), 
                                              init = init , final = final) %>% 
    dplyr::filter(value_sum_1 != 0 | value_sum_2 != 0)
  
  breaks <- dplyr::inner_join(shapefile_fix %>% dplyr::select(cwp_code, geom), geographic_dimension, by = c("cwp_code"="Precision")) %>%
    dplyr::mutate(`Impact on the data` = dplyr::case_when(`Difference (in %)` == Inf ~ "Appearing data",
                                                          Inf > `Difference (in %)`  & `Difference (in %)` >= 100 ~ "Gain (more than double)",
                                                          100 > `Difference (in %)`  & `Difference (in %)` > 0 ~ "Gain",
                                                          `Difference (in %)` == 0 ~ "No differences",
                                                          0 > `Difference (in %)`  & -100 < `Difference (in %)` ~ "Loss",
                                                          `Difference (in %)` == -100 ~  "All data lost"))
  
  breaks <- breaks %>% dplyr::ungroup()
  
  breaks$`Impact on the data` <- factor(breaks$`Impact on the data`, 
                                        levels = c("Appearing data", "Gain (more than double)", "Gain", 
                                                   "No differences", "Loss", "All data lost"))
  
  image <- tmap::tm_shape(breaks) +
    tmap::tm_fill("Impact on the data", palette = "-PiYG", id = "name") +
    tmap::tm_facets(by = c("measurement_unit", parameter_geographical_dimension_groupping), free.scales = FALSE, free.coords = FALSE) + # do not put free coords to true it creates bug
    tmap::tm_layout(legend.outside = TRUE)

  # if (plotting_type == "plot") {
    image <- image+tmap::tm_shape(continent) + tmap::tm_borders() 
    # image_to_save <- image
  # } else {
  #   # image_to_save <- tmap::tm_shape(continent) + tmap::tm_borders() + image
  # }
  title = paste0("Spatial differences between ", titre_1, " and ", titre_2, " dataset")
  breaks$geom <- NULL
  return(list(title = title, plott = image, tableforimage = breaks))
}
