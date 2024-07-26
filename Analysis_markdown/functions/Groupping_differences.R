#' Groupping Differences
#'
#' This function groups differences between the initial and final datasets based on various dimensions.
#'
#' @param init Initial dataset.
#' @param final Final dataset.
#' @param parameter_time_dimension Vector of time dimensions.
#' @param parameter_geographical_dimension Vector of geographical dimensions.
#' @param parameter_geographical_dimension_groupping Vector of geographical dimension groupings.
#' @return A list containing grouped differences for all dimensions, geographical dimension groupings, and time dimensions.
#' @examples
#' # Example usage:
#' results <- groupping_differences(init, final, parameter_time_dimension, parameter_geographical_dimension, parameter_geographical_dimension_groupping)
#' @export
groupping_differences <- function(init, final, parameter_time_dimension, parameter_geographical_dimension, parameter_geographical_dimension_groupping) {
  units <- unique(c(unique(init$measurement_unit), unique(final$measurement_unit)))
  
  Other_dimensions <- colnames(init)[colnames(init) != "measurement_unit" & 
                                       colnames(init)!= "measurement_value" & 
                                       !colnames(init) %in% parameter_time_dimension &
                                       !colnames(init) %in% parameter_geographical_dimension & 
                                       !colnames(init) %in% parameter_geographical_dimension_groupping]
  
  Dimensions <- Other_dimensions
  
  carte_init <- fonction_groupement(Dimensions[[1]], init %>% head(1), final %>% head(1))
  
  Groupped_all <- carte_init[0,]
  
  for (i in Dimensions) {
    temporaire <- fonction_groupement(i, init, final)
    assign(paste0("Groupped", i), temporaire)
    
    Groupped_all <- rbind(Groupped_all, temporaire)
  }
  
  Groupped_all$Dimension <- as.character(Groupped_all$Dimension)
  Groupped_all$Precision <- as.character(Groupped_all$Precision)
  
  GrouppedGRIDTYPE <- fonction_groupement(parameter_geographical_dimension_groupping, init = init, final = final)
  
  time_dimension_list_groupped <- lapply(parameter_time_dimension, fonction_groupement, init = init, final = final)
  # Groupped_time_dimension <- do.call(rbind, time_dimension_list_groupped)
  
  return(list(Groupped_all = Groupped_all, GrouppedGRIDTYPE = GrouppedGRIDTYPE, Groupped_time_dimension = time_dimension_list_groupped, Other_dimensions = Other_dimensions))
}
