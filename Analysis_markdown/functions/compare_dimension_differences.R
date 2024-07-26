#' Calculate and Visualize Differences for Each Dimension
#'
#' This function calculates the differences in various dimensions between two datasets
#' and provides a detailed summary of the differences.
#'
#' @param Groupped_all Data frame containing grouped strata data.
#' @param Other_dimensions Vector of dimensions to be analyzed.
#' @param parameter_diff_value_or_percent Character string indicating the parameter to sort by ("Difference in value" or "Difference (in %)").
#' @param parameter_columns_to_keep Vector of column names to keep in the final output.
#' @param topn Integer indicating the number of top differences to display.
#' @param outputonly Logical value indicating whether to output only the image.
#'
#' @return A data frame containing the summarized differences for each dimension.
#' @examples
#' \dontrun{
#' compare_dimension_differences(Groupped_all, c("Dimension1", "Dimension2"), "Difference in value", c("Column1", "Column2"), 6, FALSE)
#' }
#' @export
#' @author
#' Bastien Grasset, \email{bastien.grasset@@ird.fr}
compare_dimension_differences <- function(Groupped_all, Other_dimensions, parameter_diff_value_or_percent, parameter_columns_to_keep = c("Precision", "measurement_unit", "Values dataset 1",
                                                                                                                                         "Values dataset 2", "Loss / Gain",
                                                                                                                                         "Difference (in %)", "Dimension",
                                                                                                                                         "Difference in value"), topn = 6, outputonly = FALSE) {
  
  Groupped_all_not_disap_or_app <- Groupped_all %>% 
    dplyr::filter(`Difference (in %)` %notin% c(0, 100, -Inf) & (value_sum_1 != 0) & value_sum_2 != 0) %>%
    dplyr::filter(Dimension %in% Other_dimensions)
  
  if (nrow(Groupped_all_not_disap_or_app) != 0) {
    Groupped_all_not_disap_or_app <- Groupped_all_not_disap_or_app %>% 
      dplyr::mutate(`Loss / Gain` = ifelse(loss >= 0, "Loss", "Gain")) %>%
      dplyr::mutate(`Loss / Gain` = dplyr::case_when(is.na(`Loss / Gain`) ~ "Gain", value_sum_1 == value_sum_2 ~ "Egal", TRUE ~ `Loss / Gain`)) %>%
      dplyr::mutate(Dimension = as.factor(Dimension)) %>%
      dplyr::group_by(Dimension, measurement_unit, `Loss / Gain`) %>%
      arrange(desc(abs(.data[[parameter_diff_value_or_percent]]))) %>%
      dplyr::mutate(id = row_number()) %>%
      dplyr::mutate(Precision = as.factor(ifelse(id > topn, "Others", as.character(Precision)))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(`Loss / Gain`, Dimension, Precision, measurement_unit) %>%
      dplyr::summarise(across(is.numeric, sum)) %>%
      dplyr::mutate(`Difference (in %)` = (`Difference in value` / value_sum_1) * 100) %>%
      dplyr::select(-id, -number_lines2) %>%
      dplyr::group_by(`Loss / Gain`, Dimension, measurement_unit) %>%
      arrange(Dimension, measurement_unit, desc(abs(.data[[parameter_diff_value_or_percent]]))) %>%
      mutate(Other_column = ifelse(Precision == "Others", 1, 0))
    
    Groupped_all_not_disap_or_app_to_dysplay <- Groupped_all_not_disap_or_app %>% 
      dplyr::ungroup() %>%
      dplyr::rename(`Values dataset 1` = "value_sum_1", `Values dataset 2` = "value_sum_2") %>%
      dplyr::filter(`Loss / Gain` != "Egal") %>%
      dplyr::select(parameter_columns_to_keep, Other_column) %>%
      dplyr::mutate(across(c("Values dataset 1", "Values dataset 2", "Difference in value"), ~ round(., digits = 0))) %>%
      dplyr::mutate_if(is.numeric, round, digits = 2) %>%
      dplyr::group_by(Dimension, measurement_unit, `Loss / Gain`) %>%
      arrange(Dimension, measurement_unit, `Loss / Gain`, Other_column, desc(abs(.data[[parameter_diff_value_or_percent]]))) %>%
      dplyr::select(-Other_column)
  } else {
    Groupped_all_not_disap_or_app_to_dysplay <- data.frame()
  }
  
  return(list(Groupped_all_not_disap_or_app_to_dysplay = Groupped_all_not_disap_or_app_to_dysplay , topn = topn))
}
