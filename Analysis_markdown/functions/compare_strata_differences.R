#' Calculate and Compare Strata Differences
#'
#' This function calculates the differences in strata between two datasets and 
#' provides various summaries and visualizations of the results.
#'
#' @param init Data frame containing initial geographical data.
#' @param final Data frame containing final geographical data.
#' @param Groupped_all Data frame containing grouped strata data.
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset.
#' @param parameter_columns_to_keep Vector of column names to keep in the final output.
#' @param unique_analyse Logical value indicating whether the analysis is unique.
#'
#' @return A list containing summaries and visualizations of the strata differences.
#' @examples
#' \dontrun{
#' compare_strata_differences(init, final, Groupped_all, "Dataset1", "Dataset2", c("Column1", "Column2"), FALSE)
#' }
#' @export
compare_strata_differences <- function(init, final, Groupped_all, titre_1 = "Dataset1", titre_2 = "Dataset2", parameter_columns_to_keep, unique_analyse = FALSE) {
  Groupped_all <- as.data.frame(Groupped_all)
  Groupped_all$Dimension <- as.character(Groupped_all$Dimension)
  Groupped_all$Precision <- as.character(Groupped_all$Precision)
  
  strates_perdues <- Groupped_all %>% dplyr::filter(value_sum_2 == 0 & `Loss / Gain` == "Loss")
  strates_gagnees <- Groupped_all %>% dplyr::filter(value_sum_1 == 0 & `Loss / Gain` == "Gain")
  nombre_strates_perdues <- nrow(strates_perdues)
  nombre_strates_gagnees <- nrow(strates_gagnees)
  nombres_de_strates_totales <- nrow(Groupped_all)
  pourcentage_strates_perdues <- 100 - (100 * ((nombres_de_strates_totales - nombre_strates_perdues) / nombres_de_strates_totales))
  
  strates_perdues_first_10 <- rbind(strates_perdues, strates_gagnees) %>% 
    dplyr::filter(`Loss / Gain` != 'Egal') %>%
    ungroup %>% 
    dplyr::filter(Dimension != "geographic_identifier") %>% 
    dplyr::group_by(`Loss / Gain`, Dimension, measurement_unit) %>% 
    dplyr::select(Dimension, everything())
  
  if (nrow(strates_perdues_first_10) == 0) {
    strates_perdues_first_10 <- rbind(strates_perdues, strates_gagnees) %>% 
      dplyr::filter(`Loss / Gain` != 'Egal') %>%
      ungroup %>% 
      dplyr::filter(Dimension != "geographic_identifier") %>% 
      dplyr::group_by(`Loss / Gain`, Dimension, measurement_unit) %>% 
      dplyr::select(Dimension, everything())
  }
  strates_perdues_first_10 <- as.data.frame(strates_perdues_first_10)
  strates_perdues_first_10[strates_perdues_first_10 == ""] <- "NA"
  strates_perdues_first_10[is.na(strates_perdues_first_10)] <- "NA"
  
  number_init_column <- as.data.frame(t(init %>% dplyr::select(-measurement_value) %>% summarise_all(list(~n_distinct(.))))) %>%  
    dplyr::rename(!!titre_1 := V1)
  number_final_column <- as.data.frame(t(final %>% dplyr::select(-measurement_value) %>% summarise_all(list(~n_distinct(.))))) %>%  
    dplyr::rename(!!titre_2 := V1)
  
  number_final_column <- number_final_column[order(row.names(number_final_column)), , drop = FALSE]
  number_init_column <- number_init_column[order(row.names(number_init_column)), , drop = FALSE]
  
  number_init_column_final_column <- cbind(number_init_column, number_final_column) %>% mutate_all(as.numeric)
  number_init_column_final_column$Difference <- number_init_column_final_column[, 2] - number_init_column_final_column[, 1]
  
  rownames(number_init_column_final_column) <- paste0("Number of ", rownames(number_init_column_final_column))
  
  number_init_column_final_column <- as.data.frame(number_init_column_final_column %>% tibble::rownames_to_column(" "))
  
  disapandap <- NULL
  if (nrow(strates_perdues_first_10) != 0) {
    disapandap <- strates_perdues_first_10 %>%
      dplyr::mutate(`Loss / Gain` = dplyr::case_when(
        loss > 1 ~ "Loss",
        abs(loss) <= 1 ~ "No differences",
        loss < 1 ~ "Gain",
        TRUE ~ NA_character_
      )) %>%
      dplyr::mutate(`Loss / Gain` = dplyr::case_when(
        is.na(`Loss / Gain`) ~ "Gain",
        value_sum_1 == value_sum_2 ~ "Egal",
        TRUE ~ `Loss / Gain`
      )) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        `Values dataset 1` = "value_sum_1",
        `Values dataset 2` = "value_sum_2"
      ) %>%
      dplyr::select(parameter_columns_to_keep) %>%
      dplyr::group_by(Dimension, measurement_unit, `Loss / Gain`) %>%
      dplyr::arrange(Dimension, measurement_unit, `Loss / Gain`, desc(`Difference in value`)) %>%
      dplyr::ungroup() %>%  
      dplyr::select(Dimension, measurement_unit, `Loss / Gain`, Precision, `Difference in millions` = `Difference in value`)
  }
  
  return(list(
    strates_perdues_first_10 = strates_perdues_first_10,
    number_init_column_final_column = number_init_column_final_column,
    disapandap = disapandap,
    pourcentage_strates_perdues = pourcentage_strates_perdues
  ))
}
