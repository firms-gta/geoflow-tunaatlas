#' Perform Strata Comparisons
#'
#' This function performs various comparisons between initial and final datasets to identify lost and gained strata.
#'
#' @param init A data frame representing the initial dataset.
#' @param final A data frame representing the final dataset.
#' @param Groupped_all A data frame containing grouped data.
#' @param titre_1 A string representing the title for the initial dataset.
#' @param titre_2 A string representing the title for the final dataset.
#' @param parameter_columns_to_keep A vector of column names to keep in the final comparison.
#' @param unique_analyse A boolean flag indicating whether to perform a unique analysis. Defaults to FALSE.
#'
#' @return A list containing the following elements:
#' \item{strates_perdues_first_10}{A data frame of the first 10 lost strata.}
#' \item{number_init_column_final_column}{A data frame comparing the number of unique values in columns between the initial and final datasets.}
#' \item{disapandap}{A data frame showing the differences in strata.}
#'
#' @examples
#' compnumberstratas(init, final, Groupped_all, titre_1, titre_2, parameter_columns_to_keep, unique_analyse = FALSE)
compnumberstratas <- function(init, final, Groupped_all, titre_1, titre_2, parameter_columns_to_keep, unique_analyse = FALSE) {
  # stratasloss chunk
  nb_ligne_init_millions <- nrow(init)
  nb_ligne_final_millions <- nrow(final)
  sum_valeur_init <- sum(init$measurement_value, na.rm = TRUE)
  sum_valeur_final <- sum(final$measurement_value, na.rm = TRUE)
  
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
    ungroup() %>%
    dplyr::filter(Dimension != "geographic_identifier") %>%
    dplyr::group_by(`Loss / Gain`, Dimension, measurement_unit) %>%
    dplyr::select(Dimension, everything())
  
  if (nrow(strates_perdues_first_10) == 0) {
    strates_perdues_first_10 <- rbind(strates_perdues, strates_gagnees) %>%
      dplyr::filter(`Loss / Gain` != 'Egal') %>%
      ungroup() %>%
      dplyr::filter(Dimension != "geographic_identifier") %>%
      dplyr::group_by(`Loss / Gain`, Dimension, measurement_unit) %>%
      dplyr::select(Dimension, everything())
  }
  
  strates_perdues_first_10[strates_perdues_first_10 == ""] <- "NA"
  strates_perdues_first_10[is.na(strates_perdues_first_10)] <- "NA"
  
  # compnumberstratas chunk
  number_init_column <- as.data.frame(t(init %>% dplyr::select(-measurement_value) %>% summarise_all(list(~n_distinct(.))))) %>%
    dplyr::rename(!!as.symbol(eval(parse(text = "titre_1"))) := V1)
  number_final_column <- as.data.frame(t(final %>% dplyr::select(-measurement_value) %>% summarise_all(list(~n_distinct(.))))) %>%
    dplyr::rename(!!as.symbol(eval(parse(text = "titre_2"))) := V1)
  
  number_final_column <- number_final_column[order(row.names(number_final_column)), , drop = FALSE]
  number_init_column <- number_init_column[order(row.names(number_init_column)), , drop = FALSE]
  
  number_init_column_final_column <- cbind(number_init_column, number_final_column) %>% mutate_all(as.numeric)
  number_init_column_final_column$Difference <- number_init_column_final_column[, 2] - number_init_column_final_column[, 1]
  
  rownames(number_init_column_final_column) <- paste0("Number of ", rownames(number_init_column_final_column))
  
  number_init_column_final_column <- as.data.frame(number_init_column_final_column %>% tibble::rownames_to_column(" "))
  
  # diffstratas chunk
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
  
  if (nrow(strates_perdues_first_10) != 0) {
    cat("The strata differences (completely lost or appearing) between the first one and the second one (representing ", round(pourcentage_strates_perdues), " % of the total number of strata) are :")
  } else {
    cat("No stratum is gained nor lost")
  }
  
  # Return the tables for qflextable2
  list(
    strates_perdues_first_10 = strates_perdues_first_10,
    number_init_column_final_column = number_init_column_final_column,
    disapandap = disapandap
  )
}
