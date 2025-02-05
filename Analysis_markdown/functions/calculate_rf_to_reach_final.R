#' Calculate Raising Factors for Conversion to Nominal Data
#'
#' This function calculates the average conversion factors needed to match georeferenced data in numbers with nominal data in tons. 
#' It only considers strata where both measurement units (numbers and tons) are available and evaluates the coherence of conversion factors 
#' to achieve 100% of the nominal data for the same strata. If conversion factors are excessively high, the results are inconclusive, 
#' as nominal data should generally exceed georeferenced data; here, both conversion and raising occur. However, if the conversion factors 
#' are very low, an inconsistency between the datasets is likely.
#'
#' @param nominal_df A dataframe containing nominal data with columns for measurement units and measurement values.
#' @param lvl0_conv_df A dataframe containing georeferenced data with both "number" and "tons" units, from which conversion factors will be calculated.
#' @param strata_cols A character vector of column names used for stratifying the data (e.g., `species`, `gear_type`, `fishing_fleet`, `year`).
#'
#' @return A dataframe with calculated raising factors for each stratum where conversion is applicable. If conversion factors are too high,
#' they are marked as inconclusive. For very low conversion factors, potential inconsistencies are indicated.
#'
#' @details The function groups and aggregates data within each specified stratum. It calculates the conversion factors for each stratum 
#' by determining the difference between nominal data values and converted georeferenced data values in tons. Strata with both measurement 
#' units are selected for comparison.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' nominal_data <- data.frame(time_start = as.Date("2020-01-01"), measurement_value = 100, ...)
#' georef_data <- data.frame(time_start = as.Date("2020-01-01"), measurement_unit = "no", measurement_value = 200, ...)
#' strata_cols <- c("species", "gear_type", "fishing_fleet", "year", "geographic_identifier_nom)
#' calculate_rf_to_reach_final.R(nominal_df = nominal_data, lvl0_conv_df = georef_data, strata_cols = strata_cols)
#' }
#'
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @export
calculate_rf_to_reach_final <- function(df_to_reach, df_to_convert, strata_cols) {
  
  lvl0_conv_only_double_unit <-df_to_convert %>% 
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    dplyr::group_by(across(all_of(strata_cols))) %>%
    dplyr::mutate(numberunit = n_distinct(measurement_unit)) %>% dplyr::rowwise()%>% dplyr::filter(numberunit != 1 | (numberunit == 2 &  measurement_unit == "no"))
  
  # Regrouper les données du dataframe nominal selon les strates et calculer la somme des valeurs
  nominal_groupped <- df_to_reach %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    dplyr::group_by(across(all_of(strata_cols))) %>%
    dplyr::summarise(measurement_value = sum(measurement_value), .groups = 'drop')
  
  # Regrouper les données du dataframe lvl0_conv selon les strates et les unités de mesure, puis pivoter les données
  lvl0_conv_groupped <- lvl0_conv_only_double_unit %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    dplyr::group_by(across(all_of(strata_cols)), measurement_unit) %>%
    dplyr::summarise(measurement_value = sum(measurement_value), .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = measurement_unit, values_from = measurement_value)
  
  # Effectuer la jointure entre les deux jeux de données
  join_result <- dplyr::full_join(lvl0_conv_groupped, nominal_groupped, by = strata_cols) %>%
    dplyr::mutate(      t = dplyr::coalesce(t, 0), 
                        raising_factors = (measurement_value - t) / no) 
  
  return(join_result)
}