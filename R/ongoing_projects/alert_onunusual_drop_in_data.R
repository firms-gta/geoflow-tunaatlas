library(dplyr)
library(lubridate)
library(tidyr)

detect_species_fleet_drop <- function(
    data,
    dataset_name = "dataset",
    species_col = "species",
    fleet_col = "fishing_fleet",
    date_col = "time_start",
    value_col = "measurement_value",
    unit_col = "measurement_unit",
    ton_unit = "t",
    reference_years = 3,
    drop_threshold = 0.10,
    min_reference_value = 1
) {
  
  df <- data %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[unit_col]] == ton_unit) %>%
    dplyr::mutate(year = lubridate::year(.data[[date_col]])) %>%
    dplyr::group_by(
      species = .data[[species_col]],
      fishing_fleet = .data[[fleet_col]],
      year
    ) %>%
    dplyr::summarise(
      tons = sum(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(species, fishing_fleet, year)
  
  # Compléter les années manquantes avec 0
  df_full <- df %>%
    dplyr::group_by(species, fishing_fleet) %>%
    tidyr::complete(
      year = seq(min(year, na.rm = TRUE), max(year, na.rm = TRUE), by = 1),
      fill = list(tons = 0)
    ) %>%
    dplyr::arrange(species, fishing_fleet, year) %>%
    dplyr::ungroup()
  
  # Référence = moyenne des n années précédentes
  out <- df_full %>%
    dplyr::group_by(species, fishing_fleet) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(
      ref_tons = sapply(
        seq_along(tons),
        function(i) {
          if (i <= reference_years) return(NA_real_)
          mean(tons[(i - reference_years):(i - 1)], na.rm = TRUE)
        }
      ),
      ratio_to_ref = dplyr::if_else(!is.na(ref_tons) & ref_tons > 0, tons / ref_tons, NA_real_),
      had_reference = !is.na(ref_tons) & ref_tons >= min_reference_value,
      disappeared = had_reference & tons == 0,
      dropped_90pct = had_reference & ratio_to_ref < drop_threshold,
      alert = disappeared | dropped_90pct,
      alert_type = dplyr::case_when(
        disappeared ~ "disappeared",
        dropped_90pct ~ "drop_gt_90pct",
        TRUE ~ NA_character_
      ),
      dataset = dataset_name
    ) %>%
    dplyr::ungroup()
  
  out
}
compare_species_fleet_drop <- function(
    data1,
    data2,
    name1 = "nominal",
    name2 = "georef",
    species_col = "species",
    fleet_col = "fishing_fleet",
    date_col = "time_start",
    value_col = "measurement_value",
    unit_col = "measurement_unit",
    ton_unit = "t",
    reference_years = 3,
    drop_threshold = 0.10,
    min_reference_value = 1
) {
  
  res1 <- detect_species_fleet_drop(
    data = data1,
    dataset_name = name1,
    species_col = species_col,
    fleet_col = fleet_col,
    date_col = date_col,
    value_col = value_col,
    unit_col = unit_col,
    ton_unit = ton_unit,
    reference_years = reference_years,
    drop_threshold = drop_threshold,
    min_reference_value = min_reference_value
  )
  
  res2 <- detect_species_fleet_drop(
    data = data2,
    dataset_name = name2,
    species_col = species_col,
    fleet_col = fleet_col,
    date_col = date_col,
    value_col = value_col,
    unit_col = unit_col,
    ton_unit = ton_unit,
    reference_years = reference_years,
    drop_threshold = drop_threshold,
    min_reference_value = min_reference_value
  )
  
  alerts1 <- res1 %>%
    dplyr::filter(alert) %>%
    dplyr::select(
      species, fishing_fleet, year,
      tons_1 = tons, ref_tons_1 = ref_tons, ratio_to_ref_1 = ratio_to_ref,
      alert_type_1 = alert_type
    )
  
  alerts2 <- res2 %>%
    dplyr::filter(alert) %>%
    dplyr::select(
      species, fishing_fleet, year,
      tons_2 = tons, ref_tons_2 = ref_tons, ratio_to_ref_2 = ratio_to_ref,
      alert_type_2 = alert_type
    )
  
  full_join(alerts1, alerts2, by = c("species", "fishing_fleet", "year")) %>%
    dplyr::mutate(
      case_when_compare = dplyr::case_when(
        !is.na(alert_type_1) & !is.na(alert_type_2) ~ "alert_in_both",
        !is.na(alert_type_1) &  is.na(alert_type_2) ~ paste0("alert_only_in_", name1),
        is.na(alert_type_1) & !is.na(alert_type_2) ~ paste0("alert_only_in_", name2),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::arrange(species, fishing_fleet, year)
}

drop_compare <- compare_species_fleet_drop(
  data1 = georef %>% dplyr::filter(source_authority == "IOTC"),
  data2 = nominal_catch%>% dplyr::filter(source_authority == "IOTC"),
  name1 = "nominal",
  name2 = "georef",
  species_col = "species",
  fleet_col = "fishing_fleet",
  date_col = "time_start",
  value_col = "measurement_value",
  unit_col = "measurement_unit",
  ton_unit = "t",
  reference_years = 3,
  drop_threshold = 0.10,
  min_reference_value = 1
)

drop_compare

drop_compare <- compare_species_fleet_drop(
  data1 = georef %>% dplyr::filter(source_authority == "ICCAT"),
  data2 = iccat %>% dplyr::filter(source_authority == "ICCAT"),
  name1 = "nominal",
  name2 = "georef",
  species_col = "species",
  fleet_col = "fishing_fleet",
  date_col = "time_start",
  value_col = "measurement_value",
  unit_col = "measurement_unit",
  ton_unit = "t",
  reference_years = 3,
  drop_threshold = 0.10,
  min_reference_value = 1
)
View(drop_compare)


summarise_drop_events <- function(drop_table) {
  drop_table %>%
    dplyr::group_by(species, fishing_fleet, case_when_compare) %>%
    dplyr::summarise(
      first_year = min(year, na.rm = TRUE),
      last_year = max(year, na.rm = TRUE),
      n_years_flagged = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(species, fishing_fleet, first_year)
}
summarise_drop_events(drop_compare)




library(dplyr)
library(lubridate)
library(tidyr)

flag_drop_90pct <- function(
    data,
    group_cols = c("species","fishing_fleet"),
    date_col = "time_start",
    value_col = "measurement_value",
    unit_col = "measurement_unit",
    ton_unit = "t",
    reference_years = 3,
    drop_threshold = 0.10,
    min_reference_value = 1
) {
  
  df <- data %>%
    dplyr::filter(.data[[unit_col]] == ton_unit) %>%
    dplyr::mutate(year = lubridate::year(.data[[date_col]])) %>%
    dplyr::group_by(dplyr::across(all_of(c(group_cols,"year")))) %>%
    dplyr::summarise(
      tons = sum(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::across(all_of(group_cols)), year)
  
  # compléter années manquantes
  df <- df %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    tidyr::complete(
      year = seq(min(year), max(year), by = 1),
      fill = list(tons = 0)
    ) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::ungroup()
  
  df %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(
      
      ref_tons = sapply(
        seq_along(tons),
        function(i) {
          if (i <= reference_years) return(NA_real_)
          mean(tons[(i-reference_years):(i-1)], na.rm = TRUE)
        }
      ),
      
      ratio_to_ref = tons / ref_tons,
      
      drop_90pct =
        !is.na(ref_tons) &
        ref_tons >= min_reference_value &
        ratio_to_ref < drop_threshold,
      
      disappearance =
        !is.na(ref_tons) &
        ref_tons >= min_reference_value &
        tons == 0,
      
      alert = drop_90pct | disappearance,
      
      alert_type = dplyr::case_when(
        disappearance ~ "disappearance",
        drop_90pct ~ "drop_gt_90pct",
        TRUE ~ NA_character_
      )
      
    ) %>%
    dplyr::ungroup()
}


drop_flags <- flag_drop_90pct(iotc)
View(drop_flags %>%
  dplyr::filter(alert))


drop_flags <- flag_drop_90pct(iccat)
View(drop_flags %>%
       dplyr::filter(alert))



drop_flags <- flag_drop_90pct(iccat, group_cols = c("species"))
View(drop_flags %>%
       dplyr::filter(alert))
