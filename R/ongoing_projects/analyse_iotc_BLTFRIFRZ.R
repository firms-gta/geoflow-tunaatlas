
# N join ------------------------------------------------------------------


final_nom <- iotc_nom %>% dplyr::ungroup() %>% dplyr::select(species, fishing_fleet, time_start) %>% dplyr::distinct() %>% dplyr::filter(species %in%c("FRI", "BLT", "FRZ"))%>% 
  dplyr::mutate(data_nom = "t")
final_geom <- iotc %>% dplyr::ungroup() %>% dplyr::select(species, fishing_fleet, time_start) %>% dplyr::distinct() %>% dplyr::filter(species %in%c("FRI", "BLT", "FRZ")) %>% 
  dplyr::mutate(data_geom = "t")
missing <- full_join(final_geom, final_nom)
View(missing %>% dplyr::filter(is.na(data_geom) | is.na(data_nom)))


library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

# =========================================================
# Helper: préparer les strates distinctes
# =========================================================
prepare_species_strata <- function(data,
                                   dataset_name,
                                   grouping_keys = c("year", "fishing_fleet", "gear_type"),
                                   date_col = "time_start",
                                   species_col = "species") {
  
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = lubridate::year(.data[[date_col]]))
  
  missing_cols <- setdiff(c(grouping_keys, species_col), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  data %>%
    dplyr::select(dplyr::all_of(c(grouping_keys, species_col))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(dataset = dataset_name)
}

# =========================================================
# Fonction principale
# =========================================================
# group_species = espèces à considérer comme équivalentes
# group_label   = nom du regroupement
#
# Idée :
# 1. on compare les strates distinctes espèce par espèce
# 2. on récupère les lignes non appariées
# 3. pour chaque strate (année/pavillon/engin), on regarde
#    si le mismatch disparaît quand on regroupe
# 4. on ajoute un warning si cas partiel
# =========================================================
inspect_species_group_mismatches <- function(nominal_data,
                                             georef_data,
                                             group_species = c("FRI", "BLT", "FRZ"),
                                             group_label = "FRZ_group",
                                             grouping_keys = c("year", "fishing_fleet", "gear_type"),
                                             date_col_nom = "time_start",
                                             date_col_geo = "time_start",
                                             species_col = "species") {
  
  # -------------------------
  # Préparation
  # -------------------------
  nom <- prepare_species_strata(
    data = nominal_data,
    dataset_name = "nominal",
    grouping_keys = grouping_keys,
    date_col = date_col_nom,
    species_col = species_col
  ) %>%
    dplyr::filter(.data[[species_col]] %in% group_species)
  
  geo <- prepare_species_strata(
    data = georef_data,
    dataset_name = "georef",
    grouping_keys = grouping_keys,
    date_col = date_col_geo,
    species_col = species_col
  ) %>%
    dplyr::filter(.data[[species_col]] %in% group_species)
  
  join_keys <- c(grouping_keys, species_col)
  
  # -------------------------
  # Comparaison brute espèce par espèce
  # -------------------------
  raw_compare <- dplyr::full_join(
    geo %>% dplyr::mutate(in_georef = TRUE),
    nom %>% dplyr::mutate(in_nominal = TRUE),
    by = join_keys
  ) %>%
    dplyr::mutate(
      in_georef = dplyr::coalesce(in_georef, FALSE),
      in_nominal = dplyr::coalesce(in_nominal, FALSE),
      match_status = dplyr::case_when(
        in_georef & in_nominal ~ "common",
        in_georef & !in_nominal ~ "only_georef",
        !in_georef & in_nominal ~ "only_nominal"
      )
    )
  
  # -------------------------
  # On garde seulement les strates où il y a un mismatch
  # -------------------------
  mismatched_strata <- raw_compare %>%
    dplyr::filter(match_status != "common") %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(grouping_keys)))
  
  # Si aucun mismatch
  if (nrow(mismatched_strata) == 0) {
    return(list(
      raw_compare = raw_compare,
      mismatched_species = raw_compare %>% dplyr::filter(match_status != "common"),
      grouped_diagnosis = tibble(),
      warnings = tibble()
    ))
  }
  
  # -------------------------
  # Diagnostic regroupé au niveau strate
  # -------------------------
  grouped_diagnosis <- mismatched_strata %>%
    dplyr::left_join(
      geo %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_keys))) %>%
        dplyr::summarise(
          species_georef = list(sort(unique(.data[[species_col]]))),
          n_georef_species = dplyr::n_distinct(.data[[species_col]]),
          .groups = "drop"
        ),
      by = grouping_keys
    ) %>%
    dplyr::left_join(
      nom %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_keys))) %>%
        dplyr::summarise(
          species_nominal = list(sort(unique(.data[[species_col]]))),
          n_nominal_species = dplyr::n_distinct(.data[[species_col]]),
          .groups = "drop"
        ),
      by = grouping_keys
    ) %>%
    dplyr::mutate(
      species_georef = purrr::map(species_georef, ~ if (is.null(.x)) character(0) else .x),
      species_nominal = purrr::map(species_nominal, ~ if (is.null(.x)) character(0) else .x),
      
      common_species = purrr::map2(species_georef, species_nominal, intersect),
      only_georef_species = purrr::map2(species_georef, species_nominal, setdiff),
      only_nominal_species = purrr::map2(species_nominal, species_georef, setdiff),
      
      n_common_species = purrr::map_int(common_species, length),
      n_only_georef_species = purrr::map_int(only_georef_species, length),
      n_only_nominal_species = purrr::map_int(only_nominal_species, length),
      
      # vrai si les deux datasets ont au moins une espèce du groupe
      group_present_in_both =
        purrr::map_int(species_georef, length) > 0 &
        purrr::map_int(species_nominal, length) > 0,
      
      # vrai si mismatch au niveau espèce, mais présence des deux côtés
      regrouping_possible =
        group_present_in_both &
        (n_only_georef_species > 0 | n_only_nominal_species > 0),
      
      # warning si cas partiel :
      # une partie matche déjà, une autre non
      partial_warning =
        n_common_species > 0 &
        (n_only_georef_species > 0 | n_only_nominal_species > 0),
      
      # warning textuel
      warning_message = dplyr::case_when(
        partial_warning ~ purrr::pmap_chr(
          list(common_species, only_georef_species, only_nominal_species),
          function(common_species, only_georef_species, only_nominal_species) {
            paste0(
              "Partial match inside ", group_label, ": common = {",
              paste(common_species, collapse = ", "),
              "}; only_georef = {",
              paste(only_georef_species, collapse = ", "),
              "}; only_nominal = {",
              paste(only_nominal_species, collapse = ", "),
              "}."
            )
          }
        ),
        regrouping_possible ~ "Mismatch may be resolved by grouping species.",
        TRUE ~ NA_character_
      )
    )
  
  warnings_tbl <- grouped_diagnosis %>%
    dplyr::filter(!is.na(warning_message))
  
  list(
    raw_compare = raw_compare,
    mismatched_species = raw_compare %>% dplyr::filter(match_status != "common"),
    grouped_diagnosis = grouped_diagnosis,
    warnings = warnings_tbl
  )
}


res_frz <- inspect_species_group_mismatches(
  nominal_data = iotc_nom,
  georef_data = iotc,
  group_species = c("FRI", "BLT", "FRZ"),
  group_label = "FRI_BLT_FRZ",
  grouping_keys = c("year", "fishing_fleet", "gear_type"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)

res_frz$mismatched_species

View(res_frz$grouped_diagnosis %>%
       dplyr::filter(regrouping_possible))
View(res_frz$warnings)




# ICCAT FRZ ---------------------------------------------------------------

iccat_nom <- nominal_catch_for_raising%>% dplyr::filter(source_authority == "ICCAT")
iccat<- georef_dataset%>% dplyr::filter(source_authority == "ICCAT")

res_frz <- inspect_species_group_mismatches(
  nominal_data = iccat_nom,
  georef_data = iccat,
  group_species = c("FRI", "BLT", "FRZ"),
  group_label = "FRI_BLT_FRZ",
  grouping_keys = c("year", "fishing_fleet", "gear_type"),
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)

# Pour ICCAT, 
# FRI manque données USA nom en 1970, ça doit être NEI, c'est le cas pour plein d'années, il vaut mieux le mettre en NEI ou il y a recoupement mais 
# ça empêche pas que georef est plus précis que nominaL. 
## GG c'est que du 99.9


res_frz <- inspect_species_group_mismatches(
  nominal_data = iccat_nom,
  georef_data = iccat,
  group_species = c("FRI", "BLT", "FRZ"),
  group_label = "FRI_BLT_FRZ",
  grouping_keys = c("year"), # si on fait fait avec FF et GG, pb parce que trop de georef sans aucun nom
  date_col_nom = "time_start",
  date_col_geo = "time_start",
  species_col = "species"
)

# Sinon avant 67, que données nom pas grave, mais arpès que FRI en georef alors que nom BLT + FRI. Est-ce que les georef n'ont pas de BLT ou est-ce que c'est aggrégé en FRI et pas FRZ dans georef.
# Pb 2022, 2023, pas de BLT ni FRI en nom.



# BIL ---------------------------------------------------------------------


