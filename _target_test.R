library(targets)
library(dplyr)
library(qs)
library(purrr)
library(stringr)
library(lubridate)
library(glue)

tar_option_set(format = "rds")

# Fonction pour traiter une année
process_one_year <- function(df) {
  Sys.sleep(0.5)  # Ajoute un délai de 0.1 seconde
  df %>% dplyr::mutate(processedtest = TRUE)
}

write_year_if_changed <- function(df, year, out_dir = "data_cache") {
  final <- glue::glue("{out_dir}/data_{year}.qs")
  
  if (file.exists(final)) {
    existing <- qs::qread(final)
    
    df_sorted <- dplyr::arrange_all(strip_attrs(df))
    existing_sorted <- dplyr::arrange_all(strip_attrs(existing))
    
    if (identical(existing_sorted, df_sorted)) {
      message(glue::glue("✅ Année {year} inchangée, fichier conservé."))
      return(FALSE)
    } else {
      message(glue::glue("⚠️ Année {year} changée, différences :"))
      try({
        diffs <- waldo::compare(existing_sorted, df_sorted, max_diffs = 5)
        print(diffs)
      }, silent = TRUE)
    }
  }
  
  tmp <- tempfile(fileext = ".qs")
  qs::qsave(df, tmp)
  file.copy(tmp, final, overwrite = TRUE)
  unlink(tmp)
  message(glue::glue("📝 Année {year} modifiée → fichier mis à jour."))
  TRUE
}

strip_attrs <- function(x) {
  x <- dplyr::as_tibble(x)
  attr(x, "spec") <- NULL
  attr(x, "problems") <- NULL
  class(x) <- setdiff(class(x), "spec_tbl_df")
  x
} # permet davoir un waldo compare plus uniforme  




list(
  tar_target(
    input_qs,
    "data/target_test_new.qs",
    format = "file"
  )
  ,
  # 1. Lecture du fichier brut et découpe en fichiers annuels
  tar_target(
    split_and_save_years,
    {
      raw <- qs::qread(input_qs)
      raw <- dplyr::filter(raw, !is.na(time_start)) %>%
        dplyr::mutate(year = lubridate::year(time_start))
      
      years_present <- unique(raw$year)
      
      files_written <- purrr::imap_lgl(
        split(raw, raw$year),
        ~ write_year_if_changed(.x, .y)
      )
      
      n_written <- sum(files_written)
      message(glue::glue("📦 {n_written} fichiers modifiés sur {length(files_written)} années."))
      
      # 👉 On retourne uniquement les fichiers correspondant aux années présentes
      filenames <- glue::glue("data_cache/data_{years_present}.qs")
      filenames[file.exists(filenames)]
    }
  )
,  
  # 2. Une cible par fichier (pattern)
  tar_target(
    year_file,
    split_and_save_years,
    pattern = map(split_and_save_years),
    format = "file"
  ),
  
  # 3. Traitement unitaire
  tar_target(
    data_by_year,
    process_one_year(qs::qread(year_file)),
    pattern = map(year_file),
    iteration = "list"
  ),
  
  # 4. Fusion
  tar_target(
    all_data_processed,
    dplyr::bind_rows(data_by_year)
  )
)



# 
# simulate_new_dataset <- function(old_data) {
#   library(dplyr)
#   library(lubridate)
# 
#   # Copier données 1952 et modifier légèrement une valeur
#   updated_1952 <- old_data %>%
#     dplyr::filter(year(time_start) == 1952) %>%
#     dplyr::mutate(measurement_value = if_else(row_number() == 1, measurement_value * 1.1, measurement_value))
# 
#   # Créer de fausses données pour 2025
#   new_2025 <- updated_1952 %>%
#     slice(1:3) %>%
#     dplyr::mutate(
#       time_start = as.Date("2025-01-01"),
#       time_end   = as.Date("2025-01-31"))
# 
#   bind_rows(updated_1952, new_2025, old_data)
# }
# 
# # Exemple d'utilisation :
# qs::qsave(
#   simulate_new_dataset(effort_ll_iotc_level0),
#   "data/target_test_new.qs"
# )
