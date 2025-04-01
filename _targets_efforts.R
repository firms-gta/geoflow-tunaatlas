# Chargement des packages nécessaires
library(targets)
library(here)
library(futile.logger)
library(geoflow)

if (!file.exists(here::here("results_efforts_2025"))) {
  dir.create(here::here("results_efforts_2025"))}
# Restaurer l'environnement `renv`
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

# Définir les options globales de `{targets}`
tar_option_set(
  packages = c(
    "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor", 
    "dotenv", "data.table", "here", "xfun", "RPostgreSQL", "RPostgres", "DBI", 
    "rpostgis", "terra", "sf", "RSQLite", "webshot", "usethis", "ows4R", "sp", 
    "flextable", "dplyr", "stringr", "tibble", "bookdown", "knitr", 
    "purrr", "readxl", "odbc", "rlang", "kableExtra", "tidyr", "ggplot2", 
    "stats", "RColorBrewer", "cowplot", "tmap", "curl", "officer", 
    "gdata", "R3port", "reshape2", "tools", "plogr", "futile.logger", "lubridate", "data.table"
  )
)
options(
  encoding = "UTF-8",
  stringsAsFactors = FALSE, 
  dplyr.summarise.inform = FALSE
)

Sys.setlocale("LC_ALL", "en_US.UTF-8")
# Définition du pipeline
list(
  tar_target(
    config,
    initWorkflow(here::here("create_effort_dataset.json"))  # 🔥 recréer `config` depuis le fichier
  ),
  
  tar_target(
    entity,
    {
      entityfile <- config$metadata$content$entities[[1]]
      entityfile$relations <- NULL # Pour éviter l'échec lors du chargement de la codelist depuis Google Drive si non connecté
      entityfile
    }
  ),
  tar_target(
    output_file,
    {
    path <- executeWorkflow(here::here("create_effort_dataset.json"))  # 🔥 recréer `config` depuis le fichier
      entity_tar <- entity
      output_file <- file.path(path,"entities",entity_tar$identifiers[["id"]], "data", paste0(entity_tar$identifiers[["id"]], "_harmonized.csv"))
      
      if (file.exists(output_file)) {
        flog.info("Fichier généré avec succès : %s", output_file)
        output_file
      } else {
        flog.error("Le fichier attendu n'a pas été généré : %s", output_file)
        stop("Erreur : fichier non généré")
      }
    },
    format = "file"
  ),
  # 🔹 4. Lire et sauvegarder le fichier généré
  tar_target(
    save_results,
    {
      df <- read.csv(output_file)  # Lire le fichier généré
      
      write.csv(df, here::here("results_efforts_2025/tuna_atlas_results.csv"))  # Sauvegarde finale
      "results_efforts_2025/tuna_atlas_results.csv"
    },
    format = "file"
  )
)
# # Définition du pipeline
# list(
#   # 🔹 1. Charger la configuration JSON
#   tar_target(
#     config,
#     initWorkflow(here::here("catch_ird_level2_local.json")),  # 🔥 Chargement direct de l’objet R6
#     format = "rds"  # 💡 On utilise "rds" pour éviter que `{targets}` ne modifie l'objet
#   ),
#   tar_target(
#     entity,
#     config$metadata$content$entities,  # 🔥 On prend directement la liste d'entités
#     pattern = map(config)
#   ),tar_target(
#     actions,
#     entity$data$actions[[1]],  # 🔥 Toujours la première action
#     pattern = map(entity)
#   ),
#   tar_target(
#     results_files,
#     {
#       flog.info("Traitement de l'entité : %s", entity$identifiers[["id"]])
#       flog.info("Traitement de l'action : %s", actions)
#       
#       create_global_tuna_atlas_dataset_v2023(actions, entity, config)  # 🔥 `config` reste un objet R6
#       
#       output_file <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
#       
#       if (file.exists(output_file)) {
#         flog.info("Fichier généré avec succès : %s", output_file)
#         output_file
#       } else {
#         flog.error("Le fichier attendu n'a pas été généré : %s", output_file)
#         stop("Erreur : fichier non généré")
#       }
#     },
#     pattern = map(entity, actions),
#     format = "file"
#   ),
#   
#   # 🔹 6. Regrouper les fichiers en une liste pour éviter de brancher sur un pattern
#   tar_target(
#     results_list,
#     results_files
#   ),
#   
#   # 🔹 7. Lire et sauvegarder les résultats
#   tar_target(
#     save_results,
#     {
#       df_list <- lapply(results_list, read.csv)
#       df_final <- do.call(rbind, df_list)
#       
#       write.csv(df_final, "results/tuna_atlas_results.csv")
#       "results/tuna_atlas_results.csv"
#     },
#     format = "file"
#   )
# )
