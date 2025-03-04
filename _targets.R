# Chargement des packages nécessaires
library(targets)
library(here)
library(futile.logger)
library(geoflow)

# Restaurer l'environnement `renv`
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

# 🔹 SOURCING des fonctions nécessaires
source(here::here("tunaatlas_scripts/generation/create_global_tuna_atlas_dataset_v2023.R"))
source(here::here("R/running_time_of_workflow.R"))
source(here::here("R/executeAndRename.R"))
# Définir les options globales de `{targets}`
tar_option_set(
  packages = c(
    "jsonlite", "here", "purrr", "futile.logger", "geoflow",
    "dotenv", "data.table", "dplyr", "DBI", "RPostgreSQL",
    "rpostgis", "ggplot2", "readr", "sf", "bookdown", "knitr", "tidyr"
  )
)

# config <- initWorkflow(here::here("catch_ird_level2_local.json"))  # 🔥 Charge en dehors de `{targets}`
# entity <- config$metadata$content$entities[[1]]
# action <- entity$data$actions[[1]]
# opts <- action$options
# Définition du pipeline
list(
  tar_target(
    config,
    initWorkflow(here::here("catch_ird_level2_local.json"))  # 🔥 recréer `config` depuis le fichier
  ),
  
  tar_target(
    entity,
    config$metadata$content$entities[[1]]
  ),
  
  tar_target(
    action,
    entity$data$actions[[1]]
  ),
  tar_target(
    opts,
    action$options
  ),
  tar_target(
    catch_file,
    {
      # 🔹 Ajout explicite des valeurs de opts pour créer une dépendance
      key <- opts$keygeoref  
      doi <- opts$doigeoref  
      
      zen4R::download_zenodo(doi = doi, files = key, path = here::here("data"))
    },
    format = "file"
  ),
  
  tar_target(
    nominal_catch_file,
    {
      keynominal <- opts$keynominal  
      doinominal <- opts$doinominal  
      
      zen4R::download_zenodo(doi = doinominal, files = keynominal, path = here::here("data"))
    },
    format = "file"
  ),
  # 🔹 3. Exécuter `create_global_tuna_atlas_dataset_v2023()` avec cette entité
  tar_target(
    results_file,
    {
      flog.info("Traitement de l'entité : %s", entity$identifiers[["id"]])
      
      # 🔹 Ajout explicite des fichiers pour créer une dépendance
      nominal_catch <- nominal_catch_file  
      catch <- catch_file  
      
      create_global_tuna_atlas_dataset_v2023(action, entity, config)
      
      output_file <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
      
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
      df <- read.csv(results_file)  # Lire le fichier généré
      write.csv(df, "results/tuna_atlas_results.csv")  # Sauvegarde finale
      "results/tuna_atlas_results.csv"
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
