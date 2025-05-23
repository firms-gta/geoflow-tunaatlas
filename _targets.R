# Chargement des packages nécessaires
library(targets)
library(here)
library(futile.logger)
library(geoflow)

if (!file.exists(here::here("results"))) {
  dir.create(here::here("results"))}
if (!file.exists(here::here("data"))) {
  dir.create(here::here("data"))}
# Restaurer l'environnement `renv`
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

# 🔹 SOURCING des fonctions nécessaires
targets::tar_source(here::here("tunaatlas_scripts/generation/create_global_tuna_atlas_dataset_v2025.R"))

# Définir les options globales de `{targets}`
tar_option_set(
  packages = c(
    "jsonlite", "here", "purrr", "futile.logger", "geoflow",
    "dotenv", "data.table", "dplyr", "DBI", "RPostgreSQL",
    "rpostgis", "ggplot2", "readr", "sf", "bookdown", "knitr", "tidyr", "CWP.dataset", "utils", "ows4R", "callr"
  )
)
options(
  encoding = "UTF-8",
  stringsAsFactors = FALSE, 
  dplyr.summarise.inform = FALSE
)

Sys.setlocale("LC_ALL", "en_US.UTF-8")
# config <- initWorkflow(here::here("catch_ird_level2_local.json"))  # 
# entity <- config$metadata$content$entities[[1]]
# action <- entity$data$actions[[1]]
# opts <- action$options
# Définition du pipeline
list(
  tar_target(
    config,
    initWorkflow(here::here("catch_ird_level2_local.json"))  #
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
    action,
    entity$data$actions[[1]]
  ),
  tar_target(
    opts,
    action$options
  ),
  tar_target(
    nominal_catch_file,
    {
      # 🔹 Récupération des paramètres de téléchargement
      keynominal <- opts$keynominal  
      doinominal <- opts$doinominal  
      file_path <- here::here("data", keynominal)  # Définition du chemin attendu
      
      # 🔹 Vérification si le fichier existe déjà
      if (!file.exists(file_path)) {
        zen4R::download_zenodo(doi = doinominal, files = keynominal, path = here::here("data"))
      }
      
      # 🔹 Retourne le chemin du fichier
      file_path
    },
    format = "file"
  )
  # ,
  # tar_target(
  #   all_raw_data,
  #   {
  #     # 1) Prépare le dossier et le chemin du ZIP
  #     zipfile <- file.path("data", "All_rawdata_for_level2.zip")
  #     url     <- "https://zenodo.org/record/15496164/files/All_rawdata_for_level2.zip"
  # 
  #     # 2) Télécharge si nécessaire
  #     if (!file.exists(zipfile)) {
  #       utils::download.file(url, zipfile, mode = "wb")
  #     }
  # 
  #     # 3) Dézippe si pas déjà décompressé
  #     files_in_data <- list.files("data", all.files = TRUE, no.. = TRUE)
  #     # on considère qu'il est déjà décompressé s'il y a au moins un fichier qui n'est pas le ZIP
  #     need_unzip <- !any(!grepl("\\.zip$", files_in_data))
  #     if (need_unzip) {
  #       unzip(zipfile, exdir = "data")
  #     }
  # 
  #     # 4) On renvoie la liste des fichiers extraits
  #     list.files("data", full.names = TRUE)
  #   },
  #   format = "file"
  # )
  ,
  # 🔹 3. Exécuter `create_global_tuna_atlas_dataset_v2025()` avec cette entité
  tar_target(
    results_file,
    {
      flog.info("Traitement de l'entité : %s", entity$identifiers[["id"]])

      # 🔹 Ajout explicite des fichiers pour créer une dépendance
      nominal_catch <- nominal_catch_file
      create_global_tuna_atlas_dataset_v2025(action, entity, config)

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
  )
  # ,
  # # 🔹 4. Lire et sauvegarder le fichier généré
  # tar_target(
  #   save_results,
  #   {
  #     df <- read.csv(results_file)  # Lire le fichier généré
  # 
  #     write.csv(df, here::here("results/tuna_atlas_results.csv"))  # Sauvegarde finale
  #     "results/tuna_atlas_results.csv"
  #   },
  #   format = "file"
  # )
  # ,
  # tar_target(
  #   markdown_report,
  #   {
  #     # répertoire racine de votre projet
  #     main_dir <- here::here()
  #     # connexion à la BDD
  #     con <- config$software$output$dbi
  #     
  #     # génération du rapport "court"
  #     CWP.dataset::summarising_step(
  #       main_dir             = main_dir,
  #       connectionDB         = con,
  #       config               = config,
  #       sizepdf              = "short",
  #       savestep             = TRUE,
  #       usesave              = TRUE,
  #       source_authoritylist = "all"
  #     )
  #     
  #     # génération du rapport "moyen"
  #     CWP.dataset::summarising_step(
  #       main_dir             = main_dir,
  #       connectionDB         = con,
  #       config               = config,
  #       sizepdf              = "middle",
  #       savestep             = TRUE,
  #       usesave              = TRUE,
  #       source_authoritylist = "all",
  #       fast_and_heavy       = FALSE
  #     )
  #     
  #     # 2) on liste les PDF produits
  #     pdfs <- list.files(
  #       path       = file.path(main_dir, "_book"), # ou le dossier où sont créés les PDF
  #       pattern    = "\\.(pdf)$",
  #       full.names = TRUE
  #     )
  #     
  #     # on renvoie la liste des chemins produits
  #     pdfs
  #   },
  #   format = "file"
  # )
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
