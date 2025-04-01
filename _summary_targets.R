library(targets)
library(here)
library(futile.logger)

# Charger le fichier `config.rds` (déjà généré dans le premier pipeline)
config <- readRDS(here::here("config.rds"))

# Chemin du dossier Markdown généré dans le premier pipeline
tunaatlas_qa_global_datasets_catch_path <- here::here("Markdown")  # 🔥 Modifie si besoin

# Connection à la base de données
con <- config$software$output$dbi

# Définition du pipeline
list(
  tar_target(
    summarised_results,
    {
      flog.info("Début du résumé des résultats avec Summarising_step")
      
      Summarising_step(
        main_dir = tunaatlas_qa_global_datasets_catch_path,
        connectionDB = con,
        config = config,
        sizepdf = "short",
        savestep = FALSE,
        usesave = FALSE,
        source_authoritylist = c("all", "WCPFC", "IATTC", "ICCAT", "CCSBT", "IOTC")
      )
      
      flog.info("Résumé terminé !")
      
      # On retourne un indicateur que tout s'est bien passé
      file.path(tunaatlas_qa_global_datasets_catch_path, "summary_completed.txt")
    },
    format = "file"
  )
)
