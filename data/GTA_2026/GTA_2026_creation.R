# Load 'renv' for project-specific environments
# if (!require("renv")) install.packages("renv")
library(renv)
# install.packages("pak")
# pak::pak("bastienird/CWP.dataset")
# Activate the project environment (if using project-specific libraries)
# renv::activate()
# Restore the project library (if using renv)
renv::restore()
library(readr)
presence_absence_flagging <- read_csv(here::here("Species_Presence___Absence.csv"))
# Define all required packages (excluding 'base' and 'utils' as they are always available)
required_packages <- c(
  "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor", 
  "dotenv", "data.table", "here", "xfun", "RPostgreSQL", "RPostgres", "DBI", 
  "rpostgis", "terra", "sf", "RSQLite", "webshot", "usethis", "ows4R", "sp", 
  "flextable", "dplyr", "stringr", "tibble", "bookdown", "knitr", 
  "purrr", "readxl", "odbc", "rlang", "kableExtra", "tidyr", "ggplot2", "fs" ,
  "stats", "RColorBrewer", "cowplot", "tmap", "curl", "officer", 
  "gdata", "R3port", "reshape2", "tools", "plogr", "futile.logger", "lubridate", "data.table"
)

# Function to check, install (if necessary), and load a package
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    # install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Apply the function to each required package
sapply(required_packages, install_and_load)
require(geoflow)

# Note: This script assumes that the internet connection is available and
# the CRAN/GitHub repositories are accessible for package installation.

# Choose your .env in which you have stored you password for googledrive (if wanted) and database (mandatory)

default_file <- ".env"

if(file.exists(here::here("geoserver_sdi_lab.env"))){
  default_file <- "geoserver_sdi_lab.env"
} # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

# if(file.exists(here("geoserver_cines.env"))){
#   default_file <- here("geoserver_cines.env")
# } # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

load_dot_env(file = here::here(default_file)) # to be replaced by the one used
# load_dot_env(file = "~/Documents/Tunaatlas_level1/catch_local.env")
source(here::here("R/running_time_of_workflow.R"))
source(here::here("R/executeAndRename.R"))

patch_geoflow_getJobDataResource <- function() {
  gen <- geoflow::geoflow_entity
  
  gen$set(
    "public", "getJobDataResource",
    overwrite = TRUE,
    value = function(config, path) {
      path <- gsub("\\\\", "/", path)
      file.path(getwd(), "data", basename(path))
    }
  )
  
  invisible(TRUE)
}

patch_geoflow_getJobDataResource()
stop("Stop")
# First step is creation of the database model and loading of the codelist (around 5 minutes)
copy_all_nested_data_folders <- function(source_root, target_data_folder = here::here("data")) {
  # Cherche tous les dossiers nommés 'data' à n'importe quel niveau
  data_dirs <- list.dirs(source_root, recursive = TRUE, full.names = TRUE)
  data_dirs <- data_dirs[basename(data_dirs) == "data"]
  
  # Boucle sur chaque dossier 'data' trouvé et copie son contenu
  for (dir in data_dirs) {
    files_to_copy <- list.files(dir, full.names = TRUE, recursive = TRUE)
    file.copy(files_to_copy, target_data_folder, overwrite = TRUE, recursive = TRUE)
  }
}

## Nominal data: These datasets are mandatory to create the georeferenced dataset level 2. For level 0 or 1 they are not mandatory time around 2.7 minutes
# Around 2.7 minutes
setwd(here::here("data/GTA_2026"))
raw_nominal_catch <- executeWorkflow(here::here("config/Nominal_catch_2026.json"))
raw_nominal_catch <- executeAndRename(raw_nominal_catch, "_raw_nominal_catch_2026")
running_time_of_workflow(raw_nominal_catch)


## Georeferenced catch: These datasets contains catch AND EFFORT FOR SOME DATA as effort are used to raise catch data for level 0 to 2
# Around 1.2 hours
raw_data_georef <- executeWorkflow(here::here("config/All_raw_data_georef.json"))
raw_data_georef <- executeAndRename(raw_data_georef, "_raw_data_georef_2026")
dir.create(file.path(getwd(), "data"))
copy_all_nested_data_folders(source_root = getwd(),target_data_folder = file.path(getwd(), "data"))

running_time_of_workflow(raw_data_georef)
require(CWP.dataset)
config <- initWorkflow(here::here("config/tunaatlas_qa_dbmodel+codelists.json"))
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
source("~/firms-gta/geoflow-tunaatlas/data/GTA_2026/jobs/20260203133402_raw_data_georef_2024/testsumarising_invalid.R")
# source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
time_Summarising_invalid_data <- system.time({
  testsummarising_invalid_data(raw_data_georef, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)
})

## Goereferenced effort: These datasets are used to create the georeferenced effort
# Around 30 minutes
setwd("~/firms-gta/geoflow-tunaatlas")

# restaurer getJobdaatresource au cas ou
# ns  <- asNamespace("geoflow")
# gen <- get("geoflow_entity", ns)
# gen$set("public", "getJobDataResource",
#         get(".geoflow_old_getJobDataResource", envir = .GlobalEnv),
#         overwrite = TRUE)
raw_data_georef_effort <- executeWorkflow(here::here("config/All_raw_data_georef_effort.json"))# for iattc 5 deg, only keep the tuna because not much differneces betwwen the two, mostly duplicates
raw_data_georef_effort <- executeAndRename(raw_data_georef_effort, "_raw_data_georef_effort")
# copy_all_nested_data_folders(source_root = getwd(),target_data_folder = file.path(getwd(), "data"))
running_time_of_workflow(raw_data_georef_effort)
config <- initWorkflow(here::here("config/All_raw_data_georef_effort.json"), handleMetadata = FALSE)
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
con <- NULL
time_Summarising_invalid_data <- system.time({
  testsummarising_invalid_data(raw_data_georef_effort, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)
})


copy_job_reports_to_repo <- function(
    job_dir,
    repo_dir = "~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization",
    pattern = "Report\\.Rmd$",
    overwrite = TRUE,
    backup_before = TRUE
) {
  job_dir  <- path.expand(job_dir)
  repo_dir <- path.expand(repo_dir)
  
  if (!dir.exists(job_dir)) stop("job_dir introuvable: ", job_dir)
  if (!dir.exists(repo_dir)) stop("repo_dir introuvable: ", repo_dir)
  
  # 1) trouver tous les Report.Rmd sous entities
  entities_dir <- file.path(job_dir, "entities")
  if (!dir.exists(entities_dir)) stop("Pas de dossier entities dans: ", job_dir)
  
  src <- list.files(entities_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (!length(src)) {
    message("Aucun fichier correspondant à /", pattern, "/ trouvé sous ", entities_dir)
    return(invisible(data.frame()))
  }
  
  rfmos <- c("iotc", "iccat", "wcpfc", "iattc", "ccsbt")
  
  # helpers
  detect_rfmo <- function(x) {
    xlow <- tolower(x)
    m <- regexec("tunaatlas([a-z]+)", xlow)
    g <- regmatches(xlow, m)[[1]]
    if (length(g) >= 2 && g[2] %in% rfmos) return(g[2])
    for (r in rfmos) if (grepl(r, xlow, fixed = TRUE)) return(r)
    NA_character_
  }
  detect_meas <- function(x) {
    xlow <- tolower(x)
    if (grepl("effort", xlow)) return("effort")
    if (grepl("catch",  xlow)) return("catch")
    NA_character_
  }
  
  # 2) construire les destinations
  df <- data.frame(
    src = src,
    file = basename(src),
    rfmo = vapply(basename(src), detect_rfmo, character(1)),
    meas = vapply(basename(src), detect_meas, character(1)),
    stringsAsFactors = FALSE
  )
  
  # fallback si non détecté depuis le nom: essayer le chemin
  miss_rfmo <- is.na(df$rfmo)
  if (any(miss_rfmo)) {
    df$rfmo[miss_rfmo] <- vapply(df$src[miss_rfmo], detect_rfmo, character(1))
  }
  miss_meas <- is.na(df$meas)
  if (any(miss_meas)) {
    df$meas[miss_meas] <- vapply(df$src[miss_meas], detect_meas, character(1))
  }
  
  bad <- is.na(df$rfmo) | is.na(df$meas)
  if (any(bad)) {
    warning(
      "Impossible de déduire rfmo/meas pour certains fichiers (ignorés):\n  ",
      paste(df$src[bad], collapse = "\n  ")
    )
    df <- df[!bad, , drop = FALSE]
    if (!nrow(df)) return(invisible(data.frame()))
  }
  
  df$dest_dir  <- file.path(repo_dir, df$rfmo, df$meas)
  df$dest_path <- file.path(df$dest_dir, df$file)
  
  # 3) backup optionnel (un tar.gz par dossier cible concerné)
  if (backup_before) {
    unique_dirs <- unique(df$dest_dir)
    for (d in unique_dirs) {
      if (!dir.exists(d)) next
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      rfmo_meas <- paste(basename(dirname(d)), basename(d), sep = "_")
      out_tgz <- file.path(tempdir(), paste0("backup_pre-harmonization_", rfmo_meas, "_", stamp, ".tar.gz"))
      
      # tar() crée l'archive dans out_tgz, contenant le dossier d
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(dirname(d))
      utils::tar(tarfile = out_tgz, files = basename(d), compression = "gzip")
      message("Backup créé: ", out_tgz)
    }
  }
  
  # 4) copie
  copied <- 0L
  replaced <- 0L
  created <- 0L
  
  for (i in seq_len(nrow(df))) {
    dir.create(df$dest_dir[i], recursive = TRUE, showWarnings = FALSE)
    
    existed <- file.exists(df$dest_path[i])
    ok <- file.copy(df$src[i], df$dest_path[i], overwrite = overwrite)
    if (!ok) warning("Échec copie: ", df$src[i], " -> ", df$dest_path[i])
    
    copied <- copied + as.integer(ok)
    if (ok && existed) replaced <- replaced + 1L
    if (ok && !existed) created  <- created  + 1L
  }
  
  message("Copiés: ", copied, " | Remplacés: ", replaced, " | Nouveaux: ", created)
  df
}

# Exemple :
res <- copy_job_reports_to_repo("~/firms-gta/geoflow-tunaatlas/jobs/20260220134342_raw_data_georef_effort_raw_data_georef_effort")


setwd("~/firms-gta/geoflow-tunaatlas")
tunaatlas_qa_global_datasets_effort_path <- executeWorkflow(here::here("config/create_effort_dataset_2026.json"))  # FROM LOCAL IF NOT RUNNING USE DRIVE
tunaatlas_qa_global_datasets_effort_path <- executeAndRename(tunaatlas_qa_global_datasets_effort_path, "new_efforts")
# tunaatlas_qa_services <- initWorkflow("tunaatlas_qa_services.json")
# save.image()
# tunaatlas_qa_global_datasets_catch_path <- "jobs/20241104162955/entities/global_catch_ird_level2_rf1"
tunaatlas_qa_global_datasets_catch_path <- executeAndRename(tunaatlas_qa_global_datasets_catch_path, "new_level_1_2_01_2025")
### TODO add create_materialized_view_for_shiny_apps.R in the end of the workflow action on end

running_time_of_workflow(tunaatlas_qa_global_datasets_catch_path)
create_materialized_view <- ""