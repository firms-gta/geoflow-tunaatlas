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
# Wokflow for us to create datasets etc.. load data correct endroit, create report etc.;
## Nominal data: These datasets are mandatory to create the georeferenced dataset level 2. For level 0 or 1 they are not mandatory time around 2.7 minutes
# Around 2.7 minutes
raw_nominal_catch <- executeWorkflow(here::here("config/Nominal_catch_2026.json"))
raw_nominal_catch <- executeAndRename(raw_nominal_catch, "_raw_nominal_catch_2026_final")
running_time_of_workflow(raw_nominal_catch)
time_Summarising_invalid_data <- system.time({
  summarising_invalid_data(raw_nominal_catch, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)
})

## Georeferenced catch: These datasets contains catch AND EFFORT FOR SOME DATA as effort are used to raise catch data for level 0 to 2
# Around 1.2 hours
raw_data_georef <- executeWorkflow(here::here("config/All_raw_data_georef.json"))
raw_data_georef <- executeAndRename(raw_data_georef, "_raw_data_georef_2026")
dir.create(file.path(getwd(), "data"))
# copy_all_nested_data_folders(source_root = getwd(),target_data_folder = file.path(getwd(), "data"))

running_time_of_workflow(raw_data_georef)
require(CWP.dataset)
config <- initWorkflow(here::here("config/tunaatlas_qa_dbmodel+codelists.json"))
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
con <- NULL
time_Summarising_invalid_data <- system.time({
  CWP.dataset::summarising_invalid_data(raw_data_georef, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)
})
source("~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization/rewrite_functions_as_rmd.R")
rewrite_functions_as_rmd(raw_data_georef)
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
raw_data_georef_effort <- executeAndRename(raw_data_georef_effort, "_raw_data_georef_effort_final")
# copy_all_nested_data_folders(source_root = getwd(),target_data_folder = file.path(getwd(), "data"))
running_time_of_workflow(raw_data_georef_effort)
config <- initWorkflow(here::here("config/All_raw_data_georef_effort.json"), handleMetadata = FALSE)
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
con <- NULL
time_Summarising_invalid_data <- system.time({
  CWP.dataset::summarising_invalid_data(raw_data_georef_effort, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)
})


# towrite rmd to commit on github
source("~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization/rewrite_functions_as_rmd.R")
rewrite_functions_as_rmd(raw_data_georef)
rewrite_functions_as_rmd(raw_data_georef_effort)
rewrite_functions_as_rmd(raw_nominal_catch)
# remove_specific_files(here::here("R/tunaatlas_scripts/pre-harmonization"))
# truncate_files("~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization")
# Appel à la fonction pour supprimer les fichiers spécifiques

# puis commit sur github

# Troncature des fichiers CSV et XLSX après la génération des fichiers HTML


setwd("~/firms-gta/geoflow-tunaatlas")
tunaatlas_qa_global_datasets_effort_path <- executeWorkflow(here::here("config/create_effort_dataset_2026.json"))  # FROM LOCAL IF NOT RUNNING USE DRIVE
tunaatlas_qa_global_datasets_effort_path <- executeAndRename(tunaatlas_qa_global_datasets_effort_path, "new_efforts_final")

# Level 0 2026 ------------------------------------------------------------

tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("config/catch_ird_level0_local.json"))
tunaatlas_qa_global_datasets_catch_path <- executeAndRename(tunaatlas_qa_global_datasets_catch_path, "level_0_catch_2026")
CWP.dataset::summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                              config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                              source_authoritylist = c("all"))

setwd("~/firms-gta/geoflow-tunaatlas")
tunaatlas_qa_global_datasets_effort_path <- executeWorkflow(here::here("config/create_nominal_dataset_2026.json"))  # FROM LOCAL IF NOT RUNNING USE DRIVE
tunaatlas_qa_global_datasets_effort_path <- executeAndRename(tunaatlas_qa_global_datasets_effort_path, "nominal_final")


tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("config/catch_ird_level2_local.json")) # FROM DRIVE
tunaatlas_qa_global_datasets_catch_path <- executeAndRename(tunaatlas_qa_global_datasets_catch_path, "level_2_catch_2025")
gc()
config <- initWorkflow(here::here("config/catch_ird_level2_local.json"))
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
gc()
require(CWP.dataset)
setwd("~/firms-gta/geoflow-tunaatlas")
CWP.dataset::summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                              config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                              source_authoritylist = c("all"))




# Loading DB or DB only (if user has access to DB), workflow for users only -------------------------------------------------------

library(DBI)

default_file <- ".env"

if(file.exists(here::here("geoserver_sdi_lab.env"))){
  default_file <- "geoserver_sdi_lab.env"
} # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

load_dot_env(file = here::here(default_file)) # 

should_upload_to_db <- function(config) {
  # 1) Vérifs "contexte" via variables d'env
  drv  <- Sys.getenv("DB_DRV")
  host <- Sys.getenv("DB_HOST")
  port <- Sys.getenv("DB_PORT")
  db   <- Sys.getenv("DB_NAME")
  user <- Sys.getenv("DB_USER")
  
  ctx_ok <- identical(drv, "PostgreSQL") &&
    identical(port, "5432") &&
    identical(host, "db-tunaatlas.d4science.org") &&
    db %in% c("tunaatlas_sandbox", "tunaatlas") &&
    identical(user, "tunaatlas_u")
  
  if (!ctx_ok) return(FALSE)
  
  # 2) Vérif connexion réelle
  con <- config$software$output$dbi
  if (is.null(con)) return(FALSE)
  
  ok <- tryCatch({
    DBI::dbIsValid(con) && DBI::dbGetQuery(con, "SELECT 1 AS ok")$ok[1] == 1
  }, error = function(e) FALSE)
  
  ok
}

force_upload_to_db <- function(config, action_id = "load_dataset", value = TRUE) {
  for (ei in seq_along(config$entities)) {
    acts <- config$entities[[ei]]$actions
    ids  <- vapply(acts, `[[`, character(1), "id")
    sel  <- which(ids == action_id)
    for (j in sel) {
      config$entities[[ei]]$actions[[j]]$options$upload_to_db <- TRUE
      config$entities[[ei]]$actions[[j]]$options$upload_to_db_public <- TRUE
      config$entities[[ei]]$actions[[j]]$options$create_materialized_view <- TRUE
    }
  }
  config
}

raw_nominal_catch <- executeWorkflow(
  file = here::here("config/Nominal_catch_2026.json"),
  dir  = ".",
  on_initWorkflow = function(config, queue) {
    
    if (should_upload_to_db(config)) {
      config$logger.info("DB reachable + context OK -> upload_to_db=TRUE for load_dataset")
      force_upload_to_db(config, "load_dataset", TRUE)
    } else {
      config$logger.info("DB not reachable or context not OK -> upload_to_db unchanged")
    }
    
  }
)


raw_data_georef <- executeWorkflow(
  file = here::here("config/All_raw_data_georef.json"),
  dir  = ".",
  on_initWorkflow = function(config, queue) {
    
    if (should_upload_to_db(config)) {
      config$logger.info("DB reachable + context OK -> upload_to_db=TRUE for load_dataset")
      force_upload_to_db(config, "load_dataset", TRUE)
    } else {
      config$logger.info("DB not reachable or context not OK -> upload_to_db unchanged")
    }
    
  }
)

raw_data_georef_effort <- executeWorkflow(
  file = here::here("config/All_raw_data_georef_effort.json"),
  dir  = ".",
  on_initWorkflow = function(config, queue) {
    
    if (should_upload_to_db(config)) {
      config$logger.info("DB reachable + context OK -> upload_to_db=TRUE for load_dataset")
      force_upload_to_db(config, "load_dataset", TRUE)
    } else {
      config$logger.info("DB not reachable or context not OK -> upload_to_db unchanged")
    }
    
  }
)

config <- initWorkflow(here::here("config/All_raw_data_georef_effort.json"), handleMetadata = FALSE)
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi

CWP.dataset::summarising_invalid_data(raw_nominal_catch, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)
CWP.dataset::summarising_invalid_data(raw_data_georef, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)
CWP.dataset::summarising_invalid_data(raw_data_georef_effort, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)