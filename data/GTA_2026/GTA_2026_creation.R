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
raw_nominal_catch <- executeAndRename(raw_nominal_catch, "_raw_nominal_catch_2024")
running_time_of_workflow(raw_nominal_catch)


## Georeferenced catch: These datasets contains catch AND EFFORT FOR SOME DATA as effort are used to raise catch data for level 0 to 2
# Around 1.2 hours
raw_data_georef <- executeWorkflow(here::here("config/All_raw_data_georef.json"))
raw_data_georef <- executeAndRename(raw_data_georef, "_raw_data_georef_2024")
dir.create(file.path(getwd(), "data"))
copy_all_nested_data_folders(source_root = getwd(),target_data_folder = raw_data_georef)

running_time_of_workflow(raw_data_georef)
require(CWP.dataset)
config <- initWorkflow(here::here("config/tunaatlas_qa_dbmodel+codelists.json"))
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
source("~/firms-gta/geoflow-tunaatlas/data/GTA_2026/jobs/20260203133402_raw_data_georef_2024/testsumarising_invalid.R")
# source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
time_Summarising_invalid_data <- system.time({
  setwd(here::here("data/GTA_2026"))
  testsummarising_invalid_data(raw_data_georef, connectionDB = con, upload_DB = FALSE,upload_drive = FALSE)
})

## Goereferenced effort: These datasets are used to create the georeferenced effort
# Around 30 minutes
setwd(here::here("data/GTA_2026"))
raw_data_georef_effort <- executeWorkflow(here::here("config/All_raw_data_georef_effort.json"))# for iattc 5 deg, only keep the tuna because not much differneces betwwen the two, mostly duplicates
raw_data_georef_effort <- executeAndRename(raw_data_georef_effort, "_raw_data_georef_effort")
copy_all_nested_data_folders(raw_data_georef_effort)
copy_all_nested_data_folders(raw_data_georef_effort, target_data_folder = "efforts_all")
running_time_of_workflow(raw_data_georef_effort)

# source("~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization/rewrite_functions_as_rmd.R")
# safe_rewrite_functions_as_rmd <- function(source_path) {
#   tryCatch({
#     rewrite_functions_as_rmd(source_path)
#   }, error = function(e) {
#     message(sprintf("Error processing %s: %s", source_path, e$message))
#   })
# }
# 
# # Appels aux fonctions avec gestion des erreurs
# safe_rewrite_functions_as_rmd(raw_nominal_catch)
# safe_rewrite_functions_as_rmd(raw_data_georef)
# safe_rewrite_functions_as_rmd(raw_data_georef_effort)

## Summarising the invalid data for all the datasets pre-harmonized
# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
# source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
config <- initWorkflow(here::here("config/All_raw_data_georef.json"), handleMetadata = FALSE)
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
time_Summarising_invalid_data <- system.time({
  summarising_invalid_data(raw_data_georef, connectionDB = con, upload_DB = FALSE)
})


#Around 1 minute
time_Summarising_invalid_data_georef <- system.time({
  Summarising_invalid_data(raw_data_georef_effort, connectionDB = con)
})

# Summar