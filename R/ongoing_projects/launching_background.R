# Load 'renv' for project-specific environments
# if (!require("renv")) install.packages("renv")
library(renv)
message("detectCores: ", parallel::detectCores())
message("availableCores: ", parallelly::availableCores())
message("mc.cores option: ", getOption("mc.cores"))
# install.packages("pak")
# pak::pak("bastienird/CWP.dataset")
# Activate the project environment (if using project-specific libraries)
# renv::activate()
# Restore the project library (if using renv)
# renv::restore()
library(readr)
presence_absence_flagging <- read_csv(here::here("Species_Presence___Absence.csv"))
# Define all required packages (excluding 'base' and 'utils' as they are always available)
required_packages <- c(
  "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor", 
  "dotenv", "data.table", "here", 
  # "xfun",
  "RPostgreSQL", "RPostgres", "DBI", 
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
setwd("~/firms-gta/geoflow-tunaatlas/")
require(here)
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

tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("config/catch_ird_level2_local.json")) # FROM DRIVE
#tobeaddedlaterindata,./data/GTA_2026/dataoutputpreharmo/catch_iotc_level0.csv and ./data/GTA_2026/dataoutputpreharmo/catch_iccat_level0.csv,
tunaatlas_qa_global_datasets_catch_path <- executeAndRename(tunaatlas_qa_global_datasets_catch_path, "level_2_catch_2026")
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
CWP.dataset::summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                              config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                              source_authoritylist = c("IATTC"))
CWP.dataset::summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                              config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                              source_authoritylist = c("ICCAT"))
CWP.dataset::summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                              config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                              source_authoritylist = c("IOTC"))
CWP.dataset::summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                              config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                              source_authoritylist = c("WCPFC"))
CWP.dataset::summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                              config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                              source_authoritylist = c("CCSBT"))

config$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <- list(species = c("YFT", "SKJ", "BET", "ALB", "SBF", "SWO"))
source(("~/firms-gta/geoflow-tunaatlas/R/ongoing_projects/summarising_step2.R"))
summarising_step2(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                  config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                  source_authoritylist = c("all"), nameoutput = "majortunas")
no_sbf <- setdiff(unique((qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260311191047level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/rawdata/ancient.qs"))$species), "SBF")

config$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <- list(species = no_sbf)
source(("~/firms-gta/geoflow-tunaatlas/R/ongoing_projects/summarising_step2.R"))
summarising_step2(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                  config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                  source_authoritylist = c("all"), nameoutput = "noSBF")

config$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <- list(species = c("YFT", "SKJ", "BET", "ALB", "SBF", "SWO", "TUN", "TUS"))
summarising_step2(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, 
                  config  = config, sizepdf = "short",savestep = FALSE, usesave = FALSE, 
                  source_authoritylist = c("all"), nameoutput = "majortunasandTUN")
