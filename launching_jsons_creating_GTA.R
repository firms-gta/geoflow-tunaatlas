# This script ensures that required R packages are installed and loaded.

# Function to check, install, and load a package
check_install_package <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    require(package_name, character.only = TRUE)
  }
}

# 'renv' for project-specific environments
# check_install_package("renv")
# renv::restore() # Restore the project library

# General utility packages
check_install_package("remotes")   # Package management
check_install_package("tinytex")   # LaTeX support
check_install_package("googledrive")  # Google Drive integration
check_install_package("gsheet")    # Google Sheets integration
check_install_package("readr")     # Data import
check_install_package("plotrix")   # Plotting utilities
check_install_package("janitor")   # Data cleaning
check_install_package("dotenv")    # Environment variable management
check_install_package("data.table") # Data manipulation (Note: Marked for removal)
check_install_package("here") # Handling path

# Database related packages
check_install_package("RSQLite")    # SQLite interface
check_install_package("RPostgreSQL") # PostgreSQL interface
check_install_package("RPostgres")   # Alternative PostgreSQL interface
check_install_package("DBI")        # Database interface
check_install_package("rpostgis")   # PostGIS interface

# Geospatial packages
check_install_package("rgeos")     # Geospatial operations

# Additional, project-specific packages
# 'geoflow' package from GitHub for workflow management
if (!require(geoflow)) {
  remotes::install_github("eblondel/geoflow")
  require(geoflow)
}

# Note: This script assumes that the internet connection is available and
# the CRAN/GitHub repositories are accessible for package installation.

# Choose your .env in which you have stored you password for googledrive (if wanted) and database (mandatory)

default_file <- ".env"

if(file.exists(here::here("geoserver_sdi_lab.env"))){
  default_file <- "geoserver_sdi_lab.env"
} # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

if(file.exists(here("geoserver_cines.env"))){
  default_file <- here("geoserver_cines.env")
} # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

load_dot_env(file = here::here(default_file)) # to be replaced by the one used
# load_dot_env(file = "~/Documents/Tunaatlas_level1/catch_local.env")# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/cwp_grids.R")

# First step is creation of the database model and loading of the codelist (around 1/2 hour)
db_model <- executeWorkflow(here("tunaatlas_qa_dbmodel+codelists.json")) 

# Second step is the loading of the mappings (1h)
mappings <- executeWorkflow(here("tunaatlas_qa_mappings.json"))

# Third step is pre-harmonizing the datasets provide by tRFMOs: This step is divided in 3 
# substep depending on the type of the data:

## Nominal data

nominal_catch <- executeWorkflow(here::here("Nominal_catch.json"))

## Georeferenced catch

raw_data_georef <- executeWorkflow(here::here("All_raw_data_georef.json"))

## Goereferenced effort

raw_data_georef_effort <- executeWorkflow(here::here("All_raw_data_georef_effort.json"))


## Summarising the invalid data for all the datasets pre-harmonized
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
config <- initWorkflow(here::here("All_raw_data_georef.json"), handleMetadata = FALSE)
con <- config$software$output$dbi
Summarising_invalid_data(raw_data_georef, connectionDB = con)
Summarising_invalid_data(raw_data_georef_effort, connectionDB = con)


# Create 5 datasets catch and effort

tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"), dir = here::here())


## Recapitulation of all the treatment done for each final dataset
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/Summarising_step.R")
config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"), handleMetadata = FALSE)
con <- config$software$output$dbi
Summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, config  =config)

## Netcdf creation (24h for level 2)
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/tunaatlas_actions/convert_to_netcdf.R")
entity_dirs <- list.dirs(file.path(tunaatlas_qa_global_datasets_catch_path, "entities"), full.names = TRUE, recursive = FALSE)
config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))

wd <- getwd()
config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))

for (entitynumber in 1:length(config$metadata$content$entities)){
  entity <- config$metadata$content$entities[[entitynumber]]
  dataset_pid <- entity$identifiers[["id"]]
  setwd(file.path(tunaatlas_qa_global_datasets_catch_path,"entities", dataset_pid))
  action <- entity$data$actions[[1]]
  convert_to_netcdf(action, config, entity, uploadgoogledrive = TRUE)
} #could also be in global action but keep in mind it is very long
setwd(wd)

# Checking on created data
required_packages <- c("webshot",
                       "here", "usethis","ows4R","sp", "data.table", "flextable", "readtext", "sf", "dplyr", "stringr", "tibble",
                       "bookdown", "knitr", "purrr", "readxl", "base", "remotes", "utils", "DBI", 
                       "odbc", "rlang", "kableExtra", "readr", "tidyr", "ggplot2", "stats", "RColorBrewer", 
                       "cowplot", "tmap", "RPostgreSQL", "curl", "officer", "gdata", "tidyr", "knitr", "tmap"
)

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

upgraded_nominal <- strata_in_georef_but_not_in_nominal_report_launching(tunaatlas_qa_global_datasets_catch_path,
                    connectionDB = con)

# Putting dataset on geoserver, geonetwork and zenodo
tunaatlas_qa_services <- executeWorkflow("tunaatlas_qa_services.json")

# Enriching data with copernicus data
all_files <- list.files(getwd(), pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
netcdf_file_to_enrich <- all_files[!grepl("nominal", all_files)]

