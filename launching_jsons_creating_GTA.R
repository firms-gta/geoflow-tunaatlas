# This script ensures that required R packages are installed and loaded.

# Function to check, install, and load a package
check_install_package <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    require(package_name, character.only = TRUE)
  }
}

# 'renv' for project-specific environments
check_install_package("renv")
# renv::activate()
renv::restore(prompt = FALSE) # Restore the project library

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
check_install_package("xfun") # Handling path

# Database related packages
check_install_package("RSQLite")    # SQLite interface
check_install_package("RPostgreSQL") # PostgreSQL interface
check_install_package("RPostgres")   # Alternative PostgreSQL interface
check_install_package("DBI")        # Database interface
check_install_package("rpostgis")   # PostGIS interface
# remove.packages("rgeos")
check_install_package("terra")
check_install_package("sf")

if(!require(tinytex)) {
  check_install_package("tinytex")
  tinytex::install_tinytex()
}

# Geospatial packages
# check_install_package("rgeos")     # Geospatial operations

# Additional, project-specific packages
# 'geoflow' package from GitHub for workflow management
if (!require(geoflow)) {
  remotes::install_github("eblondel/geoflow")
  require(geoflow)
}

executeAndRename <- function(executed_file, suffix) {
  # Execute workflow
  # executed_file <- executeWorkflow(here::here(executed_file))
  
  # Derive folder and file names
  folder_file <- file.path("jobs", basename(executed_file))
  
  # Rename the file with the given suffix
  file.rename(folder_file, paste0("jobs/", basename(executed_file), suffix))
  return(paste0("jobs/", basename(executed_file), suffix))
}

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
# load_dot_env(file = "~/Documents/Tunaatlas_level1/catch_local.env")# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/cwp_grids.R")

# First step is creation of the database model and loading of the codelist (around 1/2 hour)
db_model <- executeWorkflow(here("tunaatlas_qa_dbmodel+codelists.json")) 
db_model <- executeAndRename(db_model, "_db_model")

# Second step is the loading of the mappings (1h)
mappings <- executeWorkflow(here("tunaatlas_qa_mappings.json"))
mappings <- executeAndRename(mappings, "_mappings")

# Third step is pre-harmonizing the datasets provide by tRFMOs: This step is divided in 3 
# substep depending on the type of the data:

## Nominal data: These datasets are mandatory to create the georeferenced dataset level 2. For level 0 or 1 they are not mandatory

raw_nominal_catch <- executeWorkflow(here::here("Raw_nominal_catch.json"))
raw_nominal_catch <- executeAndRename(raw_nominal_catch, "_raw_nominal_catch")

## Georeferenced catch: These datasets contains catch AND EFFORT for some data as effort are used to raise catch data for level 0 to 2

raw_data_georef <- executeWorkflow(here::here("All_raw_data_georef.json"))
raw_data_georef <- executeAndRename(raw_data_georef, "_raw_data_georef")

## Goereferenced effort: These datasets are used to create the georeferenced effort

raw_data_georef_effort <- executeWorkflow(here::here("All_raw_data_georef_effort.json"))
raw_data_georef_effort <- executeAndRename(raw_data_georef_effort, "_raw_data_georef_effort")


## Summarising the invalid data for all the datasets pre-harmonized
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developpement/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
config <- initWorkflow(here::here("All_raw_data_georef.json"), handleMetadata = FALSE)
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
Summarising_invalid_data(raw_data_georef, connectionDB = con)
Summarising_invalid_data(raw_data_georef_effort, connectionDB = con)

## These two lines of codes creates a recap for each entity of the irregularities of the data for the datasets. 
# They also creates a report summarising the irregular data for each entity so it is easier to target them


# Create 5 datasets catch and effort. These entities are the final one published on zenodo. 

tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
tunaatlas_qa_global_datasets_catch_path <- executeAndRename(tunaatlas_qa_global_datasets_catch_path, "_tunaatlas_qa_global_datasets_catch_path")


## Recapitulation of all the treatment done for each final dataset, these allows the recap of each step to ensure comprehension of the impact of each treatment
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developpement/Analysis_markdown/functions/Summarising_step.R")
config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
Summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, config  =config)

## Netcdf creation (24h for level 2). This step is to create a netcdf file of the created data. It takes a very long time but creates a very light and comprehensive dataset
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developpement/tunaatlas_actions/convert_to_netcdf.R")
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

# Pakcages for markdown
required_packages <- c("webshot",
                       "here", "usethis","ows4R","sp", "data.table", "flextable", "readtext", "sf", "dplyr", "stringr", "tibble","xfun",
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

# These step is to be done once nominal and georeferenced data are created. It extract irregular data that is :
# - Existing on the georeferenced dataset but not in the nominal
# - Superior in the georeferenced dataset than in the nominal
# The strata_in_georef_but_not_in_nominal_report_launching function return several html files for each tRFMO and 
# for several stratas used to inspect the data (more details in the report)

# This function also return an upgraded_nominal dataset which is the nominal dataset raised from the georeferenced data

upgraded_nominal <- strata_in_georef_but_not_in_nominal_report_launching(tunaatlas_qa_global_datasets_catch_path,
                                                                         connectionDB = con)

CPUE <- strata_with_catches_without_effort(tunaatlas_qa_global_datasets_catch_path,
                                                                         connectionDB = con)



# Check on CPUE data for georeferenced in case some catch are not displayed with any effort
global_catch_firms_level0_public <- read_csv(file.path(tunaatlas_qa_global_datasets_catch_path,"entities/global_catch_firms_level0_/data/global_catch_firms_level0__public.csv"))
cwp_catch <- unique(global_catch_firms_level0_public$geographic_identifier)


# Putting dataset on geoserver, geonetwork and zenodo #For now zenodo does not work due to issue with api
tunaatlas_qa_services <- initWorkflow("tunaatlas_qa_services.json")

# Enriching data with copernicus data
all_files <- list.files(getwd(), pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
netcdf_file_to_enrich <- all_files[!grepl("nominal", all_files)]

