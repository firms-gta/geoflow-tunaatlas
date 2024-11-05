# Load and Set Up Project Environment
# ------------------------------------

# Load 'renv' for project-specific environments
# If not already installed, install 'renv'
if (!require("renv")) install.packages("renv")
library(renv)

# Activate the project environment and restore the library if using 'renv'
renv::activate()
renv::restore()
require(here)
# Define required packages
required_packages <- c(
  "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor", 
  "dotenv", "data.table", "here", "xfun", "RPostgreSQL", "RPostgres", "DBI", 
  "rpostgis", "terra", "sf", "RSQLite", "webshot", "usethis", "ows4R", "sp", 
  "flextable", "dplyr", "stringr", "tibble", "bookdown", "knitr", 
  "purrr", "readxl", "odbc", "rlang", "kableExtra", "tidyr", "ggplot2", 
  "stats", "RColorBrewer", "cowplot", "tmap", "curl", "officer", 
  "gdata", "R3port", "reshape2", "tools", "plogr", "futile.logger", "lubridate"
)

# Install and load packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}
sapply(required_packages, install_and_load)
require(geoflow)

# Workflow Execution and Configurations
# -------------------------------------

# Initialize and execute the workflow
config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi

# Execute workflows and rename outputs
tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))

# Summarizing Step
# ----------------

# Load function and run summarizing step
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/Summarising_step.R")
# Summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, config = config, sizepdf = "middle", savestep = TRUE, usesave = FALSE)


# NetCDF Creation
# ---------------

# # Load function to convert to NetCDF and process entities
# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_actions/convert_to_netcdf.R")
# entity_dirs <- list.dirs(file.path(tunaatlas_qa_global_datasets_catch_path, "entities"), full.names = TRUE, recursive = FALSE)
# wd <- getwd()
# 
# # Iterate through entities to convert each to NetCDF format
# for (entitynumber in seq_along(config$metadata$content$entities)) {
#   entity <- config$metadata$content$entities[[entitynumber]]
#   dataset_pid <- entity$identifiers[["id"]]
#   setwd(file.path(tunaatlas_qa_global_datasets_catch_path, "entities", dataset_pid))
#   action <- entity$data$actions[[1]]
#   convert_to_netcdf(action, config, entity, uploadgoogledrive = FALSE)
# }
# 
# setwd(wd)
# 
# 
# # DOI Processing
# # --------------
# 
# # Process entities for DOI
# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_actions/process_entities_for_DOI.R")
# process_entities_for_DOI(tunaatlas_qa_global_datasets_catch_path, "~/firms-gta/geoflow-tunaatlas/jobs/processed_entities_for_DOI")
# 
# 
# # Irregular Data Extraction
# # -------------------------
# 
# # Extract data irregularities between nominal and georeferenced datasets
# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/strata_in_georef_but_not_in_nominal_report_launching.R")
# upgraded_nominal <- strata_in_georef_but_not_in_nominal_report_launching("~/blue-cloud-dataspace/GlobalFisheriesAtlas/data", connectionDB = con)
# 
# 
# # CPUE Analysis
# # -------------
# 
# # Perform analysis on Catch Per Unit Effort (CPUE)
# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/strata_with_catches_without_effort.R")
# CPUE <- strata_with_catches_without_effort(tunaatlas_qa_global_datasets_catch_path, connectionDB = con)
# 
# # Filter CPUE data for cases with catch but no effort, and vice versa
# catch_without_effort <- CPUE %>% dplyr::filter(is.na(measurement_value_effort) | measurement_value_effort == 0 & measurement_value_catch != 0)
# effort_without_catch <- CPUE %>% dplyr::filter(is.na(measurement_value_catch) | measurement_value_catch == 0 & measurement_value_effort != 0)
# 
# 
# # Deployment to GeoServer, GeoNetwork, and Zenodo
# # -----------------------------------------------
# 
# # Initialize and deploy datasets on geoserver
# tunaatlas_qa_services <- initWorkflow("tunaatlas_qa_services.json")
