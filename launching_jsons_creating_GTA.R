
# Load 'renv' for project-specific environments
# if (!require("renv")) install.packages("renv")
library(renv)
# Activate the project environment (if using project-specific libraries)
# renv::activate()
# Restore the project library (if using renv)
renv::restore()

# Define all required packages (excluding 'base' and 'utils' as they are always available)
required_packages <- c(
  "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor", 
  "dotenv", "data.table", "here", "xfun", "RPostgreSQL", "RPostgres", "DBI", 
  "rpostgis", "terra", "sf", "RSQLite", "webshot", "usethis", "ows4R", "sp", 
  "flextable", "dplyr", "stringr", "tibble", "bookdown", "knitr", 
  "purrr", "readxl", "odbc", "rlang", "kableExtra", "tidyr", "ggplot2", 
  "stats", "RColorBrewer", "cowplot", "tmap", "curl", "officer", 
  "gdata", "R3port", "reshape2", "tools", "plogr", "futile.logger"
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


executeAndRename <- function(executed_file, suffix) {
  
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

running_time_of_workflow <- function(folder){
  # Get the last modified times of the files
  json_time <- file.info(file.path(folder, "job.json"))$mtime
  txt_time <- file.info(file.path(folder, "job-logs.txt"))$mtime
  
  # Calculate the difference
  time_difference <- txt_time - json_time
  
  return(time_difference)
}
stop("Stop")
# First step is creation of the database model and loading of the codelist (around 5 minutes)
db_model <- executeWorkflow(here("tunaatlas_qa_dbmodel+codelists.json")) 
db_model <- executeAndRename(db_model, "_db_model")
running_time_of_workflow(db_model)

# Second step is the loading of the mappings (around 1.2 minutes)
mappings <- executeWorkflow(here("tunaatlas_qa_mappings.json"))
mappings <- executeAndRename(mappings, "_mappings")
running_time_of_workflow(mappings)

# Third step is pre-harmonizing the datasets provide by tRFMOs: This step is divided in 3 
# substep depending on the type of the data:

## Nominal data: These datasets are mandatory to create the georeferenced dataset level 2. For level 0 or 1 they are not mandatory time around 2.7 minutes
# Around 2.7 minutes
raw_nominal_catch <- executeWorkflow(here::here("Raw_nominal_catch.json"))
raw_nominal_catch <- executeAndRename(raw_nominal_catch, "_raw_nominal_catch")
running_time_of_workflow(raw_nominal_catch)


## Georeferenced catch: These datasets contains catch AND EFFORT FOR SOME DATA as effort are used to raise catch data for level 0 to 2
# Around 1.2 hours
raw_data_georef <- executeWorkflow(here::here("All_raw_data_georef.json"))
raw_data_georef <- executeAndRename(raw_data_georef, "_raw_data_georef")
running_time_of_workflow(raw_data_georef)

## Goereferenced effort: These datasets are used to create the georeferenced effort
# Around 30 minutes
raw_data_georef_effort <- executeWorkflow(here::here("All_raw_data_georef_effort.json"))
raw_data_georef_effort <- executeAndRename(raw_data_georef_effort, "_raw_data_georef_effort")
running_time_of_workflow(raw_data_georef_effort)

source("~/firms-gta/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/rewrite_functions_as_rmd.R")
safe_rewrite_functions_as_rmd <- function(source_path) {
  tryCatch({
    rewrite_functions_as_rmd(source_path)
  }, error = function(e) {
    message(sprintf("Error processing %s: %s", source_path, e$message))
  })
}

# Appels aux fonctions avec gestion des erreurs
safe_rewrite_functions_as_rmd(raw_nominal_catch)
safe_rewrite_functions_as_rmd(raw_data_georef)
safe_rewrite_functions_as_rmd(raw_data_georef_effort)

## Summarising the invalid data for all the datasets pre-harmonized
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
config <- initWorkflow(here::here("All_raw_data_georef.json"), handleMetadata = FALSE)
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
time_Summarising_invalid_data <- system.time({
  Summarising_invalid_data(raw_data_georef, connectionDB = con)
})
#Around 1 minute
time_Summarising_invalid_data_georef <- system.time({
  Summarising_invalid_data(raw_data_georef_effort, connectionDB = con)
})

Summarising_invalid_data('~/firms-gta/geoflow-tunaatlas/jobs/20240430091226_raw_nominal_catch', connectionDB = con)

# Around 50 seconds
## These two lines of codes creates a recap for each entity of the irregularities of the data for the datasets. 
# They also creates a report summarising the irregular data for each entity so it is easier to target them


# Create 5 datasets catch and effort. These entities are the final one published on zenodo. 

tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
tunaatlas_qa_global_datasets_catch_path <- executeAndRename(tunaatlas_qa_global_datasets_catch_path, "_global_datasets_level1_2")
### TODO add create_materialized_view_for_shiny_apps.R in the end of the workflow action on end

running_time_of_workflow(tunaatlas_qa_global_datasets_catch_path)
create_materialized_view <- ""

# uncomment the follwoing lines to go the shared path for analysis
# tunaatlas_qa_global_datasets_catch_path <- "~/blue-cloud-dataspace/GlobalFisheriesAtlas/data"

## Recapitulation of all the treatment done for each final dataset, these allows the recap of each step to ensure comprehension of the impact of each treatment
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/Summarising_step.R")
source(here::here("Analysis_markdown/functions/Summarising_step.R"))
config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
unlink(config$job, recursive = TRUE)
con <- config$software$output$dbi
#removed of Sumamrising step required_packages <- c("webshot","here", "usethis","ows4R","sp", "data.table", "flextable", "readtext", "sf", "dplyr", "stringr", "tibble",
#                        "bookdown", "knitr", "purrr", "readxl", "base", "remotes", "utils", "DBI", 
#                        "odbc", "rlang", "kableExtra", "readr", "tidyr", "ggplot2", "stats", "RColorBrewer", 
#                        "cowplot", "tmap", "RPostgreSQL", "curl", "officer", "gdata", "tidyr", "knitr", "tmap"
# )

Summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, config  =config)



# 
# `2024-08-28_11:12:03nominal_inferior_to_georeferenced`$GRIDTYPE <- "GRIDTYPE"
# a <- comprehensive_cwp_dataframe_analysis(parameter_init = `2024-08-28_11:12:03nominal_inferior_to_georeferenced`,
# unique_analyse = TRUE, print_map = FALSE, removemap = TRUE)
# source("~/firms-gta/geoflow-tunaatlas/comparing_conversion_factors.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/compare_georef_nominal.R")
results <- compare_georef_nominal(georeferenced, global_nominal_catch_firms_level0, connectionDB = con)
saveRDS(results, "data/resultsonallthegeorefsuptonom.rds")
## Netcdf creation (24h for level 2). This step is to create a netcdf file of the created data. It takes a very long time but creates a very light and comprehensive dataset
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_actions/convert_to_netcdf.R")
entity_dirs <- list.dirs(file.path(tunaatlas_qa_global_datasets_catch_path, "entities"), full.names = TRUE, recursive = FALSE)
wd <- getwd()
# tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
# tunaatlas_qa_global_datasets_catch_path <- executeAndRename(tunaatlas_qa_global_datasets_catch_path, "_global_datasets_level1_2")

config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
unlink(config$job, recursive = TRUE)


for (entitynumber in 1:length(config$metadata$content$entities)){
  entity <- config$metadata$content$entities[[entitynumber]]
  dataset_pid <- entity$identifiers[["id"]]
  setwd(file.path(tunaatlas_qa_global_datasets_catch_path,"entities", dataset_pid))
  action <- entity$data$actions[[1]]
  convert_to_netcdf(action, config, entity, uploadgoogledrive = FALSE)
} #could also be in global action but keep in mind it is very long

setwd(wd)

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_actions/process_entities_for_DOI.R")
process_entities_for_DOI(tunaatlas_qa_global_datasets_catch_path, "~/firms-gta/geoflow-tunaatlas/jobs/processed_entities_for_DOI")

# Pakcages for markdown

# These step is to be done once nominal and georeferenced data are created. It extract irregular data that is :
# - Existing on the georeferenced dataset but not in the nominal
# - Superior in the georeferenced dataset than in the nominal
# The strata_in_georef_but_not_in_nominal_report_launching function return several html files for each tRFMO and 
# for several stratas used to inspect the data (more details in the report)

# This function also return an upgraded_nominal dataset which is the nominal dataset raised from the georeferenced data
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/strata_in_georef_but_not_in_nominal_report_launching.R")

upgraded_nominal <- strata_in_georef_but_not_in_nominal_report_launching("~/blue-cloud-dataspace/GlobalFisheriesAtlas/data",
                                                                         connectionDB = con)
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/strata_with_catches_without_effort.R")
CPUE <- strata_with_catches_without_effort(tunaatlas_qa_global_datasets_catch_path,
                                           connectionDB = con)

catch_without_effort <- CPUE %>% dplyr::filter(((is.na(measurement_value_effort) | measurement_value_effort == 0)) & measurement_value_catch != 0)
effort_without_catch <- CPUE %>% dplyr::filter(((is.na(measurement_value_catch) | measurement_value_catch == 0)) & measurement_value_effort != 0)

# Check on CPUE data for georeferenced in case some catch are not displayed with any effort
global_catch_firms_level0_public <- read_csv(file.path(tunaatlas_qa_global_datasets_catch_path,"entities/global_catch_firms_level0/data/global_catch_firms_level0_public.csv"))
cwp_catch <- unique(global_catch_firms_level0_public$geographic_identifier)


# Putting dataset on geoserver, geonetwork and zenodo #For now zenodo does not work due to issue with api
tunaatlas_qa_services <- initWorkflow("tunaatlas_qa_services.json")

# Enriching data with copernicus data
all_files <- list.files(getwd(), pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
netcdf_file_to_enrich <- all_files[!grepl("nominal", all_files)]

