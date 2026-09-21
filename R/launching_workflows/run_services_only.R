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

cat(format(Sys.time()), "- packages charges\n")

source("R/launching_workflows/GTA_2026_creation.R")
cat(format(Sys.time()), "- GTA_2026_creation.R sourcé\n")
source("R/launching_workflows/workflow_helpers.R")
cat(format(Sys.time()), "- helpers sourcés, lancement du workflow\n")
docker_env_file <- "docker_local.env.compose"

if (file.exists(here::here(docker_env_file))) {
  tryCatch(
    dotenv::load_dot_env(file = here::here(docker_env_file)),
    error = function(e) message("No environment file loaded: ", e$message)
  )
} else {
  message("No '", docker_env_file, "' found, skipping DB-specific env file.")
}
source(here::here("compose/patches/patch-zen4r-upload-error.R"))
if(file.exists(here::here("zenodo_secrets.env"))){
  
  tryCatch(
    dotenv::load_dot_env(file = here::here("zenodo_secrets.env")),
    error = function(e) message("No environment file loaded: ", e$message)
  )
}

tunaatlas_qa_dbmodel_path <- execute_workflow_maybe_upload(
  file = here::here("config/tunaatlas_qa_services.json"),
  requires_db = TRUE,
  rename_suffix = "services"
)
