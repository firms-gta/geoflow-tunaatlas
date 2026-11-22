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

source("workflow/launchers/GTA_2026_creation.R")
cat(format(Sys.time()), "- GTA_2026_creation.R sourcé\n")
source("workflow/launchers/workflow_helpers.R")
cat(format(Sys.time()), "- helpers sourcés, lancement du workflow\n")
# Settings: the single .env at the repository root (template: env.example)
if (!file.exists(here::here(".env"))) {
  stop("No .env at the repository root: copy env.example to .env and fill it in.",
       call. = FALSE)
}
dotenv::load_dot_env(file = here::here(".env"))
source(here::here("docker/patches/patch-zen4r-upload-error.R"))

tunaatlas_qa_dbmodel_path <- run_geoflow_workflow(
  file = here::here("config/geoflow/services.json"),
  requires_db = TRUE,
  rename_suffix = "services"
)
