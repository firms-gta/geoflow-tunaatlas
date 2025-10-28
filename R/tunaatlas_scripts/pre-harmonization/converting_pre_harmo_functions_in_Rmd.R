#' Clean R Scripts from Specific Patterns
#'
#' This script reads an R path to several script and performs several cleaning operations:
#' removes function definitions, braces encapsulating the function body, and lines
#' containing specific keywords (entity, action, config, codelists). It is designed
#' to prepare scripts for conversion into R Markdown format, removing elements
#' specific to the `geoflow` framework and other non-essential components.
#'
#' @param script_path The path to the R script to be cleaned.
#' @return A character vector containing the cleaned lines of the script, which
#' can then be written to a new file or directly processed further.
#' @examples
#' clean_script("~/path/to/script.R")
#' @export
#'
#' @importFrom stringr str_detect str_remove str_remove_all
#' @importFrom fs dir_ls
#' 
#' 
require(stringr)
require(geoflow)


# rewrite_scripts

url_function <- function(config){
  list_url <-c()
  for (entitynumber in 1:length(config$metadata$content$entities)){
    entity <- config$metadata$content$entities[[entitynumber]]
    
    action <- entity$data$actions[[1]]
    script_path <- action$script
    list_url <- c(list_url, script_path)
  }
  return(list_url)
}

# Fonction pour déterminer le dossier de la tRFMO
get_trfmo_folder <- function(filename) {
  trfmos <- c("iotc", "iccat", "iattc", "ccsbt", "wcpfc")
  for(trfmo in trfmos) {
    if(str_detect(filename, trfmo)) {
      name <- trfmo
      if(str_detect(filename, "effort")) {
        name <- file.path(name, "effort")
      } else {
        name <- file.path(name, "catch")
      }
      return(name)
    }
    

  }
  return("Other")
}


# Boucle pour nettoyer et sauvegarder les scripts 
config_catch <- initWorkflow(here::here("config/All_raw_data_georef.json"))
# list_url_catch <- rewrite_scripts(config_catch)
config_effort <- initWorkflow(here::here("config/All_raw_data_georef_effort.json"))
config_nominal <- initWorkflow(here::here("config/Raw_nominal_catch.json"))
# list_url_effort <- rewrite_scripts(config_effort)
list_url_effort <- url_function(config_effort)
list_url_nominal_catch <- url_function(config_nominal)
list_url_catch <- url_function(config_catch)
path_to_scripts <- here::here("tunaatlas_scripts/pre-harmonization")
list_url <- c(list_url_catch, list_url_effort, list_url_nominal_catch)
scripts = file.path(path_to_scripts, basename(list_url))
scripts <- gsub(".R_", ".R", scripts)


for(script in scripts) {
  tryCatch({
    cleaned_code <- clean_script(script)
    trfmo_folder <- get_trfmo_folder(basename(script))
    trfmo_path <- file.path(dirname(script), trfmo_folder)
    
    # Créer le dossier s'il n'existe pas
    if(!dir.exists(trfmo_path)) {
      dir.create(trfmo_path, recursive = TRUE)
    }
    
    new_filename <- str_replace(basename(script), ".R", "_cleaned.R")
    new_filepath <- file.path(trfmo_path, new_filename)
    writeLines(cleaned_code, new_filepath)
    knitr::spin(new_filepath,knit =FALSE, 
                doc = "^#\\s*|^#+'[ ]?",
                precious = TRUE)
    file.remove(new_filepath)
    
  }, error = function(e) {
    cat("An error occurred in processing script", basename(script), ":", e$message, "\n")
  })
}


# scripts <- "~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization/Tidying_and_mapping_data.R"
          
