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
clean_script <- function(script_path) {
  content_lines <- readLines(script_path)
  require(httr)
  # uncomment thos lines to create the Global action for geoflow to be done
  # response <- GET(script_path)
  # content_lines <- readLines(textConnection(content(response, "text")))  
  # Remove function definitions and the opening braces following them
  lines <- str_remove_all(content_lines, "^function.*\\{")
  
  # Reverse the array to handle the closing brace
  lines_reversed <- rev(lines)
  # Remove the first occurrence of the closing brace
  lines_reversed[which(str_detect(lines_reversed, "\\}$"))[1]] <- str_remove(lines_reversed[which(str_detect(lines_reversed, "\\}$"))[1]], "\\}$")
  # Restore the original order of lines
  lines <- rev(lines_reversed)
  
  # Remove spaces in front of the hash symbol for comments
  lines <- gsub("^\\s*#", "#", lines)
  # Remove lines containing specific keywords
  lines <- lines[!str_detect(lines, "entity|action|config|codelists|@geoflow|@eblondel|current")]

  # Replace 'output_name_dataset' assignments with a specific fixed string
  lines <- str_replace_all(lines, "output_name_dataset\\s*<-.*", 'output_name_dataset <- "Dataset_harmonized.csv"')
  
  # Update write.csv lines and add a line for georef_dataset assignment
  for (i in seq_along(lines)) {
    if (str_detect(lines[i], "write\\.csv")) {
      dataset_name <- str_extract(lines[i], "(?<=write\\.csv\\()(.*?)(?=,)")
      georef_line <- sprintf("georef_dataset <- %s", dataset_name)
      lines <- append(lines, georef_line, after = i)
    }
  }  
  
  
  if(grepl("effort",  script_path)){
    line_fact <- 'fact <- "effort"' 
  } else { line_fact <- 'fact <- "catch"'}
  
  lines_add <- ''
  
  if(sum(grepl("path_to_raw_dataset_catch",  as.character(lines)))>0){
    lines_add <- c(lines_add, 'path_to_raw_dataset_catch <- ') 
    lines_add <- c(lines_add, 'path_to_raw_dataset_effort <- ') 
    
  } else if(sum(grepl("path_to_raw_dataset",  lines))) {
    lines_add <- c(lines_add, 'path_to_raw_dataset <- ') 
  }

    # Add extra lines for data processing 
    
    intro_lines <- c(
      "# # Introduction",
      "# ",
      "# This R Markdown document is designed to transform data that is not in CWP format into CWP format.",
      "# Initially, it changes the format of the data; subsequently, it maps the data to adhere to CWP standards.",
      "# This markdown is created from a function so the documentation keep the format of roxygen2 skeleton",
      "# A summary of the mapping process is provided. Please specify `path_to_raw_dataset`, `path_to_raw_dataset_catch` or/and `path_to_raw_dataset_effort`, the historical name as received from tRFMOs is specified in the markdown",
      "# Additional operations are performed next to verify other aspects of the data, such as the consistency of the geolocation, the values, and the reported catches in numbers and tons.",
      "# If you are interested in further details, the results and codes are available for review."
    )
    
    extra_lines <- c(
      "# Load pre-harmonization scripts and apply mappings",
      "download.file('https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developpement/tunaatlas_scripts/pre-harmonization/map_codelists_no_DB.R', destfile = 'local_map_codelists_no_DB.R')",
      "source('local_map_codelists_no_DB.R')",
      line_fact,
      "mapping_codelist <- map_codelists_no_DB(fact, mapping_dataset = 'https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv', dataset_to_map = georef_dataset, mapping_keep_src_code = FALSE, summary_mapping = TRUE, source_authority_to_map = c('IATTC', 'CCSBT', 'WCPFC'))",
      "# Handle unmapped values and save the results",
      "georef_dataset <- mapping_codelist$dataset_mapped %>% dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == 'UNK', 'NEI', fishing_fleet), species = ifelse(species == 'UNK', 'MZZ', species), gear_type = ifelse(gear_type == 'UNK', '99.9', gear_type))",
      "fwrite(mapping_codelist$recap_mapping, 'recap_mapping.csv')",
      "fwrite(mapping_codelist$not_mapped_total, 'not_mapped_total.csv')",
      "# Display the first few rows of the mapping summaries",
      "print(head(mapping_codelist$recap_mapping))",
      "print(head(mapping_codelist$not_mapped_total))"
    )
    
    lines <- c(intro_lines,lines_add,  lines, extra_lines)
    
  
  
  return(lines)
}

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
config_catch <- initWorkflow(here::here("All_raw_data_georef.json"))
# list_url_catch <- rewrite_scripts(config_catch)
config_effort <- initWorkflow(here::here("All_raw_data_georef_effort.json"))
config_nominal <- initWorkflow(here::here("Raw_nominal_catch.json"))
# list_url_effort <- rewrite_scripts(config_effort)
list_url_effort <- url_function(config_effort)
list_url_nominal_catch <- url_function(config_nominal)
list_url_catch <- url_function(config_catch)
path_to_scripts <- here::here("tunaatlas_scripts/pre-harmonization")
list_url <- c(list_url_catch, list_url_effort, list_url_nominal_catch)
scripts = file.path(path_to_scripts, basename(list_url))
scripts <- gsub(".R_", ".R", scripts)

# function(config, entity, action){}

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


# scripts <- "~/firms-gta/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/Tidying_and_mapping_data.R"
          
