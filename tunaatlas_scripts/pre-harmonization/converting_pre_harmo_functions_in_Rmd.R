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
# Définir le chemin vers les scripts R


# Function to rewrite R scripts by adding comments when 'entity$data$source' is mentioned
rewrite_scripts <- function(config) {
  
  list_url <- c()
  for (entitynumber in 1:length(config$metadata$content$entities)){
    entity <- config$metadata$content$entities[[entitynumber]]

    action <- entity$data$actions[[1]]
    script_path <- action$script
    list_url <- c(list_url, script_path)
    print(list_url)
    # List all R files in the specified directory
    file_path <- file.path(here::here("tunaatlas_scripts/pre-harmonization",basename(script_path)))
    history_comments_added <- FALSE
    author_added = FALSE
    # Loop through each file
    print(file_path)
    # Read the content of the file
    lines <- readLines(file_path, warn = FALSE)
    
    # Initialize a vector to store the new lines
    new_lines <- character()
    
    # Check for existing author line
    if (sum(grepl("bastien.grasset", as.character(lines)))>1) {
      author_added = TRUE
    }
    
    
    # Loop through each line of the file
    for (i in seq_along(lines)) {
      print(i)
      # Add the current line to the new vector of lines
      new_lines <- c(new_lines, lines[i])
      
      # Check if the line contains 'entity$data$source'
      if (grepl("entity\\$data\\$source", lines[i])) {
        # Extract any potential source index if it exists
        matches <- regmatches(lines[i], regexec("entity\\$data\\$source\\[\\[(\\d+)\\]\\]", lines[i]))
        if (length(matches[[1]]) > 1) {  # Ensure there's a match
          index <- matches[[1]][2]  # Corrected to get the captured group, not the entire match
          comment_line = sprintf("# Historical name for the dataset at source  %s, if multiple, this means this function is used for several dataset, keep the same order to match data", eval(parse(text = paste0("entity$data$source[[", index, "]]"))))
          new_lines <- c(new_lines, comment_line)
        }
      }
      
      # Add additional authors if Paul Taconet's email is mentioned
      if (grepl("paul.taconet@ird.fr", lines[i])) {
        if (!author_added) {
          author_line = "#' @author Bastien Grasset, IRD \\email{bastien.grasset@ird.fr}"
          new_lines <- c(new_lines, author_line)
        }
      }
    }
    
    # Write the new lines into the file, overwriting the original
    new_file_path <- file.path(here::here("tunaatlas_scripts/pre-harmonization/",str_replace(basename(file_path), ".R", ".R")))
    
    writeLines(new_lines, new_file_path)
  }
  return(list_url)
}
list_url <- rewrite_scripts(config)
# Example usage:
# Suppose your R scripts are stored in 'path/to/your/scripts'
# rewrite_scripts(config)


# source("~/firms-gta/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/extract_urls_from_column.R")
# source("~/firms-gta/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/downloading_pre_harmo_gsheet.R")

path_to_scripts <- here::here("tunaatlas_scripts/pre-harmonization")
scripts = file.path(path_to_scripts, basename(list_url))
scripts <- gsub(".R_", ".R", scripts)

# scripts <- list_github_paths listightub path from downloading pre harmo sheet

# Fonction pour nettoyer chaque script enlever les appels à geoflow, la gestion des codelist
clean_script <- function(script_path, add_extra_lines = TRUE) {
  # Read the script file line by line
  lines <- readLines(script_path)
  
  # Remove function definitions and the opening braces following them
  lines <- str_remove_all(lines, "^function.*\\{")
  
  # Reverse the array to handle the closing brace
  lines_reversed <- rev(lines)
  # Remove the first occurrence of the closing brace
  lines_reversed[which(str_detect(lines_reversed, "\\}$"))[1]] <- str_remove(lines_reversed[which(str_detect(lines_reversed, "\\}$"))[1]], "\\}$")
  # Restore the original order of lines
  lines <- rev(lines_reversed)
  
  # Remove spaces in front of the hash symbol for comments
  lines <- gsub("^\\s*#", "#", lines)
  # Remove lines containing specific keywords
  lines <- lines[!str_detect(lines, "entity|action|config|codelists|geoflow|eblondel")]
  
  # Add extra lines for data processing if specified
  if(add_extra_lines) {
    
    intro_lines <- c(
      "# # Introduction",
      "# This R Markdown document is designed to transform data that is not in CWP format into CWP format.",
      "# Initially, it changes the format of the data; subsequently, it maps the data to adhere to CWP standards.",
      "# A summary of the mapping process is provided. Please specify `path_to_raw_dataset`, the historical name as received from tRFMOs is specified",
      "# Additional operations are performed next to verify other aspects of the data, such as the consistency of the geolocation, the values, and the reported catches in numbers and tons.",
      "# If you are interested in further details, the results and codes are available for review."
    )
    
    extra_lines <- c(
      "# Load pre-harmonization scripts and apply mappings",
      "download.file('https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developpement/tunaatlas_scripts/pre-harmonization/map_codelists_no_DB.R', destfile = 'local_map_codelists_no_DB.R')",
      "source('local_map_codelists_no_DB.R')",
      "mapping_codelist <- map_codelists_no_DB(opts$fact, mapping_dataset = 'https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv', dataset_to_map = georef_dataset, mapping_keep_src_code = FALSE, summary_mapping = TRUE, source_authority_to_map = c('IATTC', 'CCSBT', 'WCPFC'))",
      "# Handle unmapped values and save the results",
      "georef_dataset <- mapping_codelist$dataset_mapped %>% dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == 'UNK', 'NEI', fishing_fleet), species = ifelse(species == 'UNK', 'MZZ', species), gear_type = ifelse(gear_type == 'UNK', '99.9', gear_type))",
      "fwrite(mapping_codelist$recap_mapping, 'data/recap_mapping.csv')",
      "fwrite(mapping_codelist$not_mapped_total, 'data/not_mapped_total.csv')",
      "# Display the first few rows of the mapping summaries",
      "print(head(mapping_codelist$recap_mapping))",
      "print(head(mapping_codelist$not_mapped_total))"
    )
    lines <- c(intro_lines, lines, extra_lines)
  }
  
  
  return(lines)
}


# Fonction pour déterminer le dossier de la tRFMO
get_trfmo_folder <- function(filename) {
  trfmos <- c("iotc", "iccat", "iattc", "ccsbt", "wcpfc")
  for(trfmo in trfmos) {
    if(str_detect(filename, trfmo)) {
      return(trfmo)
    }
  }
  return("Other")
}


# Boucle pour nettoyer et sauvegarder les scripts
for(script in scripts) {
  tryCatch({
    cleaned_code <- clean_script(script)
    trfmo_folder <- get_trfmo_folder(basename(script))
    trfmo_path <- file.path(dirname(script), trfmo_folder)
    
    # Créer le dossier s'il n'existe pas
    if(!dir.exists(trfmo_path)) {
      dir.create(trfmo_path)
    }
    
    new_filename <- str_replace(basename(script), ".R", "_cleaned.R")
    new_filepath <- file.path(trfmo_path, new_filename)
    writeLines(cleaned_code, new_filepath)
    knitr::spin(new_filepath,knit =FALSE, 
                doc = "^#\\s*",
                precious = TRUE)
    
  }, error = function(e) {
    cat("An error occurred in processing script", basename(script), ":", e$message, "\n")
  })
}

# scripts <- "~/firms-gta/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/Tidying_and_mapping_data.R"
