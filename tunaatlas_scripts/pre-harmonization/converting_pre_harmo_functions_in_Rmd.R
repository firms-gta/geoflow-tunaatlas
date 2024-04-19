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
source("~/firms-gta/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/extract_urls_from_column.R")
source("~/firms-gta/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/downloading_pre_harmo_gsheet.R")
path_to_scripts <- here::here("tunaatlas_scripts/pre-harmonization")
scripts = file.path(path_to_scripts, basename(list_github_paths))
scripts <- gsub(".R_", ".R", scripts)

# scripts <- list_github_paths listightub path from downloading pre harmo sheet

# Fonction pour nettoyer chaque script enlever les appels à geoflow, la gestion des codelist
clean_script <- function(script_path) {
  # Lire le contenu du script
  lines <- readLines(script_path)
  
  # Enlever la définition de la fonction et les accolades ouvrantes qui l'encadrent
  lines <- str_remove_all(lines, "^function.*\\{")
  
  # Enlever uniquement la dernière accolade fermante
  # Inverser les lignes pour traiter la dernière occurrence comme la première
  lines_reversed <- rev(lines)
  # Supprimer la première occurrence de l'accolade fermante rencontrée
  lines_reversed[which(str_detect(lines_reversed, "\\}$"))[1]] <- str_remove(lines_reversed[which(str_detect(lines_reversed, "\\}$"))[1]], "\\}$")
  # Rétablir l'ordre original des lignes
  lines <- rev(lines_reversed)
  #enlever les espaces devant le #
  lines <- gsub("^\\s*#", "#", lines)
  # Enlever les lignes spécifiques à `entity`, `action`,  `config` et codelists
  lines <- lines[!str_detect(lines, "entity|action|config|codelists|geoflow|eblondel")]
  
  # Retourner le code nettoyé qui sera écrit dans un autre fichier avant d'être transformé en Rmd 
  #(il vaut mieux passer par un fichier R je pense mais on peut s'en détacher en rentrant directement le texte dans knitr::spin )
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

