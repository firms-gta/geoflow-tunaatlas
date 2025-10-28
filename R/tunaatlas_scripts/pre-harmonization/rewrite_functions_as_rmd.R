# Chargement des bibliothèques nécessaires
library(fs)
library(dplyr)
library(readxl)
library(writexl)
library(purrr)
library(here)
library(rmarkdown)

# Fonction pour tronquer les fichiers CSV et XLSX
truncate_files <- function(root_dir) {
  # Fonction pour tronquer un fichier CSV
  truncate_csv <- function(file_path) {
    data <- read.csv(file_path)
    if ("Record" %in% colnames(data)) {
      data <- data %>% arrange(Record) %>% head(10)
    } else {
      data <- head(data, 10)
    }
    write.csv(data, file_path, row.names = FALSE)
    message(paste("Truncated CSV file:", file_path))
  }
  
  # Fonction pour tronquer un fichier XLSX
  truncate_xlsx <- function(file_path) {
    sheets <- excel_sheets(file_path)
    new_data <- lapply(sheets, function(sheet) {
      data <- read_excel(file_path, sheet = sheet)
      if ("Record" %in% colnames(data)) {
        data <- data %>% arrange(Record) %>% head(10)
      } else {
        data <- head(data, 10)
      }
      data
    })
    names(new_data) <- sheets
    writexl::write_xlsx(new_data, file_path)
    message(paste("Truncated XLSX file:", file_path))
  }
  
  # Liste de tous les fichiers CSV et XLSX dans le dossier et les sous-dossiers
  csv_files <- dir_ls(path = root_dir, recurse = TRUE, glob = "*.csv")
  xlsx_files <- dir_ls(path = root_dir, recurse = TRUE, glob = "*.xlsx")
  
  # Appliquer la troncature aux fichiers CSV et XLSX
  purrr::walk(csv_files, truncate_csv)
  purrr::walk(xlsx_files, truncate_xlsx)
  
  message("All files have been truncated.")
}

# Fonction principale pour réécrire les fichiers RMarkdown et générer les fichiers HTML
rewrite_functions_as_rmd <- function(source_path) {
  # Chemin du dossier de destination
  destination_path <- "~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization"
  
  # Fonction pour déplacer les fichiers vers le dossier correspondant
  move_files <- function(file_path) {
    file_name <- path_file(file_path)
    trfmo <- str_match(file_name, "(iattc|iccat|ccsbt|wcpfc|iotc)")[,2]
    type <- if (str_detect(basename(file_name), "effort")) "effort" else if (str_detect(basename(file_name), "nominal")) "nominal" else "catch"
    full_destination_path <- file.path(destination_path, trfmo, type, "data", file_name)
    
    if (!dir_exists(dirname(full_destination_path))) {
      dir_create(dirname(full_destination_path))
    }
    
    file_copy(file_path, full_destination_path, overwrite = TRUE)
    message(paste("Moved:", file_name, "to", full_destination_path))
  }
  
  invalid_files <- dir_ls(path = source_path, recurse = TRUE, glob = "*invalid_data.csv")
  purrr::walk(invalid_files, move_files)
  
  # Fonction pour mettre à jour les chemins dans les fichiers Rmd et ajouter le titre
  update_rmd_paths <- function(rmd_file) {
    lines <- readLines(rmd_file)
    title_line <- lines[grep("armon", lines)][1]
    title <- sub("^'\\s*", "", title_line)
    
    yaml_header <- c(
      "---",
      paste0("title: \"", title, "\""),
      "output: ",
      "  html_document: ",
      "    code_folding: hide",
      "---",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)",
      "```"
    )
    
    trfmo <- str_match(path_file(rmd_file), "(iattc|iccat|ccsbt|wcpfc|iotc)")[,2]
    type <- if (str_detect(basename(rmd_file), "effort")) "effort" else if (str_detect(basename(rmd_file), "nominal")) "nominal" else "catch"
    rmd_destination_path <- file.path(destination_path, trfmo, type)
    
    if (!dir_exists(rmd_destination_path)) {
      dir_create(rmd_destination_path, recursive = TRUE)
    }
    
    new_rmd_path <- file.path(rmd_destination_path, path_file(rmd_file))
    data_path <- file.path(dirname(rmd_file), "data")
    
    csv_files <- dir_ls(path = data_path, glob = "*.csv")
    xlsx_files <- dir_ls(path = data_path, glob = "*.xlsx")
    data_files <- c(csv_files, xlsx_files)
    data_files <- data_files[!str_detect(data_files, "codelists")]
    data_files <- data_files[!str_detect(data_files, "mapped")]
    data_files <- data_files[!str_detect(data_files, "harmonized")]
    
    if (!is.na(trfmo) && type == "nominal" && trfmo == "wcpfc") {
      lines <- gsub("path_to_raw_dataset <-.*", "path_to_raw_dataset1 <- here::here('tunaatlas_scripts/pre-harmonization', 'wcpfc', 'nominal', 'data', 'XLS_WCPFC.csv') \n path_to_raw_dataset2 <- here::here('tunaatlas_scripts/pre-harmonization', 'wcpfc', 'nominal', 'data', 'XLS_WCPO.csv')", lines)
    } else {
      if (length(data_files) >= 1) {
        lines <- gsub("path_to_raw_dataset <-.*", paste0("path_to_raw_dataset <- here::here('tunaatlas_scripts/pre-harmonization', '", trfmo, "', '", type, "', 'data', '", path_file(data_files[1]), "')"), lines)
      }
      if (any(grepl("path_to_raw_dataset_catch <-", lines)) && length(data_files) >= 1) {
        lines <- gsub("path_to_raw_dataset_catch <-.*", paste0("path_to_raw_dataset_catch <- here::here('tunaatlas_scripts/pre-harmonization', '", trfmo, "', '", type, "', 'data', '", path_file(data_files[1]), "')"), lines)
      }
      if (any(grepl("path_to_raw_dataset_effort <-", lines)) && length(data_files) >= 2) {
        lines <- gsub("path_to_raw_dataset_effort <-.*", paste0("path_to_raw_dataset_effort <- here::here('tunaatlas_scripts/pre-harmonization', '", trfmo, "', '", type, "', 'data', '", path_file(data_files[2]), "')"), lines)
      }
    }
    
    # Ajouter un espace après chaque # suivi directement d'une lettre
    lines <- gsub("(^#)(\\S)", "\\1 \\2", lines)
    
    # Supprimer les lignes contenant uniquement des tirets
    lines <- lines[!grepl("^[-]+$", lines)]
    
    new_data_path <- file.path(rmd_destination_path, "data")
    if (!dir_exists(new_data_path)) {
      dir_create(new_data_path)
    }
    
    copy_files <- function(file_path) {
      if (str_detect(file_path, "\\.csv$")) {
        data <- read.csv(file_path)
        new_file_path <- file.path(new_data_path, path_file(file_path))
        write.csv(data, new_file_path, row.names = FALSE)
      } else if (str_detect(file_path, "\\.xlsx$")) {
        sheets <- excel_sheets(file_path)
        new_data <- lapply(sheets, function(sheet) {
          read_excel(file_path, sheet = sheet)
        })
        names(new_data) <- sheets
        new_file_path <- file.path(new_data_path, path_file(file_path))
        writexl::write_xlsx(new_data, new_file_path)
      }
    }
    
    purrr::walk(data_files, copy_files)
    
    writeLines(c(yaml_header, "", lines), new_rmd_path)
    message(paste("Updated and moved Rmd:", path_file(rmd_file)))
    
    # Génération des fichiers HTML
    if (basename(new_rmd_path) != "data_cwp_format_Report.Rmd") {
      tryCatch({
        rmarkdown::render(new_rmd_path, output_format = "html_document", quiet = TRUE)
        message(paste("Markdown created:", new_rmd_path))
      }, error = function(e) {
        message(paste("Error rendering:", new_rmd_path, ":", e$message))
      })
    }
  }
  
  rmd_files <- dir_ls(path = source_path, recurse = TRUE, glob = "*.Rmd")
  purrr::walk(rmd_files, update_rmd_paths)
  message("All operations completed.")
}

# Fonction pour exécuter rewrite_functions_as_rmd avec gestion des erreurs
safe_rewrite_functions_as_rmd <- function(source_path) {
  tryCatch({
    rewrite_functions_as_rmd(source_path)
  }, error = function(e) {
    message(sprintf("An error occurred while processing '%s': %s", source_path, e$message))
  })
}


# Fonction pour supprimer les fichiers spécifiques
remove_specific_files <- function(root_dir) {
  files_to_remove <- c("CWP_dataset.csv", "Dataset_harmonized.csv", "not_mapped_total.csv", "recap_mapping.csv", "local_map_codelists_no_DB.R")
  
  # Rechercher les fichiers spécifiés dans le répertoire racine et ses sous-dossiers
  all_files <- dir_ls(path = root_dir, recurse = TRUE, type = "file")
  
  # Filtrer les fichiers à supprimer
  files_found <- all_files[path_file(all_files) %in% files_to_remove]
  
  # Vérification des fichiers trouvés avant suppression
  if (length(files_found) > 0) {
    file_delete(files_found)
    message("Specific files have been removed.")
  } else {
    message("No specific files found to remove.")
  }
}

# Appel à la fonction pour supprimer les fichiers spécifiques
remove_specific_files(here::here("tunaatlas_scripts/pre-harmonization"))


# Troncature des fichiers CSV et XLSX après la génération des fichiers HTML
truncate_files("~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization")
