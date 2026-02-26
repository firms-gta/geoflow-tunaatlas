# truncate_files <- function(root_dir) {
#   # Fonction pour tronquer un fichier CSV
#   truncate_csv <- function(file_path) {
#     data <- read.csv(file_path)
#     if ("Record" %in% colnames(data)) {
#       data <- data %>% arrange(Record) %>% head(10)
#     } else {
#       data <- head(data, 10)
#     }
#     write.csv(data, file_path, row.names = FALSE)
#     message(paste("Truncated CSV file:", file_path))
#   }
#   
#   # Fonction pour tronquer un fichier XLSX
#   truncate_xlsx <- function(file_path) {
#     sheets <- excel_sheets(file_path)
#     new_data <- lapply(sheets, function(sheet) {
#       data <- read_excel(file_path, sheet = sheet)
#       if ("Record" %in% colnames(data)) {
#         data <- data %>% arrange(Record) %>% head(10)
#       } else {
#         data <- head(data, 10)
#       }
#       data
#     })
#     names(new_data) <- sheets
#     writexl::write_xlsx(new_data, file_path)
#     message(paste("Truncated XLSX file:", file_path))
#   }
#   
#   # Liste de tous les fichiers CSV et XLSX dans le dossier et les sous-dossiers
#   csv_files <- dir_ls(path = root_dir, recurse = TRUE, glob = "*.csv")
#   xlsx_files <- dir_ls(path = root_dir, recurse = TRUE, glob = "*.xlsx")
#   
#   # Appliquer la troncature aux fichiers CSV et XLSX
#   purrr::walk(csv_files, truncate_csv)
#   purrr::walk(xlsx_files, truncate_xlsx)
#   
#   message("All files have been truncated.")
# }


# Chargement des bibliothèques nécessaires
library(fs)
library(dplyr)
library(readxl)
library(writexl)
library(purrr)
library(here)
library(rmarkdown)
require(stringr)

# Fonction pour tronquer les fichiers CSV et XLSX

copy_prehamo_data_files <- function(source_path, destination_path = here::here("R/tunaatlas_scripts/pre-harmonization")) {
  
  move_files <- function(file_path, truncate = FALSE) {
    file_name <- fs::path_file(file_path)
    trfmo <- stringr::str_match(file_path, "(iattc|iccat|ccsbt|wcpfc|iotc)")[,2]
    type <- case_when(
      stringr::str_detect(file_path, stringr::regex("effort", ignore_case = TRUE)) ~ "effort",
      stringr::str_detect(file_path, stringr::regex("catch", ignore_case = TRUE)) ~ "catch",
      stringr::str_detect(file_path, stringr::regex("nominal", ignore_case = TRUE)) ~ "nominal",
      stringr::str_detect(file_path, stringr::regex("EF", ignore_case = TRUE)) ~ "effort",
      TRUE ~ "catch"
    )
    full_destination_path <- file.path(destination_path, trfmo, type, "data", file_name)
    
    if (!fs::dir_exists(dirname(full_destination_path))) {
      fs::dir_create(dirname(full_destination_path), recurse = TRUE)
    }
    
    if (truncate) {
      # mapped.csv -> 5 lignes
      x <- readr::read_csv(file_path, show_col_types = FALSE)
      readr::write_csv(utils::head(x, 5), full_destination_path)
      message("Copied ONLY 5 rows of: ", file_name, " -> ", full_destination_path)
    } else {
      fs::file_copy(file_path, full_destination_path, overwrite = TRUE)
      message("Copied: ", file_name, " -> ", full_destination_path)
    }
  }
  
  # # invalid -> complet
  # invalid_files <- fs::dir_ls(path = source_path, recurse = TRUE, glob = "*invalid_data.csv")
  # purrr::walk(invalid_files, ~ move_files(.x, truncate = FALSE)) on l'enleve on met pas sur github les invalid lines
  
  # mapped -> 5 lignes
  mapped_files <- fs::dir_ls(path = source_path, recurse = TRUE, glob = "*mapped.csv")
  purrr::walk(mapped_files, ~ move_files(.x, truncate = TRUE))
  
  invisible(list(mapped_files = mapped_files))
}

# Fonction principale pour réécrire les fichiers RMarkdown et générer les fichiers HTML
rewrite_functions_as_rmd <- function(source_path) {
  # Chemin du dossier de destination
  destination_path <- "~/firms-gta/geoflow-tunaatlas/R/tunaatlas_scripts/pre-harmonization"
  
  copy_prehamo_data_files(source_path)
  
  # Fonction pour mettre à jour les chemins dans les fichiers Rmd et ajouter le titre
  update_rmd_paths <- function(rmd_file) {
    lines <- readLines(rmd_file)
    title_line <- lines[grepl("Harmonize", lines)][1]
    
    # Fallback si jamais pas trouvé
    if (is.na(title_line)) {
      title_line <- lines[grepl("^\\s*#?'\\s*", lines)][1]
    }
    
    title <- sub("^\\s*#?'\\s*", "", title_line)
    
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
    data_files <- data_files[!str_detect(data_files, "removed_irregular_areas")]
    data_files <- data_files[!str_detect(data_files, "recap_mapping")]
    data_files <- data_files[!str_detect(data_files, "areas_in_land")]
    data_files <- data_files[!str_detect(data_files, "not_displayed_monthly")]
    if (!is.na(trfmo) && type == "nominal" && trfmo == "wcpfc") {
      lines <- gsub("path_to_raw_dataset <-.*", "path_to_raw_dataset1 <- here::here('R/tunaatlas_scripts/pre-harmonization', 'wcpfc', 'nominal', 'data', 'XLS_WCPFC_2025-11-27.csv') \n path_to_raw_dataset2 <- here::here('R/tunaatlas_scripts/pre-harmonization', 'wcpfc', 'nominal', 'data', 'XLS_WCPO_2025-11-27.csv')", lines)
    } else {
      if (length(data_files) >= 1) {
        lines <- gsub("path_to_raw_dataset <-.*", paste0("path_to_raw_dataset <- here::here('R/tunaatlas_scripts/pre-harmonization', '", trfmo, "', '", type, "', 'data', '", path_file(data_files[1]), "')"), lines)
      }
      if (any(grepl("path_to_raw_dataset_catch <-", lines)) && length(data_files) >= 1) {
        lines <- gsub("path_to_raw_dataset_catch <-.*", paste0("path_to_raw_dataset_catch <- here::here('R/tunaatlas_scripts/pre-harmonization', '", trfmo, "', '", type, "', 'data', '", path_file(data_files[1]), "')"), lines)
      }
      if (any(grepl("path_to_raw_dataset_effort <-", lines)) && length(data_files) >= 2) {
        lines <- gsub("path_to_raw_dataset_effort <-.*", paste0("path_to_raw_dataset_effort <- here::here('R/tunaatlas_scripts/pre-harmonization', '", trfmo, "', '", type, "', 'data', '", path_file(data_files[2]), "')"), lines)
      }
    }
    # Ajouter un espace après chaque # suivi directement d'une lettre
    lines <- gsub("(^#)(\\S)", "\\1 \\2", lines)
    
    # Supprimer les lignes contenant uniquement des tirets
    lines <- lines[!grepl("^[-]+$", lines)]
    
    drop_patterns <- c(
      "^\\s*base1\\s*<-\\s*tools::file_path_sans_ext\\(basename\\(filename1\\)\\)\\s*$",
      "^\\s*base2\\s*<-\\s*tools::file_path_sans_ext\\(basename\\(filename2\\)\\)\\s*$"
    )
    
    lines <- lines[!Reduce(`|`, lapply(drop_patterns, grepl, x = lines))]
    
    new_data_path <- file.path(rmd_destination_path, "data")
    if (!dir_exists(new_data_path)) {
      dir_create(new_data_path)
    }

    copy_files <- function(file_path, n = 10) {
      
      head_order_record <- function(df, n) {
        if ("Record" %in% names(df)) df <- dplyr::arrange(df, .data$Record)
        dplyr::slice_head(df, n = n)
      }
      
      new_file_path <- file.path(new_data_path, fs::path_file(file_path))
      
      if (stringr::str_detect(file_path, "\\.csv$")) {
        
        df <- readr::read_csv(file_path, show_col_types = FALSE)
        df <- head_order_record(df, n = n)
        readr::write_csv(df, new_file_path)
        
      } else if (stringr::str_detect(file_path, "\\.xlsx$")) {
        
        sheets <- readxl::excel_sheets(file_path)
        new_data <- setNames(lapply(sheets, function(sheet) {
          df <- readxl::read_excel(file_path, sheet = sheet)
          head_order_record(df, n = n)
        }), sheets)
        
        writexl::write_xlsx(new_data, new_file_path)
      }
    }
    
    purrr::walk(data_files, ~ copy_files(.x, n = 1000))
    
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
    purrr::walk(data_files, ~ copy_files(.x, n = 10))
  }
  
  rmd_files <- dir_ls(path = source_path, recurse = TRUE, glob = "*.Rmd")
  purrr::walk(rmd_files, update_rmd_paths)
  message("All operations completed.")
}

# Fonction pour supprimer les fichiers spécifiques
remove_specific_files <- function(root_dir) {
  files_to_remove <- c("CWP_dataset.csv", "Dataset_harmonized.csv", "not_mapped_total.csv", "recap_mapping.csv", "local_map_codelists_no_DB.R", "not_displayed_monthly.csv", "removed_irregular_areas.csv", 
                       "areas_in_land.csv")
  
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
