library(googledrive)
library(lubridate)

parent_folder_id <- "16fVLytARK13uHCKffho3kYJgm0KopbKL"
trfmos <- c("ccsbt", "iattc", "iotc", "iccat", "wcpfc")

files <- drive_ls(as_id(parent_folder_id))

if (nrow(files) == 0) {
  stop("Aucun fichier trouvé dans le dossier Google Drive.")
}

recent_files <- files[ymd_hms(files$drive_resource[[1]]$modifiedTime) > (Sys.time() - months(2)), ]

for (trfmo in trfmos) {
  folder_name <- paste0(trfmo, "_2024")
  existing_folder <- drive_ls(as_id(parent_folder_id), pattern = folder_name)
  
  if (nrow(existing_folder) == 0) {
    new_folder <- drive_mkdir(folder_name, path = as_id(parent_folder_id))
  } else {
    new_folder <- existing_folder
  }
  
  matching_files <- recent_files[grepl(trfmo, recent_files$name, ignore.case = TRUE), ]
  
  if (nrow(matching_files) > 0) {
    for (i in seq_len(nrow(matching_files))) {
      file_id <- matching_files$id[i]
      file_name <- matching_files$name[i]
      
      if (!is.na(file_id)) {
        tryCatch({
          drive_cp(as_id(file_id), path = as_id(new_folder$id))
          message(paste("Copie réussie :", file_name, "->", folder_name))
        }, error = function(e) {
          message(paste("Erreur lors de la copie de :", file_name, "->", folder_name))
        })
      }
    }
  } else {
    message(paste("Aucun fichier récent pour", trfmo))
  }
}

message("Copie des fichiers terminée.")
