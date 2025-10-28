# Authenticate - this will open a browser for you to log in
require(googledrive)
require(googlesheets4)
library(httr)

# Construire l'URL pour le téléchargement direct
sheet_id <- "1tHkgvLNAPjAytLaRDqdjtDae_OBO8Yke0XWvgNl7DRk"
sheet_gid <- '1649072964'
url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv&gid=%s", sheet_id, sheet_gid)

# Spécifier le chemin local où sauvegarder le fichier CSV
output_file <- "pre_harmo_dublin_core_format.csv"
# Télécharger le fichier
GET(url, write_disk(output_file, overwrite = TRUE))

# Vérifier si le téléchargement a réussi
file.exists(output_file)
pre_harmo_dublin_core_format <- read.csv(output_file)
list_github_paths <- extract_urls_from_column(pre_harmo_dublin_core_format)
