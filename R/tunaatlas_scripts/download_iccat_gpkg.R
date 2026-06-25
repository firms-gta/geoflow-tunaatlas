library(sf)
library(readr)
library(dplyr)

url <- "https://www.iccat.int/Documents/Gis/ICCAT_gis.rar"
output_csv <- "data/GTA_2026/iccat_shapefile.csv"
layer_name <- "iccat_subareas"   # ex: "ICCAT_areas" ; sinon NULL = première couche

# 1. Télécharger le .rar
rar_file <- file.path(tempdir(), "ICCAT_gis.rar")
download.file(url, rar_file, mode = "wb")

# 2. Extraire le .rar
extract_dir <- file.path(tempdir(), "ICCAT_gis_extract")
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

if (nzchar(Sys.which("unrar"))) {
  system2("unrar", c("x", "-o+", rar_file, extract_dir))
} else if (nzchar(Sys.which("7z"))) {
  system2("7z", c("x", rar_file, paste0("-o", extract_dir), "-y"))
} else {
  stop("Il faut installer 'unrar' ou '7z' pour extraire le fichier .rar.")
}

# 3. Trouver le GeoPackage
gpkg_file <- list.files(
  extract_dir,
  pattern = "\\.gpkg$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(gpkg_file) == 0) {
  stop("Aucun fichier .gpkg trouvé après extraction.")
}

gpkg_file <- gpkg_file[1]

# 4. Choisir la couche
layers_info <- sf::st_layers(gpkg_file)

if (is.null(layer_name)) {
  layer_name <- layers_info$name[1]
  message("Couche utilisée : ", layer_name)
  message("Couches disponibles : ", paste(layers_info$name, collapse = ", "))
} else if (!layer_name %in% layers_info$name) {
  stop(
    "La couche demandée n'existe pas. Couches disponibles : ",
    paste(layers_info$name, collapse = ", ")
  )
}

# 5. Lire la couche
x <- sf::st_read(gpkg_file, layer = layer_name, quiet = TRUE)

# 6. Convertir la géométrie en WKT et écrire en CSV
x_csv <- x %>%
  dplyr::mutate(geometry = sf::st_as_text(sf::st_geometry(x))) %>%
  sf::st_drop_geometry()

readr::write_csv(x_csv, output_csv)

message("CSV créé : ", output_csv)