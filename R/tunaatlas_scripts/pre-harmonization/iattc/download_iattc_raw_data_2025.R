# Définition du dossier de téléchargement
download_folder <- here::here("~/firms-gta/geoflow-tunaatlas/data")
if (!dir.exists(download_folder)) {
  dir.create(download_folder, recursive = TRUE)
}

# Liste des URLs
urls <- c(
  "https://www.iattc.org/getmedia/28abf87e-37af-40ab-8158-cb1b51b0e567/CatchByFlagGear.zip",
  "https://www.iattc.org/getmedia/215185c7-9892-4843-8cf0-130eafc028ab/PublicPSTuna.zip",
  "https://www.iattc.org/getmedia/3d532007-2063-4b47-bf00-59296f4c13b1/PublicPSBillfish.zip",
  "https://www.iattc.org/getmedia/28a7d4c4-92ff-4edd-8a03-cdade4c91bd7/PublicPSShark.zip",
  "https://www.iattc.org/getmedia/7ddad259-8ab4-42b3-a8c7-5863945f6e40/PublicSizePSBillfish.zip",
  "https://www.iattc.org/getmedia/b8f0bdbb-595d-4c16-9965-cbedcf122aaa/PublicLLTunaBillfish.zip",
  "https://www.iattc.org/getmedia/334da158-5f3e-495b-9df8-05b873977751/PublicLLShark.zip",
  "https://www.iattc.org/getmedia/0c5f64fe-186c-4479-8f05-064f411e66f2/PublicLPTuna.zip"
)

for (url in urls) {
  filename <- file.path(download_folder, basename(url))
  
  # Télécharger le fichier ZIP
  download.file(url, destfile = filename, mode = "wb")
  message("Téléchargé : ", filename)
  
  # Extraire le fichier ZIP
  unzip(filename, exdir = download_folder)
  message("Extrait : ", filename)
}
