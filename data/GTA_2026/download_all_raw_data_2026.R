# Définition du dossier de téléchargement
download_folder <- here::here("~/firms-gta/geoflow-tunaatlas/data/GTA_2026")
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


# donwload wcpfc ----------------------------------------------------------

# download wcpfc ----------------------------------------------------------

wcpfc_urls <- c(
  # Purse seine – grille 1x1 CWP
  "https://www.wcpfc.int/sites/default/files/2025-12/WCPFC_S_PUBLIC_BY_YY_MM_1x1.zip",
  # purseseines grilles 5x5 --> semblent plus complètes, peut on utiliser les duex ? (garder les données 1 deg et pour les données mnaquantes prendre des 5 deg ? )
  "https://www.wcpfc.int/sites/default/files/2025-12/WCPFC_S_PUBLIC_BY_YY_MM.zip",
  # Longline – par flag
  "https://www.wcpfc.int/sites/default/files/2025-12/WCPFC_L_PUBLIC_BY_YY_MM_FLAG.zip",
  
  # Gillnet (driftnet, inactive since 1991 mais données historiques)
  "https://www.wcpfc.int/sites/default/files/2025-10/WCPFC_G_PUBLIC_BY_YR_MON_2.zip",
  
  # Pole-and-line
  "https://www.wcpfc.int/sites/default/files/2025-12/WCPFC_P_PUBLIC_BY_YY_MM.zip"
)

for (url in wcpfc_urls) {
  filename <- file.path(download_folder, basename(url))
  
  download.file(url, destfile = filename, mode = "wb")
  message("Téléchargé : ", filename)
  
  unzip(filename, exdir = download_folder)
  message("Extrait : ", filename)
}
file.rename(file.path(download_folder,"WCPFC_G_PUBLIC_BY_YR_MON.CSV"), file.path(download_folder,"WCPFC_G_PUBLIC_BY_YR_MON.csv"))

# dowload IOTC effort


# https://iotc.org/sites/default/files/documents/2025/10/IOTC-DATASETS-2025-10-13-CEALL.zip download for iotc

# download for iccat effort

# https://www.iccat.int/Data/t2ce_PS1991-2024_bySchool.zip