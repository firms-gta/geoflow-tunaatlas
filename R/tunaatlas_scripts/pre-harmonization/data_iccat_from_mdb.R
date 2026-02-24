# ===============================
# 📦 Dépendances système à installer dans ton Dockerfile :
# RUN apt-get update && apt-get install -y \
#   p7zip-full \
#   mdbtools

# 📦 Dépendance R minimale :
# install.packages("readr")
# ===============================
#install sudo apt install mdbtools
# 📥 Télécharger l’archive .7z contenant le fichier Access
url <- "https://www.iccat.int/Data/t2ce_20250131.7z"
destfile <- "t2ce_20250131.7z"
download.file(url, destfile, mode = "wb")

# 📂 Décompression avec 7z
dir_out <- "unzipped_data"
dir.create(dir_out, showWarnings = FALSE)
system(sprintf('7z x "%s" -o"%s"', destfile, dir_out))

# 📁 Chemin vers le fichier MDB
mdb_file <- file.path(dir_out, "t2ce_20250131web.mdb")
mdb_file_2026 <- path.expand("~/Downloads/t2ce_20260130web.mdb")

tables <- system(sprintf("mdb-tables -1 '%s'", mdb_file), intern = TRUE)
dir.create("data2024", showWarnings = FALSE)
for (table in tables) {
  # Nettoyer le nom de fichier (éventuels espaces ou majuscules)
  out_file <- paste0("data2024/", tolower(gsub(" ", "_", table)), ".csv")
  
  cmd <- sprintf('mdb-export "%s" "%s" > "%s"', mdb_file, table, out_file)
  message("Exporting: ", table)
  system(cmd)
}
flags <- read_csv("data2024/flags.csv")
catalogue <- read_csv("data2024/catalogue.csv")


t2ce_noSchool <- catalogue %>% dplyr::inner_join(flags)

# 📤 Extraire les noms de table avec mdb-tables
tables <- system(sprintf("mdb-tables -1 '%s'", mdb_file_2026), intern = TRUE)

# 📂 Créer un dossier pour les fichiers CSV
dir.create("data", showWarnings = FALSE)

# 🔁 Exporter chaque table en CSV
for (table in tables) {
  # Nettoyer le nom de fichier (éventuels espaces ou majuscules)
  out_file <- paste0("data/", tolower(gsub(" ", "_", table)), ".csv")
  
  cmd <- sprintf('mdb-export "%s" "%s" > "%s"', mdb_file_2026, table, out_file)
  message("Exporting: ", table)
  system(cmd)
}

# ✅ Lire un exemple
library(readr)
flags <- read_csv("data/flags.csv")
catalogue <- read_csv("data/catalogue.csv")
t2ce <- read_csv("data/t2ce.csv")


t2ce_noSchool <- t2ce %>% dplyr::inner_join(flags )
fwrite(t2ce_noSchool,"data/t2ce_noSchool.csv")
