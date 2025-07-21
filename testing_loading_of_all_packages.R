# 1) Restaurer l'environnement
renv::restore(prompt = FALSE)

# 2) Lister tous les packages à tester
packages <- names(renv::lockfile_read()$Packages)
packages <- setdiff(packages, "R") # retirer l'entrée pour R lui-même

# 3) Fonction pour tester un package
check_and_reinstall <- function(pkg) {
  message(sprintf("🔍 Test du package : %s", pkg))
  
  ok <- tryCatch({
    suppressMessages(library(pkg, character.only = TRUE))
    TRUE
  }, error = function(e) {
    message(sprintf("❌ Erreur avec %s : %s", pkg, e$message))
    FALSE
  })
  
  if (!ok) {
    message(sprintf("🔄 Réinstallation de %s...", pkg))
    tryCatch({
      renv::install(pkg)
      message(sprintf("✅ %s réinstallé avec succès", pkg))
    }, error = function(e) {
      message(sprintf("❌ Impossible de réinstaller %s : %s", pkg, e$message))
    })
  } else {
    message(sprintf("✅ %s fonctionne déjà", pkg))
  }
}

# 4) Boucler sur tous les packages
for (pkg in packages) {
  check_and_reinstall(pkg)
}
