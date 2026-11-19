#!/bin/bash
## from https://raw.githubusercontent.com/firms-gta/tunaatlas_pie_map_shiny/280418cbeb0424d51539ec5ffb2b5b3c8e770776/entrypoint.sh
set -e

if [ "$MODE" = "dev" ]; then
  echo "🔧 MODE=dev → Préparation de l’environnement RStudio"
  useradd -ms /bin/bash rstudio || echo "Utilisateur rstudio déjà présent"

  if [ ! -d /home/rstudio/geoflow-tunaatlas ]; then
    echo "📁 Copie des fichiers vers /home/rstudio"
    mkdir -p /home/rstudio/geoflow-tunaatlas
    cp -a /root/geoflow-tunaatlas/. /home/rstudio/geoflow-tunaatlas/
    echo "setwd('/home/rstudio/geoflow-tunaatlas')" > /home/rstudio/.Rprofile
    chown -R rstudio:rstudio /home/rstudio
  fi

  echo "🚀 Mode dev : lancement de RStudio Server"
  exec /init
else
  echo "🚀 Mode prod : lancement de l'application Shiny"
  exec R -e "shiny::runApp('/root/geoflow-tunaatlas', port=3838, host='0.0.0.0')"
fi
