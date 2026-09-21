# =============================================================================
# docker/Dockerfile.workflow
# Image GTA : R 4.2.3 + renv.lock restauré + geoflow patché + codelists FDI
# Contexte de build : racine du dépôt  ->  docker build -f docker/Dockerfile.workflow .
# =============================================================================

ARG BASE_IMAGE=rocker/r-ver:4.2.3
FROM ${BASE_IMAGE}
ARG BASE_IMAGE

LABEL maintainer="Grasset Bastien <bastien.grasset@ird.fr>"

ENV PROJECT_DIR=/home/rstudio/geoflow-tunaatlas
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE

ENV GTA_STEPS=rawdata
ENV GTA_DATA_SOURCE=auto
ENV GTA_SUMMARISE_INVALID_RAW=false
ENV GTA_STOP_ON_MISSING_INPUTS=true
ENV GTA_BOOTSTRAP_RESTORE_RENV=false

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Facultatif, non testé : à activer seulement si le segfault de fin de session (renv_watchdog_unload) persiste
# ENV RENV_WATCHDOG_ENABLED=FALSE

RUN id -u rstudio >/dev/null 2>&1 || useradd -m -s /bin/bash rstudio

WORKDIR ${PROJECT_DIR}

# --- Bibliothèques système (liste inchangée) ---------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo git curl wget unzip dos2unix cmake pkg-config protobuf-compiler \
    gdal-bin udunits-bin redland-utils libssl-dev libcurl4-gnutls-dev libxml2-dev \
    libudunits2-dev libproj-dev libgeos-dev libgdal-dev libv8-dev libsodium-dev \
    libsecret-1-dev libnetcdf-dev libjq-dev libprotobuf-dev librdf0 librdf0-dev \
    libtbb-dev libzmq3-dev libpoppler-cpp-dev libcairo2-dev libfontconfig1-dev \
    libfreetype6-dev libharfbuzz-dev libfribidi-dev \
 && rm -rf /var/lib/apt/lists/*

# --- Restauration renv (couche lente, en cache tant que renv.lock ne change pas)
COPY --chown=rstudio:rstudio renv.lock ${PROJECT_DIR}/renv.lock
COPY --chown=rstudio:rstudio renv/ ${PROJECT_DIR}/renv/

RUN Rscript -e "install.packages(c('remotes','jsonlite'), repos='https://cloud.r-project.org')" \
 && Rscript -e "ver <- jsonlite::fromJSON('renv.lock')\$Packages[['renv']]\$Version; remotes::install_version('renv', version = ver, upgrade='never', repos='https://cloud.r-project.org')" \
 && Rscript -e "source('renv/activate.R'); print(.libPaths()); renv::status()" \
 && Rscript -e "source('renv/activate.R'); renv::restore(prompt = FALSE)" \
 && Rscript -e "source('renv/activate.R'); renv::repair()" \
 && Rscript -e "source('renv/activate.R'); renv::isolate()"

# --- Patchs geoflow --------------------------------
COPY compose/patches/patch-geometa.R \
     compose/patches/patch-geoflow-entities.R \
     compose/patches/patch-geoflow-src-entities.R \
     compose/patches/patch-geoflow-zenodo-size.R \
     /opt/patches/
RUN chmod 644 /opt/patches/*.R

# Les patchs modifient les fichiers de geoflow dans renv/library ; chaque vérification fait échouer le build
RUN cd ${PROJECT_DIR} \
 && for p in patch-geoflow-entities patch-geoflow-src-entities patch-geoflow-zenodo-size; do \
      Rscript -e "source('renv/activate.R'); source('/opt/patches/$p.R')" || exit 1; \
    done \
 && L=renv/library/R-4.2/x86_64-pc-linux-gnu/geoflow \
 && [ "$(grep -c 'PATCH:' $L/metadata/entity/entity_handler_dbi_df.R)" -ge 2 ] \
 && grep -q "keep source attribute" $L/metadata/entity/entity_handler_dbi_df.R \
 && grep -q "size = 100L" $L/actions/zen4R_deposit_record.R \
 && Rscript -e 'source("renv/activate.R"); for (i in 1:2) source("/opt/patches/patch-geometa.R"); invisible(geometa::GMLUnitDefinition$buildFrom("m"))' \
 && Rscript -e 'source("renv/activate.R"); cat("geoflow", as.character(packageVersion("geoflow")), "\n")'

# --- Test de chargement de tous les paquets (avec relance : le chargement de geoflow plante parfois)
COPY --chown=rstudio:rstudio R/docker_creation/testing_loading_of_all_packages.R /tmp/testing_loading_of_all_packages.R
RUN for i in 1 2 3; do \
      Rscript -e "source('${PROJECT_DIR}/renv/activate.R'); source('/tmp/testing_loading_of_all_packages.R')" && exit 0; \
      echo "essai $i échoué"; \
    done; exit 1

# --- Code du projet (après la restauration : un changement de script n'invalide pas les couches renv)
COPY --chown=rstudio:rstudio . ${PROJECT_DIR}

USER rstudio
WORKDIR ${PROJECT_DIR}

RUN find ${PROJECT_DIR}/R -name "*.R" -print0 \
 | xargs -0 perl -CSD -pi -e "s/[‘’]/'/g; s/[“”]/'/g; s/°/ degrees /g; s/–/-/g; s/—/-/g; s/…/.../g"

# --- Ressources FDI, en toute fin -------------------------------------------
ENV FDI_CODELISTS_REPO="https://github.com/bastienird/fdi-codelists.git"
ENV FDI_CODELISTS_REF="f469d9767110c4ea947dbd356a3e4b79b9108d92"
ENV FDI_CODELISTS_DIR=${PROJECT_DIR}/data/fdi-codelists

ENV FDI_MAPPINGS_REPO="https://github.com/fdiwg/fdi-mappings.git"
ENV FDI_MAPPINGS_REF="c74ff137ebd28b0367172a8a73821a0d6"
ENV FDI_MAPPINGS_DIR=${PROJECT_DIR}/data/fdi-mappings

RUN git clone ${FDI_CODELISTS_REPO} ${FDI_CODELISTS_DIR} \
 && cd ${FDI_CODELISTS_DIR} && git checkout ${FDI_CODELISTS_REF} && rm -rf .git

RUN git clone ${FDI_MAPPINGS_REPO} ${FDI_MAPPINGS_DIR} \
 && cd ${FDI_MAPPINGS_DIR} && git checkout ${FDI_MAPPINGS_REF} && rm -rf .git

# --- Provenance (JSON corrigé : celui de ton fichier avait des accolades en trop)
RUN cat > ${PROJECT_DIR}/RESOURCE_VERSIONS.json <<EOF
{
  "project": {
    "repository": "https://github.com/firms-gta/geoflow-tunaatlas"
  },
  "build": {
    "docker_base_image": "${BASE_IMAGE}",
    "renv_lock_sha256": "$(sha256sum ${PROJECT_DIR}/renv.lock | cut -d' ' -f1)"
  },
  "resources": {
    "fdi-codelists": {
      "repository": "${FDI_CODELISTS_REPO}",
      "commit": "${FDI_CODELISTS_REF}"
    },
    "fdi-mappings": {
      "repository": "${FDI_MAPPINGS_REPO}",
      "commit": "${FDI_MAPPINGS_REF}"
    }
  }
}
EOF

CMD ["Rscript", "R/launching_workflows/run_gta_2026_workflow_cli.R"]