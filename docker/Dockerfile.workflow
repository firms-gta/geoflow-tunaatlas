# =============================================================================
# Dockerfile.workflow
# =============================================================================
#
# Purpose
# -------
# Lightweight image for running the GTA data-production workflows.
#
# This image contains:
#   - R 4.2.3
#   - System libraries required by GTA
#   - The geoflow-tunaatlas source code
#   - A restored renv project library
#
# Faster local build
# ------------------
# To avoid downloading all R packages again during the build, run locally:
#
#   Rscript -e "renv::isolate()"
#
# This copies the current renv environment into:
#
#   renv/library
#
# Then make sure renv/library is NOT excluded in .dockerignore.
# Docker will copy the isolated library directly into the image.
#
# This is convenient for local builds, but less clean for CI or public builds.
#
# =============================================================================

ARG BASE_IMAGE=rocker/r-ver:4.2.3
FROM ${BASE_IMAGE}

LABEL maintainer="Grasset Bastien <bastien.grasset@ird.fr>"

# -----------------------------------------------------------------------------
# Project configuration
# -----------------------------------------------------------------------------

ENV PROJECT_DIR=/home/rstudio/geoflow-tunaatlas

ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE

ENV GTA_STEPS=rawdata
ENV GTA_DATA_SOURCE=auto
ENV GTA_SUMMARISE_INVALID_RAW=false
ENV GTA_STOP_ON_MISSING_INPUTS=true
ENV GTA_BOOTSTRAP_RESTORE_RENV=false

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# -----------------------------------------------------------------------------
# Create runtime user.
# -----------------------------------------------------------------------------

RUN id -u rstudio >/dev/null 2>&1 || useradd -m -s /bin/bash rstudio

WORKDIR ${PROJECT_DIR}

# -----------------------------------------------------------------------------
# Install system dependencies required by the GTA workflow.
# -----------------------------------------------------------------------------

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    git \
    curl \
    wget \
    unzip \
    dos2unix \
    cmake \
    pkg-config \
    protobuf-compiler \
    gdal-bin \
    udunits-bin \
    redland-utils \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libv8-dev \
    libsodium-dev \
    libsecret-1-dev \
    libnetcdf-dev \
    libjq-dev \
    libprotobuf-dev \
    librdf0 \
    librdf0-dev \
    libtbb-dev \
    libzmq3-dev \
    libpoppler-cpp-dev \
    libcairo2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
 && rm -rf /var/lib/apt/lists/*

# -----------------------------------------------------------------------------
# Create directories used by the workflow.
# -----------------------------------------------------------------------------

RUN mkdir -p \
    ${PROJECT_DIR} \
    ${RENV_PATHS_ROOT} \
    /data \
    ${FDI_MAPPINGS_CACHE_DIR} \
 && chown -R rstudio:rstudio \
    /home/rstudio \
    ${RENV_PATHS_ROOT} \
    /data \
    ${FDI_MAPPINGS_CACHE_DIR}

# -----------------------------------------------------------------------------
# Copy renv metadata and optional isolated local library.
#
# For faster local builds, run before docker build:
#
#   Rscript -e "renv::restore(prompt = FALSE)"
#   Rscript -e "renv::isolate()"
#
# If renv/library is present, it is copied into the image and used as the first
# source of installed packages. renv::restore() will then only download/install
# what is still missing or out of sync with renv.lock.
# -----------------------------------------------------------------------------

COPY --chown=rstudio:rstudio renv.lock ${PROJECT_DIR}/renv.lock
COPY --chown=rstudio:rstudio renv/ ${PROJECT_DIR}/renv/

# -----------------------------------------------------------------------------
# Install renv, check the copied library, complete missing packages, then isolate.
# -----------------------------------------------------------------------------

RUN Rscript -e "install.packages(c('remotes','jsonlite'), repos='https://cloud.r-project.org')" \
 && Rscript -e "ver <- jsonlite::fromJSON('renv.lock')\$Packages[['renv']]\$Version; remotes::install_version('renv', version = ver, upgrade='never', repos='https://cloud.r-project.org')" \
 && Rscript -e "source('renv/activate.R'); print(.libPaths()); renv::status()" \
 && Rscript -e "source('renv/activate.R'); renv::restore(prompt = FALSE)" \
 && Rscript -e "source('renv/activate.R'); renv::repair()" \
 && Rscript -e "source('renv/activate.R'); renv::isolate()"


# -----------------------------------------------------------------------------
# Copy the complete project source code.
#
# This is done after renv::restore() so changes in R scripts do not invalidate
# the package installation layer when renv.lock is unchanged.
# -----------------------------------------------------------------------------

# Copier only the script for if data has been copied from renv/ folder
COPY --chown=rstudio:rstudio R/docker_creation/testing_loading_of_all_packages.R /tmp/testing_loading_of_all_packages.R

RUN Rscript -e "source('${PROJECT_DIR}/renv/activate.R'); source('/tmp/testing_loading_of_all_packages.R')"

# Copy all the project
COPY --chown=rstudio:rstudio . ${PROJECT_DIR}

USER rstudio

WORKDIR ${PROJECT_DIR}

# -----------------------------------------------------------------------------
# Replace a few problematic Unicode characters.
# -----------------------------------------------------------------------------

RUN find ${PROJECT_DIR}/R -name "*.R" -print0 \
 | xargs -0 perl -CSD -pi -e "s/[‘’]/'/g; s/[“”]/'/g; s/°/ degrees /g; s/–/-/g; s/—/-/g; s/…/.../g"

# -----------------------------------------------------------------------------
# Download and cache FDI mappings.
# -----------------------------------------------------------------------------

ENV FDI_MAPPINGS_CACHE_DIR=/opt/fdi-mappings-cache
ENV FDI_MAPPINGS_REF="98491a38c5f85628e90cae63d740c23a2460aed6"

RUN Rscript -e "source('./R/docker_creation/cache_fdi_mappings.R'); cache_fdi_mappings(mapping_cache_dir = Sys.getenv('FDI_MAPPINGS_CACHE_DIR'), fdi_mappings_ref = Sys.getenv('FDI_MAPPINGS_REF'))"

CMD ["Rscript", "R/launching_workflows/run_gta_2026_workflow_cli.R"]