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
#   - system libraries required by spatial/database/R packages
#   - the geoflow-tunaatlas repository
#   - the renv-restored R environment
#
# It does NOT contain:
#   - GTA raw data
#   - LaTeX / full PDF reporting stack
#   - RStudio Server
#
# Data should be provided at runtime with a Docker volume:
#
#   docker run \
#     -v /home/bastien/data/GTA_2026:/data/GTA_2026 \
#     -e GTA_STEPS=rawdata,level1 \
#     gta-workflow
#
# Or with a mounted zip:
#
#   docker run \
#     -v /home/bastien/GTA_2026.zip:/data/GTA_2026.zip \
#     -e GTA_STEPS=rawdata \
#     gta-workflow
#
# =============================================================================

ARG BASE_IMAGE=rocker/r-ver:4.2.3
FROM ${BASE_IMAGE}

LABEL maintainer="Grasset Bastien <bastien.grasset@ird.fr>"

ARG GITHUB_BRANCH=master

ENV PROJECT_DIR=/home/rstudio/geoflow-tunaatlas
ENV RENV_PATHS_ROOT=/opt/renv
ENV RENV_PATHS_CACHE=/opt/renv/cache
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE

# Default workflow parameters used by the CLI wrapper.
ENV GTA_STEPS=rawdata
ENV GTA_DATA_SOURCE=auto
ENV GTA_SUMMARISE_INVALID_RAW=false
ENV GTA_STOP_ON_MISSING_INPUTS=true
ENV GTA_BOOTSTRAP_RESTORE_RENV=false

# Create the rstudio user even without using rocker/rstudio.
# This keeps paths consistent with existing scripts and volumes.
RUN id -u rstudio >/dev/null 2>&1 || useradd -m -s /bin/bash rstudio

WORKDIR ${PROJECT_DIR}

# System dependencies required for geospatial, database, XML, NetCDF, and
# miscellaneous R packages used by the GTA workflow.
#
# Deliberately excluded here:
#   - texlive-*
#   - full LaTeX stack
#   - RStudio Server
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    curl \
    wget \
    unzip \
    dos2unix \
    cmake \
    make \
    g++ \
    pkg-config \
    protobuf-compiler \
    gdal-bin \
    udunits-bin \
    redland-utils \
    libssl-dev \
    libcurl4-openssl-dev \
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

RUN mkdir -p \
    ${PROJECT_DIR} \
    ${RENV_PATHS_CACHE} \
    /data \
 && chown -R rstudio:rstudio /home/rstudio ${RENV_PATHS_ROOT} /data

RUN git clone --branch ${GITHUB_BRANCH} --depth 1 \
    https://github.com/firms-gta/geoflow-tunaatlas.git \
    ${PROJECT_DIR}

WORKDIR ${PROJECT_DIR}

# Install the renv version recorded in renv.lock, then restore the project
# library at image build time. This makes runtime execution much faster and more
# reproducible.
RUN Rscript -e "install.packages(c('remotes','jsonlite'), repos='https://cloud.r-project.org')" \
 && Rscript -e "ver <- jsonlite::fromJSON('renv.lock')\$Packages[['renv']]\$Version; remotes::install_version('renv', version = ver, upgrade = 'never', repos = 'https://cloud.r-project.org')" \
 && Rscript -e "renv::restore(prompt = FALSE)"

RUN chown -R rstudio:rstudio ${PROJECT_DIR} ${RENV_PATHS_ROOT} /data

USER rstudio

WORKDIR ${PROJECT_DIR}

CMD ["Rscript", "R/launching_workflows/run_gta_2026_workflow_cli.R"]