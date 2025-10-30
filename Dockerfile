# syntax=docker/dockerfile:1.7
ARG MODE=prod
ARG BASE_IMAGE
FROM ${BASE_IMAGE:-rocker/r-ver:4.2.3}

# Maintainer information
LABEL org.opencontainers.image.authors="julien.barde@ird.fr" org.opencontainers.image.authors="bastien.grasset@ird.fr"
LABEL maintainer="Grasset Bastien <grasset.bastien@ird.fr>"

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
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
    git \
    libnetcdf-dev \
    curl \
    libjq-dev \
    cmake \
    protobuf-compiler \
    libprotobuf-dev \
    librdf0 \
    librdf0-dev \
    libfontconfig1-dev \
    libharfbuzz-dev libfribidi-dev \
    redland-utils \
    unzip && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get clean


RUN apt-get update && apt-get install -y \
    libcairo2-dev
RUN apt-get update && apt-get install -y libpoppler-cpp-dev


# Update and upgrade the system
RUN apt-get update && apt-get upgrade -y

# Install R core package dependencies the following line install httpuv that is usually used in shiny apps
RUN install2.r --error --skipinstalled --ncpus -1 httpuv

# Set the working directory
WORKDIR /root/geoflow-tunaatlas

RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org'); \
                remotes::install_version('jsonlite', version = '1.9.1', upgrade = 'never', repos = 'https://cran.r-project.org')"

ENV RENV_PATHS_ROOT=/root/.cache/R/renv

# Si en mode dev, changer pour le user rstudio
RUN if [ "$MODE" = "dev" ]; then \
      export NEW_PATH="/home/rstudio/.cache/R/renv" && \
      mkdir -p "$NEW_PATH" && \
      chown -R rstudio:rstudio "$(dirname $NEW_PATH)" && \
      echo "RENV_PATHS_ROOT=$NEW_PATH" >> /etc/environment && \
      echo "RENV_PATHS_CACHE=$NEW_PATH" >> /etc/environment; \
    fi

# Ces variables sont utilisées par R/renv à runtime
ENV RENV_PATHS_CACHE=${RENV_PATHS_ROOT}

ARG RENV_LOCK_HASH
RUN if [ -z "${RENV_LOCK_HASH}" ]; then \
      export RENV_LOCK_HASH=$(sha256sum renv.lock | cut -d' ' -f1); \
    fi && \
    echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}" > /tmp/renv_lock_hash.txt

RUN mkdir -p ${RENV_PATHS_ROOT}
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

#using remotes incase cache keep ancient renv version

RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org')"
RUN Rscript -e "remotes::install_version('renv', version = jsonlite::fromJSON('renv.lock')\$Packages[['renv']]\$Version, repos = 'https://cran.r-project.org')"

# COPY renv/library/ renv/library/

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache) carreful keep in multiple lines
RUN R -e "renv::restore()" 
RUN R -e "renv::repair()" 

ARG TEST_SCRIPT_REF=main
ARG TEST_SCRIPT_URL="https://raw.githubusercontent.com/firms-gta/tunaatlas_pie_map_shiny/${TEST_SCRIPT_REF}/testing_loading_of_all_packages.R"

RUN R -e "source(url('$TEST_SCRIPT_URL'), local=TRUE, encoding='UTF-8')"

RUN mkdir -p data

COPY data/ data/

COPY pipelines/data_creation/level_2_catch_local.R ./pipelines/data_creation/
COPY ["config/geoflow_entities_tuna_global_datasets_ird_level2 - 2025_worfklow_catch_level2.csv", "./config/"]
COPY R/sardara_functions/ R/sardara_functions/
COPY R/tunaatlas_actions/ R/tunaatlas_actions/
COPY R/tunaatlas_scripts/ R/tunaatlas_scripts/
COPY config/catch_ird_level2_local.json ./config/
COPY docs/reports/summary_catch_level2_after_workflow.Rmd ./docs/reports/

# Run the data to donwload GTA data for species label, species group, cwp_shape
RUN R -e "options(encoding = \"UTF-8\", stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE)"
RUN R -e "source('pipelines/data_creation/level_2_catch_local.R')"

ENV SITE_DIR=/site
RUN mkdir -p "$SITE_DIR"

WORKDIR /root/geoflow-tunaatlas

# this command can be ran also outside dockerfile
RUN R -q -e "site_dir <- Sys.getenv('SITE_DIR'); \
             if (identical(site_dir, '')) site_dir <- 'docs/reports'; \
             rmarkdown::render(
               input = here::here('docs/reports/summary_catch_level2_after_workflow.Rmd'),
               output_format = 'bookdown::html_document2',
               output_dir = site_dir,
               output_file = 'Summarycatchlevel2.html',
               envir = .GlobalEnv,
               knit_root_dir = here::here()
             )"
