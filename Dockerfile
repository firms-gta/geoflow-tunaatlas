FROM rocker/r-ver:4.2.3

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

# Create data repository to copy DOI.csv, a file listing the dataset to download from zenodo
RUN mkdir -p data 
COPY data/All_rawdata_for_level2.zip /data/All_rawdata_for_level2.zip

RUN mkdir -p /data && \
    cp ./data/All_rawdata_for_level2.zip /data/All_rawdata_for_level2.zip 2>/dev/null || \
      echo "Pas de zip local, on passera au download ensuite" && \
    if [ ! -f /data/All_rawdata_for_level2.zip ]; then \
      curl -fSL -o /data/All_rawdata_for_level2.zip https://zenodo.org/record/15496164/files/All_rawdata_for_level2.zip; \
    fi
    
RUN cd ./data && ls -la

# ARG defines a constructor argument called RENV_PATHS_ROOT. Its value is passed from the YAML file. An initial value is set up in case the YAML does not provide one
ARG RENV_PATHS_ROOT=/root/.cache/R/renv
ENV RENV_PATHS_ROOT=${RENV_PATHS_ROOT}

# Set environment variables for renv cache
ENV RENV_PATHS_CACHE=${RENV_PATHS_ROOT}

# Echo the RENV_PATHS_ROOT for logging
RUN echo "RENV_PATHS_ROOT=${RENV_PATHS_ROOT}"
RUN echo "RENV_PATHS_CACHE=${RENV_PATHS_CACHE}"

# Define the build argument for the hash of renv.lock to stop cache if renv.lock has changed
ARG RENV_LOCK_HASH
RUN if [ -z "${RENV_LOCK_HASH}" ]; then \
      export RENV_LOCK_HASH=$(sha256sum renv.lock | cut -d' ' -f1); \
    fi && \
    echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}" > /tmp/renv_lock_hash.txt

# Create the renv cache directory
RUN mkdir -p ${RENV_PATHS_ROOT}

# Install renv package that records the packages used in the shiny app
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache)
RUN R -e "renv::restore()" 

# Copy the rest of the application code
COPY . .

COPY level_2_catch_local.R .level_2_catch_local.R

# Run the data to donwload GTA data for species label, species group, cwp_shape
RUN R -e "options(encoding = \"UTF-8\", stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE)"
RUN R -e "source('level_2_catch_local.R')"


