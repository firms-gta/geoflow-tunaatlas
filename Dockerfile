# =============================================================================
# Dockerfile: the single Dockerfile of the Global Tuna Atlas workflow
#
# Build context: the repository root. Targets:
#   workflow   R 4.2.3 + renv.lock + patched geoflow 1.3.0 + FDI code lists.
#              Runs the workflow (docker/run_workflow.sh, CI, reports.yml).
#              Default target: `docker build .` builds it.
#   rstudio    workflow + RStudio Server (pinned versions).
#   reporting  workflow + LaTeX, for PDF / bookdown reports (not built by the CI).
#   netcdf     workflow + NetCDF creation (entry point workflow/netcdf/run.sh),
#              for the CCP / EDITO platforms.
#
#   docker build -t gta-workflow .
#   docker build --target rstudio -t gta-rstudio .
#
# Layer cache: every layer up to and including the renv restore is reused as
# long as renv.lock and the instructions above it do not change. Changing any
# of those instructions (even an ENV line) rebuilds every R package, about
# 2 GB to download. Comments can change freely.
# =============================================================================

ARG BASE_IMAGE=rocker/r-ver:4.2.3
FROM ${BASE_IMAGE} AS workflow
ARG BASE_IMAGE

LABEL maintainer="Grasset Bastien <bastien.grasset@ird.fr>"

ENV PROJECT_DIR=/home/rstudio/geoflow-tunaatlas
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE

# Defaults of the image itself; a run sets its own values from .env.
ENV GTA_STEPS=rawdata
ENV GTA_DATA_SOURCE=auto
ENV GTA_SUMMARISE_INVALID_RAW=false
ENV GTA_STOP_ON_MISSING_INPUTS=true
ENV GTA_BOOTSTRAP_RESTORE_RENV=false

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Optional, untested: only if the end-of-session segfault (renv_watchdog_unload) persists
# ENV RENV_WATCHDOG_ENABLED=FALSE

RUN id -u rstudio >/dev/null 2>&1 || useradd -m -s /bin/bash rstudio

WORKDIR ${PROJECT_DIR}

# --- System libraries (+ pandoc: HTML reports and summaries) ------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo git curl wget unzip dos2unix cmake pkg-config protobuf-compiler \
    gdal-bin udunits-bin redland-utils libssl-dev libcurl4-gnutls-dev libxml2-dev \
    libudunits2-dev libproj-dev libgeos-dev libgdal-dev libv8-dev libsodium-dev \
    libsecret-1-dev libnetcdf-dev libjq-dev libprotobuf-dev librdf0 librdf0-dev \
    libtbb-dev libzmq3-dev libpoppler-cpp-dev libcairo2-dev libfontconfig1-dev \
    libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    pandoc \
 && rm -rf /var/lib/apt/lists/*

COPY --chown=rstudio:rstudio renv.lock ${PROJECT_DIR}/renv.lock
COPY --chown=rstudio:rstudio renv/ ${PROJECT_DIR}/renv/

RUN Rscript -e "install.packages(c('remotes','jsonlite'), repos='https://cloud.r-project.org')"

RUN Rscript -e "ver <- jsonlite::fromJSON('renv.lock')\$Packages[['renv']]\$Version; remotes::install_version('renv', version = ver, upgrade='never', repos='https://cloud.r-project.org')"

RUN Rscript -e "source('renv/activate.R'); print(.libPaths()); renv::status()"

RUN Rscript -e "source('renv/activate.R'); renv::restore(prompt = FALSE)"

# Isolate the library so that the packages are in the image, then empty the
# renv cache to save space
RUN Rscript -e "source('renv/activate.R'); renv::isolate()" \
 && Rscript -e "cat(renv::paths\$cache(), \"\n\")" > /tmp/renv_cache_path.txt \
 && rm -rf "$(cat /tmp/renv_cache_path.txt)" \
 && rm -f /tmp/renv_cache_path.txt

# geoflow 1.3.0: register_vocabularies() (.onLoad) rewrites the unzipped GEMET
# thesaurus (~400k lines) with readr::write_lines() (vroom). vroom_write_()
# segfaults reproducibly on that file (vroom/readr bug). Workaround: base R
# writeLines() does the same job without vroom.
RUN mkdir -p /tmp/geoflow-src && cd /tmp/geoflow-src \
 && curl -sO https://cran.r-project.org/src/contrib/Archive/geoflow/geoflow_1.3.0.tar.gz \
 && tar xzf geoflow_1.3.0.tar.gz \
 && sed -i "s/readr::write_lines(readLines(gz_file, warn = F), file = trg_file)/writeLines(readLines(gz_file, warn = F), con = trg_file)/" geoflow/R/geoflow_vocabulary.R \
 && grep -q "writeLines(readLines(gz_file" geoflow/R/geoflow_vocabulary.R \
 && R CMD INSTALL --library=${PROJECT_DIR}/renv/library/R-4.2/x86_64-pc-linux-gnu geoflow \
 && cd / && rm -rf /tmp/geoflow-src

# geoflow 1.3.0: register_software() logs the whole SQL (onstart/onend) with
# config$logger$INFO(), which treats its argument as a sprintf() format:
#   1) 8192-character limit (sprintf 'fmt' length)
#   2) any literal '%' in the SQL (e.g. LIKE '6%') breaks the format
#      ('too few arguments')
# Workaround: truncate and escape the '%' before logging (the complete .sql
# file is still written next to it, not affected by this patch).
RUN mkdir -p /tmp/geoflow-src2 && cd /tmp/geoflow-src2 \
 && curl -sO https://cran.r-project.org/src/contrib/Archive/geoflow/geoflow_1.3.0.tar.gz \
 && tar xzf geoflow_1.3.0.tar.gz \
 && sed -i "s/config\$logger\$INFO(paste0(\"\\\\n\", sql))/config\$logger\$INFO(paste0(\"\\\\n\", gsub(\"%\", \"%%\", substr(sql, 1, 8000)), if (nchar(sql) > 8000) \"... [truncated]\" else \"\"))/" geoflow/R/geoflow_software.R \
 && grep -q 'gsub("%"' geoflow/R/geoflow_software.R \
 && R CMD INSTALL --library=${PROJECT_DIR}/renv/library/R-4.2/x86_64-pc-linux-gnu geoflow \
 && cd / && rm -rf /tmp/geoflow-src2

# --- geoflow patches -----------------------------------------------------------
COPY docker/patches/patch-geometa.R \
     docker/patches/patch-geoflow-entities.R \
     docker/patches/patch-geoflow-src-entities.R \
     docker/patches/patch-geoflow-zenodo-size.R \
     /opt/patches/
RUN chmod 644 /opt/patches/*.R

# The patches edit the geoflow files in renv/library; each check fails the build
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

# --- Patches taken over from the former gta-workflow:geoflow-1.3.0 image -------
#  1. patch-geoflow-zenodo.R: upload_type = "dataset" on the empty Zenodo
#     deposit (recent zen4R no longer sets it). Its first part
#     (addRelatedIdentifier) is already fixed in geoflow 1.3.0 and has no effect.
#  2. Typo "PostreSQL" in geosapi_publish_ogc_services.R: the DBI driver
#     "PostgreSQL" was not recognised as PostGIS and the creation of the
#     GeoServer datastore called stop().
#  (patch-geometa.R works in memory: .Rprofile loads it in every session)
COPY docker/patches/patch-geoflow-zenodo.R /opt/patches/
RUN chmod 644 /opt/patches/patch-geoflow-zenodo.R \
 && cd ${PROJECT_DIR} \
 && Rscript -e "source('renv/activate.R'); source('/opt/patches/patch-geoflow-zenodo.R')" \
 && L=renv/library/R-4.2/x86_64-pc-linux-gnu/geoflow \
 && grep -q "PATCH: set Zenodo upload_type" $L/actions/zen4R_deposit_record.R \
 && sed -i 's/c("Postgres","PostreSQL")/c("Postgres","PostgreSQL")/g' $L/actions/geosapi_publish_ogc_services.R \
 && grep -q 'c("Postgres","PostgreSQL")' $L/actions/geosapi_publish_ogc_services.R \
 && ! grep -q 'PostreSQL' $L/actions/geosapi_publish_ogc_services.R

# --- FDI code lists and mappings, at pinned commits -----------------------------
# Only the pinned commit is fetched (--depth 1), not the whole history.
ENV FDI_CODELISTS_REPO="https://github.com/bastienird/fdi-codelists.git"
ENV FDI_CODELISTS_REF="8b9d530b76b64857a1e74246d6f4dfced3db9b34"
ENV FDI_CODELISTS_DIR=${PROJECT_DIR}/data/fdi-codelists
ENV FDI_MAPPINGS_REPO="https://github.com/bastienird/fdi-mappings.git"
ENV FDI_MAPPINGS_REF="c121ccf171f3f15ccb9673ca051f0a17b9668a50"
ENV FDI_MAPPINGS_DIR=${PROJECT_DIR}/data/fdi-mappings

RUN mkdir -p ${FDI_CODELISTS_DIR} && cd ${FDI_CODELISTS_DIR} \
 && git init -q && git remote add origin ${FDI_CODELISTS_REPO} \
 && git fetch -q --depth 1 origin ${FDI_CODELISTS_REF} && git checkout -q FETCH_HEAD \
 && rm -rf .git

RUN mkdir -p ${FDI_MAPPINGS_DIR} && cd ${FDI_MAPPINGS_DIR} \
 && git init -q && git remote add origin ${FDI_MAPPINGS_REPO} \
 && git fetch -q --depth 1 origin ${FDI_MAPPINGS_REF} && git checkout -q FETCH_HEAD \
 && rm -rf .git

# --- Project code (after the restore: a script change keeps the renv layers) ----
COPY --chown=rstudio:rstudio . ${PROJECT_DIR}

RUN cd ${PROJECT_DIR} && Rscript -e 'for (d in c("workflow/launchers","workflow/actions","R/tunaatlas_scripts/generation")) for (f in list.files(d, pattern="[.]R$", recursive=TRUE, full.names=TRUE)) parse(f)'

# --- Provenance -------------------------------------------------------------------
RUN cat > ${PROJECT_DIR}/RESOURCE_VERSIONS.json <<EOF
{
  "project": { "repository": "https://github.com/firms-gta/geoflow-tunaatlas" },
  "build": {
    "docker_base_image": "${BASE_IMAGE}",
    "renv_lock_sha256": "$(sha256sum ${PROJECT_DIR}/renv.lock | cut -d' ' -f1)"
  },
  "resources": {
    "fdi-codelists": { "repository": "${FDI_CODELISTS_REPO}", "commit": "${FDI_CODELISTS_REF}" },
    "fdi-mappings": { "repository": "${FDI_MAPPINGS_REPO}", "commit": "${FDI_MAPPINGS_REF}" }
  }
}
EOF

# A single chown, once, at the very end
RUN chown -R rstudio:rstudio ${PROJECT_DIR}/data/fdi-codelists ${PROJECT_DIR}/data/fdi-mappings \
 && chown rstudio:rstudio ${PROJECT_DIR}/RESOURCE_VERSIONS.json ${PROJECT_DIR}

USER rstudio
CMD ["Rscript", "workflow/launchers/run_gta_2026_workflow_cli.R"]


# =============================================================================
# rstudio: workflow + RStudio Server
# =============================================================================
# Pinned versions, for reproducible builds:
# - RStudio "latest" (2026.11) puts its pandoc in quarto/bin/tools/x86_64/,
#   a path the rocker script of the base image does not know;
# - with no pandoc found, PANDOC_VERSION=default calls the GitHub API (rate
#   limited on CI runners) and the build fails without any message.
# With an exact version, the pandoc .deb is downloaded straight from the
# release page, without the API.
FROM workflow AS rstudio
USER root
ENV S6_VERSION=v2.1.0.2 RSTUDIO_VERSION=2026.10.0+233 DEFAULT_USER=rstudio \
    PANDOC_VERSION=3.12.1 PATH=/usr/lib/rstudio-server/bin:$PATH
RUN /rocker_scripts/install_rstudio.sh && /rocker_scripts/install_pandoc.sh
ENTRYPOINT []
CMD ["/init"]
EXPOSE 8787


# =============================================================================
# reporting: workflow + LaTeX, for PDF / bookdown reports
# =============================================================================
# Large (LaTeX): build it only when such reports are needed. pandoc is already
# in the workflow stage (pandoc-citeproc no longer exists on Ubuntu 22.04:
# citations are built into pandoc).
FROM workflow AS reporting
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    lmodern \
 && rm -rf /var/lib/apt/lists/*
USER rstudio


# =============================================================================
# netcdf: NetCDF creation, for the CCP / EDITO platforms
# =============================================================================
# Runs as root, as the platforms expect (they mount /ccp_data). qs and chron
# are not in renv.lock: installed here only, from a dated snapshot that still
# builds binaries for R 4.2.
FROM workflow AS netcdf
USER root
RUN Rscript -e "source('renv/activate.R'); install.packages(c('qs','chron'), repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/2024-06-03')"

# Input of the NetCDF creation: the level 2 dataset (Zenodo), and the CWP grid
# from the pinned fdi-codelists of the workflow stage.
RUN mkdir -p data outputs \
 && curl -fsSL 'https://zenodo.org/record/15405414/files/global_catch_tunaatlasird_level2_1950_2023.qs?download=1' \
      -o data/global_catch_tunaatlasird_level2_1950_2023.qs \
 && unzip -o data/fdi-codelists/global/cwp/cl_areal_grid.zip -d data/

RUN Rscript R/netcdf/mapping_cwp_to_raster.R \
 && chmod +x workflow/netcdf/run.sh

ENTRYPOINT ["/home/rstudio/geoflow-tunaatlas/workflow/netcdf/run.sh"]
CMD ["bash"]


# =============================================================================
# Default target (docker build . without --target): the workflow image
# =============================================================================
FROM workflow
