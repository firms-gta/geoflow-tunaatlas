# Global Tuna Atlas data-production workflow

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11563961.svg)](https://doi.org/10.5281/zenodo.11563961)

This repository contains the reproducible R and `geoflow` processing workflow used to prepare, harmonise, validate and publish Global Tuna Atlas (GTA) catch and fishing-effort datasets.

The workflow processes data from the five tuna Regional Fisheries Management Organisations (tRFMOs) and produces harmonised nominal catch, georeferenced catch, fishing-effort and Level 0–2 catch products.

## Documentation

* [Running the workflow](docs/RUNNING.md) — quick start, input data and standard runs
* [Advanced execution](docs/RUNNING_ADVANCED.md) — partial runs, runtime parameters, existing jobs, reporting and local builds
* [Workflow architecture](docs/WORKFLOW.md) — processing stages, configurations and dependencies
* [Validation](docs/VALIDATION.md) — technical and scientific validation procedures

## Docker images

Pre-built Docker images are available from the GitHub Container Registry (GHCR):

| Image                              | Purpose                                             |
| ---------------------------------- | --------------------------------------------------- |
| `ghcr.io/firms-gta/gta-workflow`   | GTA scientific data-processing workflow             |
| `ghcr.io/firms-gta/tunaatlas-data` | GTA input-data image for self-contained deployments |

For reproducible runs, use an immutable version or commit tag rather than `latest`.

For example:

```bash
docker pull ghcr.io/firms-gta/gta-workflow:2d93b5a
```

The workflow image contains the R environment and required dependencies, so no local R installation is required.

## Quick start

Create persistent runtime directories:

```bash 
mkdir -p runtime/extracted runtime/jobs runtime/cache
```

Run the complete production workflow directly from the GTA input archive published on Zenodo:

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=rawdata,nominal,effort,level0,level1,level2 \
  -e GTA_DATA_SOURCE=doi \
  -e GTA_DOI=10.5281/zenodo.20834708 \
  -e GTA_DOI_FILE=all_raw_data_GTA.zip \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-workflow:2d93b5a
```

The first run downloads the input archive from Zenodo. Downloaded files are cached in `runtime/cache`, prepared data are persisted in `runtime/extracted`, and workflow jobs and logs are written to `runtime/jobs`.

For local data directories, ZIP archives, alternative input datasets and other standard execution modes, see [Running the workflow](docs/RUNNING.md).

For partial runs, reuse of existing jobs, reporting and other advanced options, see [Advanced execution](docs/RUNNING_ADVANCED.md).

## Repository structure

```text 
geoflow-tunaatlas/
├── R/                  # workflow and processing code
├── config/             # geoflow workflow configurations
├── data/               # static reference data
├── docker/             # Docker images and entry points
├── docs/               # project documentation
├── reports/            # report sources
├── tests/              # launcher and workflow checks
└── renv.lock
```

Runtime input data and generated jobs are not part of the repository and should be mounted externally.

## Reproducibility

The workflow separates the software environment from runtime data:

* R dependencies are pinned with `renv.lock`;
* Docker pins the R runtime and required system dependencies;
* raw datasets are supplied at runtime;
* generated data, jobs and logs are persisted outside the container.

For an auditable production run, retain the repository commit SHA, Docker image tag or digest, input DOI or checksum, runtime parameters and generated job directories.

## Tests

Run the launcher smoke tests with:

```bash
Rscript tests/smoke_test_launcher.R
```

Scientific and runtime acceptance procedures are documented in [Validation](docs/VALIDATION.md).

## Licence and citation

Reuse the software and datasets according to the repository licence and the licence associated with each published dataset.

When using Global Tuna Atlas data, cite the DOI corresponding to the dataset release rather than using the repository DOI as a substitute for the dataset citation.
