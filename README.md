# Global Tuna Atlas data-production workflow

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11563961.svg)](https://doi.org/10.5281/zenodo.11563961)

This repository contains the reproducible R and `geoflow` processing workflow used to prepare, harmonise, validate and publish Global Tuna Atlas (GTA) catch and fishing-effort datasets.

The workflow processes data from the five tuna Regional Fisheries Management Organisations (tRFMOs) and produces harmonised nominal catch, georeferenced catch, fishing-effort and Level 0–2 catch products.

No local R installation is needed: everything runs in Docker.

## Which way to run it?

| I want to… | Use | Guide |
| --- | --- | --- |
| produce the datasets (no database, no publication) | `docker run` | [docs/RUNNING.md](docs/RUNNING.md) |
| same, with shorter commands | `compose.workflow.yml` | [docs/DOCKER_COMPOSE.md](docs/DOCKER_COMPOSE.md) |
| produce **and publish** (PostGIS, GeoServer, GeoNetwork, Zenodo), or run the test | `compose/` scripts | [compose/README.md](compose/README.md) |

Other documentation:

* [Advanced execution](docs/RUNNING_ADVANCED.md) — runtime parameters, data source modes, partial runs, existing jobs, reports, local builds
* [Workflow architecture](docs/WORKFLOW.md) — processing stages, configurations and dependencies
* [Incremental runs with {targets}](docs/TARGETS.md) — re-run only what changed, for development and debugging (RStudio)
* [Validation](docs/VALIDATION.md) — technical and scientific validation procedures
* [Continuous integration](docs/CI.md) — automated tests and published images
* [Deployment](docs/DEPLOYMENT.md) — batch runs on SSP Cloud / Kubernetes

## Quick start

Run the complete processing chain directly from the input archive published on Zenodo:

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache

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

The archive is downloaded once and cached in `runtime/cache`; prepared data go to `runtime/extracted`, jobs and logs to `runtime/jobs`.

To run with the database and the publication services:

```bash
./compose/run_workflow_retry.sh    # test on the sample data (same as the CI)
./compose/run_full_workflow.sh     # full run on runtime/extracted/all_raw_data_GTA
```

Read [compose/README.md](compose/README.md) before a full run, in particular for the Zenodo target.

## Docker images

Images are published on the GitHub Container Registry in `ghcr.io/firms-gta/gta-workflow`:

| Tag | Origin |
| --- | --- |
| `sha-<commit>` / `latest` | built and tested by the CI from `master` |
| `sha-<commit>-dev` / `<branch>-dev` | built and tested by the CI from a feature branch |
| `2d93b5a` | earlier release |

The input data are also available as an image, `ghcr.io/firms-gta/tunaatlas-data`, for self-contained deployments.

Use an immutable `sha-…` tag (or a digest) for reproducible runs, rather than `latest`.

## Repository structure

```text
geoflow-tunaatlas/
├── R/                    # workflow and processing code
├── compose/              # workflow Dockerfile, full Compose stack, geoflow patches, launch scripts
├── config/               # geoflow workflow configurations
├── data/                 # static reference data
├── docker/               # additional Docker images (reporting, NetCDF, …)
├── docs/                 # documentation
├── tests/                # launcher checks and sample input data
├── compose.workflow.yml  # workflow-only Compose file
└── renv.lock
```

Runtime input data and generated jobs are not part of the repository and are mounted at runtime.

## Reproducibility

* R dependencies are pinned with `renv.lock`, and Docker pins R and the system libraries;
* raw datasets are supplied at runtime;
* generated data, jobs and logs are written outside the container.

For an auditable run, keep the repository commit SHA, the image tag or digest, the input DOI or checksum, the `GTA_*` parameters and the job directories.

## Tests

```bash
Rscript tests/smoke_test_launcher.R   # launcher smoke tests (seconds, no Docker)
./compose/run_workflow_retry.sh       # end-to-end test of the full stack on the sample data
```

Both also run on GitHub Actions ([CI](docs/CI.md)). Scientific acceptance checks are in [Validation](docs/VALIDATION.md).

## Licence and citation

Reuse the software and datasets according to the repository licence and the licence associated with each published dataset.

When using Global Tuna Atlas data, cite the DOI corresponding to the dataset release rather than using the repository DOI as a substitute for the dataset citation.
