# Global Tuna Atlas data-production workflow

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11563961.svg)](https://doi.org/10.5281/zenodo.11563961)

This repository contains the reproducible R and `geoflow` processing workflow used to prepare, harmonise, validate and publish Global Tuna Atlas catch and fishing-effort datasets.

The workflow processes data from the five tuna Regional Fisheries Management Organisations (tRFMOs) and produces harmonised nominal catch, georeferenced catch, fishing-effort and Level 0–2 catch products.

## Documentation

* [Workflow architecture](docs/WORKFLOW.md) — processing stages, configurations and dependencies
* [Running the workflow](docs/RUNNING.md) — Docker, input data, volumes, runtime parameters and examples
* [Validation](docs/VALIDATION.md) — technical and scientific validation procedures

## Pre-built Docker images

Pre-built Docker images are available from the GitHub Container Registry (GHCR):

| Image                              | Purpose                                                                  |
| ---------------------------------- | ------------------------------------------------------------------------ |
| `ghcr.io/firms-gta/gta-workflow`   | Complete runtime environment for the GTA processing workflow             |
| `ghcr.io/firms-gta/tunaatlas-data` | GTA input-data image used for reproducible or self-contained deployments |

Images are tagged with the corresponding source revision. For reproducible runs, prefer an immutable version or commit tag rather than `latest`.

For example:

```bash
docker pull ghcr.io/firms-gta/gta-workflow:2d93b5a
docker pull ghcr.io/firms-gta/tunaatlas-data:9e9e214
```

The workflow image can therefore be used directly without rebuilding the R environment locally.

To inspect the available image versions, see the repository packages on GitHub Container Registry.

## Quick start

Pull the workflow image:

```bash
docker pull ghcr.io/firms-gta/gta-workflow:2d93b5a
```

Optionally create a shorter local tag:

```bash
docker tag \
  ghcr.io/firms-gta/gta-workflow:2d93b5a \
  gta-workflow:latest
```

Prepare persistent runtime directories:

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
```

Run the pre-harmonisation workflows from a local raw-data directory:

```bash
docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v /absolute/path/to/all_raw_data_GTA:/data/GTA_2026:ro \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=rawdata \
  -e GTA_DATA_SOURCE=volume_dir \
  -e GTA_DATA_PATH=/data/GTA_2026 \
  ghcr.io/firms-gta/gta-workflow:2d93b5a
```

Alternatively, the image can be built locally when developing or modifying the workflow:

```bash
docker build \
  -f docker/Dockerfile.workflow \
  -t gta-workflow:latest \
  .
```

See [Running the workflow](docs/RUNNING.md) for other input methods, including Zenodo DOI, local archives and persistent volumes.

## Workflow stages

The workflow can run the complete processing chain or selected stages through `GTA_STEPS`.

| `GTA_STEPS`   | Purpose                                                                     |
| ------------- | --------------------------------------------------------------------------- |
| `rawdata`     | Run nominal catch, georeferenced catch and fishing-effort pre-harmonisation |
| `raw_nominal` | Run nominal catch pre-harmonisation only                                    |
| `raw_georef`  | Run georeferenced catch pre-harmonisation only                              |
| `raw_effort`  | Run fishing-effort pre-harmonisation only                                   |
| `nominal`     | Produce the harmonised nominal catch dataset                                |
| `effort`      | Produce the harmonised fishing-effort dataset                               |
| `level0`      | Produce the Level 0 catch dataset                                           |
| `level1`      | Produce the Level 1 catch dataset                                           |
| `level2`      | Produce the Level 2 catch dataset                                           |
| `summaries`   | Regenerate dataset summaries                                                |
| `reports`     | Generate Level 2 versus nominal comparison outputs                          |
| `qa_rmd`      | Regenerate pre-harmonisation QA documentation                               |
| `all`         | Run all production stages                                                   |

Several stages can be selected with a comma-separated value:

```bash
-e GTA_STEPS=nominal,level0,level1,level2
```

See [Workflow architecture](docs/WORKFLOW.md) for details on dependencies and the corresponding `geoflow` configurations.

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
├── compose.workflow.yml
└── renv.lock
```

Runtime raw data and generated jobs are not part of the repository and should be mounted externally.

## Reproducibility

The workflow is designed to keep the scientific environment and runtime data separate:

* R dependencies are pinned with `renv.lock`;
* the Docker image pins the R runtime and external mapping dependencies;
* raw datasets are supplied at runtime rather than embedded in the image;
* downloaded archives, working data and generated jobs use separate persistent paths;
* database publication is optional and requires a validated database connection.

For an auditable production run, retain the repository commit SHA, Docker image tag or digest, input DOI or checksum, runtime parameters and generated job directories.

## Tests

Run the launcher smoke tests with:

```bash
Rscript tests/smoke_test_launcher.R
```

The same checks are run by GitHub Actions on relevant repository changes.

Scientific and runtime acceptance procedures are documented in [Validation](docs/VALIDATION.md).

## Licence and citation

Reuse the software and datasets according to the repository licence and the licence associated with each published dataset.

When using Global Tuna Atlas data, cite the DOI corresponding to the dataset release rather than using the repository DOI as a substitute for the dataset citation.
