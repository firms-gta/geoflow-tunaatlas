# Global Tuna Atlas data-production workflow

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11563961.svg)](https://doi.org/10.5281/zenodo.11563961)

This repository contains the reproducible R and geoflow processing chain used
to prepare, harmonise, validate, summarise, and publish Global Tuna Atlas catch
and fishing-effort datasets.

The 2026 launcher supports full runs and selected workflow stages, local files
or a Zenodo DOI as input, persistent runtime volumes, database-free execution,
and separate Docker images for data production and report generation.

## Start here

- [Technical architecture](docs/TECHNICAL_WORKFLOW.md)
- [Runtime parameters, data inputs, and persistent volumes](docs/RUNTIME_DATA_AND_VOLUMES.md)
- [Local and SSP Cloud deployment](docs/DEPLOYMENT.md)
- [Validation plan and test record](docs/VALIDATION.md)
- [Detailed Docker runbook](docs/GTA_2026_Docker_Workflow_Guide.Rmd)

## Quick start

Build the workflow image from the repository root:

```bash
docker build \
  -f docker/Dockerfile.workflow \
  -t gta-workflow:latest \
  .
```

Run the three pre-harmonisation workflows from a local directory:

```bash
mkdir -p "$PWD/runtime/jobs" "$PWD/runtime/cache" "$PWD/runtime/extracted"

docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v /absolute/path/to/all_raw_data_GTA:/data/GTA_2026:ro \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=rawdata \
  -e GTA_DATA_SOURCE=volume_dir \
  -e GTA_DATA_PATH=/data/GTA_2026 \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  gta-workflow:latest
```

Run the same stage directly from the Zenodo record containing
`all_raw_data_GTA.zip`:

```bash
docker run --rm \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=rawdata \
  -e GTA_DATA_SOURCE=doi \
  -e GTA_DOI=10.5281/zenodo.20834708 \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  gta-workflow:latest
```

The DOI scenario requires network access for the first run. Later runs reuse
the verified archive stored in the mounted `/cache` volume.

## Main workflow stages

| Value for `GTA_STEPS` | Result |
|---|---|
| `rawdata` | Raw nominal catch, georeferenced catch, and georeferenced effort pre-harmonisation |
| `raw_nominal`, `raw_georef`, `raw_effort` | One pre-harmonisation branch only |
| `effort` | Harmonised effort dataset |
| `nominal` | Harmonised nominal catch dataset |
| `level0`, `level1`, `level2` | Selected catch processing level |
| `summaries` | Summary regeneration from current or existing job paths |
| `reports` | Level 2 versus nominal comparison outputs |
| `qa_rmd` | Pre-harmonisation QA documentation regeneration |
| `all` | All production stages |

Comma-separated values are accepted, for example
`GTA_STEPS=nominal,level0,level1`.

## Docker images

| Image | Dockerfile | Intended use |
|---|---|---|
| `gta-workflow` | `docker/Dockerfile.workflow` | Data checks and scientific processing |
| `gta-reporting` | `docker/Dockerfile.reporting` | Summaries, R Markdown, bookdown, and PDF reports |

The reporting image extends the workflow image. Raw data are not embedded in
either image; they are supplied at runtime through a DOI or a mounted path.

## Tests

The launcher checks parse both entry scripts, verify DOI parsing and archive
selection, and confirm the documented persistent runtime paths:

```bash
Rscript tests/smoke_test_launcher.R
```

The same checks run through
`.github/workflows/workflow-launcher-checks.yml` on relevant pull requests and
can also be started manually.

## Reproducibility and publication

- R package versions are pinned in `renv.lock`.
- The FDI mappings revision is pinned in `docker/Dockerfile.workflow`.
- Database publication is enabled only after the expected database context and
  connection have both been validated.
- Without a valid database connection, processing continues with a temporary
  database-free workflow configuration.
- Inputs, source code, job outputs, and download caches use separate paths.

Do not commit `.env` files, database passwords, access tokens, downloaded raw
data, or generated job directories.

## Licence and citation

Reuse the software and datasets in accordance with the repository licence and
the licence attached to each published dataset. Cite the corresponding dataset
DOI rather than treating the repository DOI as a substitute for a data citation.
