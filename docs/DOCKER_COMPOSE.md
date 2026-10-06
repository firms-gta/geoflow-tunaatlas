# Workflow-only Docker Compose (`compose.workflow.yml`)

`compose.workflow.yml` runs the workflow container **alone**: scientific
processing only, without database, GeoServer, GeoNetwork or Zenodo publication.
It is a shorter way to write the `docker run` commands of [RUNNING.md](RUNNING.md).

> To run the workflow **with** the database and the publication services, use
> the full stack in [`compose/`](../compose/README.md).

## How it works

Everything is configured with environment variables passed on the command line.
The service mounts four host directories:

| Host path (default) | Variable | Container path | Purpose |
| --- | --- | --- | --- |
| `./runtime/input` | `GTA_INPUT_DIR` | `/data/GTA_2026` (read-only) | Raw input data |
| `./runtime/extracted` | `GTA_EXTRACTED_DIR` | `/home/rstudio/geoflow-tunaatlas/data/GTA_2026` | Writable working data |
| `./runtime/jobs` | `GTA_JOBS_DIR` | `/home/rstudio/geoflow-tunaatlas/jobs` | geoflow jobs and outputs |
| `./runtime/cache` | `GTA_CACHE_DIR` | `/cache` | Download cache (Zenodo) |

Without `GTA_IMAGE`, Compose uses `gta-workflow:latest` (built locally from
`compose/Dockerfile.workflow` if missing). Set an explicit GHCR tag for
reproducible runs.

## Before the first run

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
```

The container runs with `GTA_UID`/`GTA_GID` (your user in the examples), so
these folders must belong to you. Errors such as
`Failed to open file /cache/all_raw_data_GTA.zip.curltmp` mean a folder is not
writable.

The examples below share these settings; set them once in your shell:

```bash
export GTA_UID="$(id -u)" GTA_GID="$(id -g)"
export GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a
```

## From a local data directory

The directory must directly contain the raw files (`EF_RAW.csv`, …).

```bash
GTA_INPUT_DIR=/absolute/path/to/all_raw_data_GTA \
GTA_DATA_SOURCE=volume_dir GTA_DATA_PATH=/data/GTA_2026 \
GTA_STEPS=rawdata \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

## Directly from Zenodo

```bash
GTA_DATA_SOURCE=doi \
GTA_DOI=10.5281/zenodo.20834708 GTA_DOI_FILE=all_raw_data_GTA.zip \
GTA_STEPS=rawdata \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

The record contains several files, so `GTA_DOI_FILE` is required. The archive is
cached in `runtime/cache/` and reused by later runs.

## Choosing the stages

Change `GTA_STEPS` in the commands above (comma-separated):

| `GTA_STEPS` | Runs |
| --- | --- |
| `rawdata` | the three pre-harmonisation branches |
| `raw_nominal`, `raw_georef`, `raw_effort` | one pre-harmonisation branch |
| `nominal,level0,level1,level2` | harmonised nominal catch and catch levels |
| `all` | the complete production chain |

Downstream stages depend on upstream outputs: see [WORKFLOW.md](WORKFLOW.md).

## Reusing existing jobs

Stages such as `summaries` and `qa_rmd` can start from jobs already present in
`runtime/jobs/`. Paths are given inside the container, i.e. `jobs/<directory>`:

```bash
# summaries from existing Level 0/1/2 jobs
GTA_STEPS=summaries \
GTA_TUNAATLAS_LEVEL0_CATCH=jobs/LEVEL0_JOB \
GTA_TUNAATLAS_LEVEL1_CATCH=jobs/LEVEL1_JOB \
GTA_TUNAATLAS_LEVEL2_CATCH=jobs/LEVEL2_JOB \
docker compose -f compose.workflow.yml run --rm gta-workflow

# QA documentation from existing raw jobs
GTA_STEPS=qa_rmd \
GTA_RAW_NOMINAL_CATCH=jobs/RAW_NOMINAL_JOB \
GTA_RAW_DATA_GEOREF=jobs/RAW_GEOREF_JOB \
GTA_RAW_DATA_GEOREF_EFFORT=jobs/RAW_EFFORT_JOB \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

| Variable | Existing job |
| --- | --- |
| `GTA_RAW_NOMINAL_CATCH` | Raw nominal catch |
| `GTA_RAW_DATA_GEOREF` | Raw georeferenced catch |
| `GTA_RAW_DATA_GEOREF_EFFORT` | Raw fishing effort |
| `GTA_TUNAATLAS_NOMINAL` | Harmonised nominal catch |
| `GTA_TUNAATLAS_EFFORT` | Harmonised effort |
| `GTA_TUNAATLAS_LEVEL0_CATCH` / `_LEVEL1_CATCH` / `_LEVEL2_CATCH` | Catch Level 0 / 1 / 2 |

## Other variables

| Variable | Purpose |
| --- | --- |
| `GTA_SUMMARISE_INVALID_RAW` | Generate invalid-record summaries after raw stages (`false`) |
| `GTA_STOP_ON_MISSING_INPUTS` | Stop when required inputs are missing (`true`) |

All runtime parameters are described in [RUNNING_ADVANCED.md](RUNNING_ADVANCED.md).
