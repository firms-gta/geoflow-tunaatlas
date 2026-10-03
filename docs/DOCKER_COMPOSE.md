# Running the Global Tuna Atlas workflow with Docker Compose

This document describes how to run the Global Tuna Atlas workflow using `compose.workflow.yml`.

Docker Compose provides a shorter interface than writing the complete `docker run` command manually.

For the equivalent explicit `docker run` commands, see [RUNNING.md](RUNNING.md).

For the workflow architecture and processing stages, see [WORKFLOW.md](WORKFLOW.md).

## 1. How the Compose configuration works

The repository contains:

```text
compose.workflow.yml
```

It defines the `gta-workflow` service and configures:

* the Docker image;
* the Linux UID and GID used inside the container;
* workflow environment variables;
* raw input data;
* extracted working data;
* persistent `geoflow` jobs;
* the download cache.

By default, the following host directories are used:

| Host path             | Container path                                  | Purpose                    |
| --------------------- | ----------------------------------------------- | -------------------------- |
| `./runtime/input`     | `/data/GTA_2026`                                | Raw input data             |
| `./runtime/extracted` | `/home/rstudio/geoflow-tunaatlas/data/GTA_2026` | Writable working input     |
| `./runtime/jobs`      | `/home/rstudio/geoflow-tunaatlas/jobs`          | `geoflow` jobs and outputs |
| `./runtime/cache`     | `/cache`                                        | Download cache             |

The paths can be changed using environment variables such as `GTA_INPUT_DIR`, `GTA_EXTRACTED_DIR`, `GTA_JOBS_DIR` and `GTA_CACHE_DIR`.

## 2. Docker image

For reproducible runs, use a specific image tag.

For example:

```bash
docker pull ghcr.io/firms-gta/gta-workflow:2d93b5a
```

The image used by Compose is selected with:

```text
GTA_IMAGE
```

The examples below therefore explicitly set:

```text
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a
```

If `GTA_IMAGE` is not provided, the Compose configuration falls back to:

```text
gta-workflow:latest
```

Using an explicit GHCR tag is recommended for reproducible production runs.

## 3. Run from a local data directory

Use this method when the GTA raw source files are already available in a local directory.

The directory must directly contain the source files expected by the workflow.

For example:

```text
/path/to/all_raw_data_GTA/
├── iotc_nominal_catch_firms_level0_2026-04-13.csv
├── ...
```

Replace `/absolute/path/to/all_raw_data_GTA` below with the actual directory.

First create the runtime directories and make sure the current user owns them:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Then run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_INPUT_DIR=/absolute/path/to/all_raw_data_GTA \
GTA_STEPS=rawdata \
GTA_DATA_SOURCE=volume_dir \
GTA_DATA_PATH=/data/GTA_2026 \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

### What this command does

`GTA_UID` and `GTA_GID` make the container run with the current Linux user's identity.

This avoids creating workflow outputs owned by `root`.

`GTA_IMAGE` selects the exact workflow image.

`GTA_INPUT_DIR` tells Compose which host directory should be mounted as:

```text
/data/GTA_2026
```

The input directory is mounted read-only.

`GTA_STEPS=rawdata` runs the three pre-harmonisation branches:

* nominal catch;
* georeferenced catch;
* georeferenced fishing effort.

`GTA_DATA_SOURCE=volume_dir` tells the launcher that the input is an existing directory.

`GTA_DATA_PATH=/data/GTA_2026` points the launcher to the mounted directory inside the container.

The workflow outputs are persisted under:

```text
runtime/jobs/
```

## 4. Run directly from Zenodo

Use this method when the raw-data package should be downloaded directly from Zenodo.

The GTA 2026 raw-data package is available from record:

```text
10.5281/zenodo.20834708
```

This Zenodo record contains several files.

The raw-data archive must therefore currently be specified explicitly as:

```text
all_raw_data_GTA.zip
```

Before running, prepare the persistent directories:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Then run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_STEPS=rawdata \
GTA_DATA_SOURCE=doi \
GTA_DOI=10.5281/zenodo.20834708 \
GTA_DOI_FILE=all_raw_data_GTA.zip \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

### What this command does

`GTA_DATA_SOURCE=doi` tells the launcher to retrieve its input from Zenodo.

`GTA_DOI` identifies the Zenodo record.

`GTA_DOI_FILE=all_raw_data_GTA.zip` selects the raw-data archive within the record.

The downloaded file is cached under:

```text
runtime/cache/
```

The extracted working data are stored under:

```text
runtime/extracted/
```

The workflow jobs are stored under:

```text
runtime/jobs/
```

The first download requires network access.

Because the cache is persistent, later executions can reuse the downloaded archive.

## 5. Run a single pre-harmonisation branch

Instead of running all three raw-data branches, one branch can be selected.

### Nominal catch only

Prepare the runtime directories:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_INPUT_DIR=/absolute/path/to/all_raw_data_GTA \
GTA_STEPS=raw_nominal \
GTA_DATA_SOURCE=volume_dir \
GTA_DATA_PATH=/data/GTA_2026 \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

This runs only the nominal-catch pre-harmonisation workflow.

### Georeferenced catch only

Prepare the runtime directories:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_INPUT_DIR=/absolute/path/to/all_raw_data_GTA \
GTA_STEPS=raw_georef \
GTA_DATA_SOURCE=volume_dir \
GTA_DATA_PATH=/data/GTA_2026 \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

This runs only the georeferenced-catch pre-harmonisation workflow.

### Fishing effort only

Prepare the runtime directories:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_INPUT_DIR=/absolute/path/to/all_raw_data_GTA \
GTA_STEPS=raw_effort \
GTA_DATA_SOURCE=volume_dir \
GTA_DATA_PATH=/data/GTA_2026 \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

This runs only the fishing-effort pre-harmonisation workflow.

## 6. Run several processing stages

Several workflow stages can be selected by separating them with commas.

For example, to run:

* harmonised nominal catch;
* Catch Level 0;
* Catch Level 1;
* Catch Level 2;

prepare the runtime directories:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Then run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_INPUT_DIR=/absolute/path/to/all_raw_data_GTA \
GTA_STEPS=nominal,level0,level1,level2 \
GTA_DATA_SOURCE=volume_dir \
GTA_DATA_PATH=/data/GTA_2026 \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

The selected stages are executed according to the workflow launcher.

Downstream `geoflow` workflows may depend on resources created by upstream workflows.

Do not skip required upstream stages unless the expected existing job resources are explicitly provided.

See [WORKFLOW.md](WORKFLOW.md) for the stage dependencies.

## 7. Run the complete production workflow

The special stage:

```text
all
```

selects the complete production chain.

Prepare the directories:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Then run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_INPUT_DIR=/absolute/path/to/all_raw_data_GTA \
GTA_STEPS=all \
GTA_DATA_SOURCE=volume_dir \
GTA_DATA_PATH=/data/GTA_2026 \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

This should only be used when the full chain is required.

For development and validation, running individual stages is generally faster and easier to diagnose.

## 8. Reuse existing jobs

The launcher can reuse outputs generated by previous `geoflow` runs.

Available variables include:

| Variable                     | Existing workflow output |
| ---------------------------- | ------------------------ |
| `GTA_RAW_NOMINAL_CATCH`      | Raw nominal catch        |
| `GTA_RAW_DATA_GEOREF`        | Raw georeferenced catch  |
| `GTA_RAW_DATA_GEOREF_EFFORT` | Raw fishing effort       |
| `GTA_TUNAATLAS_EFFORT`       | Harmonised effort        |
| `GTA_TUNAATLAS_NOMINAL`      | Harmonised nominal catch |
| `GTA_TUNAATLAS_LEVEL0_CATCH` | Catch Level 0            |
| `GTA_TUNAATLAS_LEVEL1_CATCH` | Catch Level 1            |
| `GTA_TUNAATLAS_LEVEL2_CATCH` | Catch Level 2            |

The paths supplied through these variables are paths inside the container.

Because `runtime/jobs` is mounted to:

```text
/home/rstudio/geoflow-tunaatlas/jobs
```

existing job paths can normally be referenced as:

```text
jobs/JOB_DIRECTORY
```

## 9. Regenerate summaries from existing jobs

Assume that `runtime/jobs/` already contains the required Level 0, Level 1 and Level 2 jobs.

Prepare permissions:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Then run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_STEPS=summaries \
GTA_TUNAATLAS_LEVEL0_CATCH=jobs/LEVEL0_JOB \
GTA_TUNAATLAS_LEVEL1_CATCH=jobs/LEVEL1_JOB \
GTA_TUNAATLAS_LEVEL2_CATCH=jobs/LEVEL2_JOB \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

Replace:

```text
LEVEL0_JOB
LEVEL1_JOB
LEVEL2_JOB
```

with the actual directory names under `runtime/jobs/`.

This allows summaries to be regenerated without recreating the scientific datasets.

## 10. Generate QA documentation from existing raw jobs

The `qa_rmd` stage uses the existing outputs from the three pre-harmonisation branches.

Prepare the runtime directories:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Then run:

```bash
GTA_UID="$(id -u)" \
GTA_GID="$(id -g)" \
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:2d93b5a \
GTA_STEPS=qa_rmd \
GTA_RAW_NOMINAL_CATCH=jobs/RAW_NOMINAL_JOB \
GTA_RAW_DATA_GEOREF=jobs/RAW_GEOREF_JOB \
GTA_RAW_DATA_GEOREF_EFFORT=jobs/RAW_EFFORT_JOB \
docker compose -f compose.workflow.yml run --rm gta-workflow
```

Replace:

```text
RAW_NOMINAL_JOB
RAW_GEOREF_JOB
RAW_EFFORT_JOB
```

with the actual job directory names.

## 11. Environment variables

The Compose configuration supports the main workflow runtime variables directly.

| Variable                     | Purpose                                     |
| ---------------------------- | ------------------------------------------- |
| `GTA_IMAGE`                  | Docker image used by Compose                |
| `GTA_UID`                    | UID used inside the container               |
| `GTA_GID`                    | GID used inside the container               |
| `GTA_STEPS`                  | Workflow stages                             |
| `GTA_DATA_SOURCE`            | Input-data mode                             |
| `GTA_DATA_PATH`              | Input path inside the container             |
| `GTA_DOI`                    | Zenodo DOI or record                        |
| `GTA_DOI_FILE`               | Exact file within a Zenodo record           |
| `GTA_INPUT_DIR`              | Host directory mounted as raw input         |
| `GTA_EXTRACTED_DIR`          | Host directory for writable extracted input |
| `GTA_JOBS_DIR`               | Host directory for persistent jobs          |
| `GTA_CACHE_DIR`              | Host directory for persistent cache         |
| `GTA_SUMMARISE_INVALID_RAW`  | Generate raw invalid-record summaries       |
| `GTA_STOP_ON_MISSING_INPUTS` | Stop when required inputs are missing       |

Existing workflow outputs can additionally be passed using the `GTA_RAW_*` and `GTA_TUNAATLAS_*` variables documented above.

## 12. Permissions

The Compose service runs with the UID and GID provided by:

```text
GTA_UID
GTA_GID
```

The examples use:

```text
$(id -u)
$(id -g)
```

to match the current Linux user.

Before each run, the examples therefore execute:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

This avoids common errors when a directory was previously created by `root` or another Docker container.

For example:

```text
Failed to open file /cache/all_raw_data_GTA.zip.curltmp
```

can occur when the mounted cache is not writable by the container user.

## 13. Persistent outputs

The most important host directories are:

```text
runtime/jobs/
runtime/cache/
runtime/extracted/
```

`runtime/jobs/` contains the scientific workflow outputs.

`runtime/cache/` allows downloaded resources such as Zenodo archives to be reused.

`runtime/extracted/` contains writable input data prepared for the workflow.

Removing the Docker container does not remove these host directories.

The Compose commands use:

```bash
docker compose -f compose.workflow.yml run --rm gta-workflow
```

so the temporary container itself is removed after execution, while the mounted workflow data remain available.

## 14. When to use Docker Compose

Use Docker Compose when:

* running the workflow repeatedly;
* reusing the same runtime directory layout;
* reducing the length of Docker commands;
* passing different `GTA_*` parameters to the same container configuration.

Use the explicit commands in [RUNNING.md](RUNNING.md) when:

* debugging Docker mounts;
* documenting exactly how the container is constructed;
* testing a new runtime configuration;
* diagnosing behaviour independently of Compose.

Both methods execute the same workflow launcher.

## 15. Related documentation

* [README](../README.md) — project overview
* [RUNNING.md](RUNNING.md) — explicit `docker run` commands
* [WORKFLOW.md](WORKFLOW.md) — processing architecture and stage dependencies
* [VALIDATION.md](VALIDATION.md) — technical and scientific validation
