# Running the Global Tuna Atlas workflow

This document describes how to run the Global Tuna Atlas production workflow using the pre-built Docker images or locally built images.

For the processing architecture and scientific workflow stages, see [WORKFLOW.md](WORKFLOW.md).

## 1. Docker images

Pre-built images are published on the GitHub Container Registry (GHCR).

| Image                             | Purpose                                                                             |
| --------------------------------- | ----------------------------------------------------------------------------------- |
| `ghcr.io/firms-gta/gta-workflow`  | Scientific data processing and GTA workflow execution                               |
| `ghcr.io/firms-gta/gta-reporting` | Workflow environment extended with Pandoc, LaTeX and report-generation dependencies |

For reproducible runs, use a specific version or commit tag rather than `latest`.

For example:

```bash
docker pull ghcr.io/firms-gta/gta-workflow:2d93b5a
docker pull ghcr.io/firms-gta/gta-reporting:2d93b5a
```

The two images should use the same version tag.

## 2. Runtime interface

The Docker entry point is configured using environment variables.

| Variable                     | Default           | Meaning                                                       |
| ---------------------------- | ----------------- | ------------------------------------------------------------- |
| `GTA_STEPS`                  | `rawdata`         | Comma-separated workflow stages to execute                    |
| `GTA_DATA_SOURCE`            | `auto`            | Input mode: `auto`, `volume_dir`, `volume_zip`, or `doi`      |
| `GTA_DATA_PATH`              | empty             | Container path to a mounted directory or archive              |
| `GTA_DOI`                    | empty             | Zenodo DOI, record URL, or numeric record ID                  |
| `GTA_DOI_FILE`               | empty             | Exact file to use when a Zenodo record contains several files |
| `GTA_SUMMARISE_INVALID_RAW`  | `false`           | Generate invalid-record summaries after raw stages            |
| `GTA_STOP_ON_MISSING_INPUTS` | `true`            | Stop before processing when required inputs are missing       |
| `GTA_BOOTSTRAP_RESTORE_RENV` | `false` in Docker | Restore R packages at runtime; normally unnecessary           |

Boolean values accept `true`, `1`, `yes`, or `y`, case-insensitively.

## 3. Workflow stages

| `GTA_STEPS`   | Purpose                                            |
| ------------- | -------------------------------------------------- |
| `rawdata`     | Run all three pre-harmonisation branches           |
| `raw_nominal` | Nominal catch pre-harmonisation                    |
| `raw_georef`  | Georeferenced catch pre-harmonisation              |
| `raw_effort`  | Fishing-effort pre-harmonisation                   |
| `nominal`     | Harmonised nominal catch                           |
| `effort`      | Harmonised fishing effort                          |
| `level0`      | Catch Level 0                                      |
| `level1`      | Catch Level 1                                      |
| `level2`      | Catch Level 2                                      |
| `summaries`   | Regenerate summaries                               |
| `reports`     | Generate Level 2 versus nominal comparison outputs |
| `qa_rmd`      | Regenerate pre-harmonisation QA documentation      |
| `all`         | Run all production stages                          |

Several stages can be selected in the same run, for example:

```text
GTA_STEPS=nominal,level0,level1,level2
```

Stage dependencies and corresponding `geoflow` configurations are documented in [WORKFLOW.md](WORKFLOW.md).

## 4. Runtime directories

The examples below use three persistent directories:

| Host directory      | Container path                                  | Purpose                   |
| ------------------- | ----------------------------------------------- | ------------------------- |
| `runtime/extracted` | `/home/rstudio/geoflow-tunaatlas/data/GTA_2026` | Writable working input    |
| `runtime/jobs`      | `/home/rstudio/geoflow-tunaatlas/jobs`          | Workflow jobs and outputs |
| `runtime/cache`     | `/cache`                                        | Download cache            |

Before each workflow run, the commands below create the directories if necessary and ensure that they belong to the current user and are writable.

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

This is particularly important when the directories were previously created by Docker or by `root`.

Do not mount an empty volume over the complete project `data/` directory. Static reference files required by the workflow are included in the image.

## 5. Run from a local data directory

Replace `/absolute/path/to/all_raw_data_GTA` with the directory containing the GTA source files.

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime

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
  ghcr.io/firms-gta/gta-workflow:2d93b5a
```

This runs the three pre-harmonisation workflows:

* nominal catch;
* georeferenced catch;
* georeferenced fishing effort.

The source directory is mounted read-only. The workflow uses `runtime/extracted` as its writable working directory.

Because the source data are local and database publication is not required, this example uses `--network none`.

## 6. Run from a local ZIP archive

Replace `/absolute/path/to/all_raw_data_GTA.zip` with the archive to process.

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime

docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v /absolute/path/to/all_raw_data_GTA.zip:/data/GTA_2026.zip:ro \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=rawdata \
  -e GTA_DATA_SOURCE=volume_zip \
  -e GTA_DATA_PATH=/data/GTA_2026.zip \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-workflow:2d93b5a
```

The archive is extracted into `runtime/extracted` before processing.

## 7. Run directly from Zenodo

The GTA raw-data package is available in Zenodo record `20834708`.

The record contains several files, so the raw-data archive must currently be specified explicitly:

```text
GTA_DOI_FILE=all_raw_data_GTA.zip
```

Run:

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime

docker run --rm \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=rawdata \
  -e GTA_DATA_SOURCE=doi \
  -e GTA_DOI=10.5281/zenodo.20834708 \
  -e GTA_DOI_FILE=all_raw_data_GTA.zip \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-workflow:2d93b5a
```

The first run requires network access.

The downloaded archive is stored in `runtime/cache` and can be reused by later runs.

Do not use `--network none` for the first DOI download.

## 8. Run selected workflow stages

For example, to run nominal catch and catch Levels 0, 1 and 2 from a local data directory:

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime

docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v /absolute/path/to/all_raw_data_GTA:/data/GTA_2026:ro \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=nominal,level0,level1,level2 \
  -e GTA_DATA_SOURCE=volume_dir \
  -e GTA_DATA_PATH=/data/GTA_2026 \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-workflow:2d93b5a
```

Some downstream stages depend on resources produced by upstream `geoflow` jobs.

Skipping an upstream stage is safe only when the expected resource already exists at the path referenced by the downstream configuration.

The launcher does not automatically rewrite scientific `geoflow` configuration metadata.

## 9. Reuse existing jobs

Existing jobs can be supplied for partial reruns, summaries, reports or QA generation.

| Environment variable         | Workflow resource        |
| ---------------------------- | ------------------------ |
| `GTA_RAW_NOMINAL_CATCH`      | `raw_nominal_catch`      |
| `GTA_RAW_DATA_GEOREF`        | `raw_data_georef`        |
| `GTA_RAW_DATA_GEOREF_EFFORT` | `raw_data_georef_effort` |
| `GTA_TUNAATLAS_EFFORT`       | `tunaatlas_effort`       |
| `GTA_TUNAATLAS_NOMINAL`      | `tunaatlas_nominal`      |
| `GTA_TUNAATLAS_LEVEL0_CATCH` | `tunaatlas_level0_catch` |
| `GTA_TUNAATLAS_LEVEL1_CATCH` | `tunaatlas_level1_catch` |
| `GTA_TUNAATLAS_LEVEL2_CATCH` | `tunaatlas_level2_catch` |

For example, to regenerate summaries from existing Level 0, Level 1 and Level 2 jobs:

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime

docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -e GTA_STEPS=summaries \
  -e GTA_TUNAATLAS_LEVEL0_CATCH=jobs/LEVEL0_JOB \
  -e GTA_TUNAATLAS_LEVEL1_CATCH=jobs/LEVEL1_JOB \
  -e GTA_TUNAATLAS_LEVEL2_CATCH=jobs/LEVEL2_JOB \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-reporting:2d93b5a
```

Replace `LEVEL0_JOB`, `LEVEL1_JOB` and `LEVEL2_JOB` with the actual job directory names.

Paths are resolved inside the container. Existing jobs must therefore be accessible through the mounted `jobs/` directory.

## 10. Generate comparison reports

Report generation uses the `gta-reporting` image.

To generate the Level 2 versus nominal comparison from existing jobs:

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime

docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -e GTA_STEPS=reports \
  -e GTA_TUNAATLAS_NOMINAL=jobs/NOMINAL_JOB \
  -e GTA_TUNAATLAS_LEVEL2_CATCH=jobs/LEVEL2_JOB \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-reporting:2d93b5a
```

Replace `NOMINAL_JOB` and `LEVEL2_JOB` with the actual job directory names.

Use matching `gta-workflow` and `gta-reporting` tags when reproducing a specific release.

## 11. Build the images locally

Pre-built GHCR images are recommended when running an existing release.

Build locally when developing or modifying the workflow.

### Workflow image

From the repository root:

```bash
docker build \
  -f docker/Dockerfile.workflow \
  -t gta-workflow:latest \
  .
```

### Reporting image

Build the workflow image first:

```bash
docker build \
  -f docker/Dockerfile.workflow \
  -t gta-workflow:latest \
  .
```

Then build the reporting image:

```bash
docker build \
  -f docker/Dockerfile.reporting \
  --build-arg BASE_IMAGE=gta-workflow:latest \
  -t gta-reporting:latest \
  .
```

## 12. Network access

| Operation                       | Network required |
| ------------------------------- | ---------------- |
| Local directory input           | No               |
| Local archive input             | No               |
| First Zenodo DOI download       | Yes              |
| Reusing a cached Zenodo archive | No               |
| Database publication            | Yes              |
| Building Docker images          | Yes              |

Local-data examples therefore use:

```text
--network none
```

Do not use `--network none` for the first Zenodo download or when database publication is required.

## 13. Outputs and reproducibility

`geoflow` creates job directories under:

```text
/home/rstudio/geoflow-tunaatlas/jobs
```

With the recommended mount, these are persisted under:

```text
runtime/jobs/
```

For an auditable production run, retain:

* the repository commit SHA;
* the Docker image tag or digest;
* the input Zenodo DOI and archive name, or checksum of local inputs;
* the exact `GTA_*` parameters;
* generated job directories;
* execution logs.

Scientific and technical acceptance checks are described in [VALIDATION.md](VALIDATION.md).
