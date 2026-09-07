# Advanced Global Tuna Atlas workflow execution

This document describes advanced execution options for the Global Tuna Atlas (GTA) workflow, including partial runs, runtime configuration, reuse of existing jobs, reporting and local Docker builds.

For a standard production run, see [RUNNING.md](RUNNING.md).

For the processing architecture and scientific workflow stages, see [WORKFLOW.md](WORKFLOW.md).

---

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

---

## 2. Runtime parameters

The Docker entry point is configured using environment variables.

| Variable                     | Default           | Meaning                                                       |
| ---------------------------- | ----------------- | ------------------------------------------------------------- |
| `GTA_STEPS`                  | `rawdata`         | Comma-separated workflow stages to execute                    |
| `GTA_DATA_SOURCE`            | `auto`            | Input mode: `auto`, `volume_dir`, `volume_zip`, or `doi`      |
| `GTA_DATA_PATH`              | empty             | Container path to a mounted data directory or ZIP archive     |
| `GTA_DOI`                    | empty             | Zenodo DOI, record URL, or numeric record ID                  |
| `GTA_DOI_FILE`               | empty             | Exact file to use when a Zenodo record contains several files |
| `GTA_SUMMARISE_INVALID_RAW`  | `false`           | Generate invalid-record summaries after raw stages            |
| `GTA_STOP_ON_MISSING_INPUTS` | `true`            | Stop before processing when required inputs are missing       |
| `GTA_BOOTSTRAP_RESTORE_RENV` | `false` in Docker | Restore R packages at runtime; normally unnecessary           |

Boolean values accept `true`, `1`, `yes`, or `y`, case-insensitively.

---

## 3. Workflow stages

The workflow can be run completely or stage by stage.

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

Several stages can be selected in the same run:

```text
GTA_STEPS=rawdata,nominal,effort,level0,level1,level2
```

Stage dependencies and corresponding `geoflow` configurations are documented in [WORKFLOW.md](WORKFLOW.md).

---

## 4. Data source modes

The workflow supports three explicit input modes.

### Local directory: `volume_dir`

With `volume_dir`, the mounted GTA directory is used directly as the workflow working directory.

For example:

```bash
-v /absolute/path/to/all_raw_data_GTA:/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
-e GTA_DATA_SOURCE=volume_dir \
-e GTA_DATA_PATH=/home/rstudio/geoflow-tunaatlas/data/GTA_2026
```

The directory must be writable because the workflow reads the source datasets and writes generated and intermediate datasets into the same `GTA_2026` tree.

Do not mount this directory with `:ro`.

### Local ZIP archive: `volume_zip`

With `volume_zip`, the source archive is mounted read-only and extracted into a writable GTA working directory:

```bash
-v /absolute/path/to/all_raw_data_GTA.zip:/data/GTA_2026.zip:ro \
-v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
-e GTA_DATA_SOURCE=volume_zip \
-e GTA_DATA_PATH=/data/GTA_2026.zip
```

### Zenodo: `doi`

With `doi`, the archive is downloaded from Zenodo and prepared in the writable GTA working directory.

For the current GTA raw-data record:

```text
GTA_DOI=10.5281/zenodo.20834708
GTA_DOI_FILE=all_raw_data_GTA.zip
```

The first download requires network access.

---

## 5. Runtime directories

Depending on the selected data source, the workflow can use the following persistent directories:

| Host directory                             | Container path                                  | Purpose                             |
| ------------------------------------------ | ----------------------------------------------- | ----------------------------------- |
| local GTA directory or `runtime/extracted` | `/home/rstudio/geoflow-tunaatlas/data/GTA_2026` | Writable GTA working tree           |
| `runtime/jobs`                             | `/home/rstudio/geoflow-tunaatlas/jobs`          | Workflow jobs, logs and job outputs |
| `runtime/cache`                            | `/cache`                                        | Download cache                      |

For ZIP and Zenodo runs:

```bash
mkdir -p runtime/extracted/dataoutputpreharmo runtime/jobs runtime/cache

sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

For `volume_dir`, the mounted local data directory must also be writable by the user running the container.

Do not mount an empty volume over the complete project `data/` directory. Static reference files required by the workflow are included in the image.

---

## 6. Partial workflow runs

Individual stages or groups of stages can be selected using `GTA_STEPS`.

For example, to run only pre-harmonisation:

```text
GTA_STEPS=rawdata
```

To run nominal catch and catch Levels 0–2:

```text
GTA_STEPS=nominal,level0,level1,level2
```

To run only Level 2:

```text
GTA_STEPS=level2
```

Some downstream stages depend on resources produced by upstream `geoflow` jobs.

Skipping an upstream stage is safe only when the expected resource already exists and is available at the path referenced by the downstream configuration.

The launcher does not automatically rewrite scientific `geoflow` configuration metadata.

---

## 7. Reuse existing jobs

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

Paths are resolved inside the container. Existing jobs must therefore be accessible through the mounted `jobs/` directory.

For example:

```text
GTA_TUNAATLAS_LEVEL0_CATCH=jobs/LEVEL0_JOB
GTA_TUNAATLAS_LEVEL1_CATCH=jobs/LEVEL1_JOB
GTA_TUNAATLAS_LEVEL2_CATCH=jobs/LEVEL2_JOB
```

Replace the placeholders with the actual job directory names.

---

## 8. Regenerate summaries from existing jobs

Summary generation uses the reporting image.

Create the required directories:

```bash
mkdir -p \
  runtime/extracted/dataoutputpreharmo \
  runtime/jobs \
  runtime/cache \
  runtime/cwp-summary-figures/Summary

sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime
```

Run:

```bash
docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -v "$PWD/runtime/cwp-summary-figures":/home/rstudio/geoflow-tunaatlas/renv/library/R-4.2/x86_64-pc-linux-gnu/CWP.dataset/rmd/Figures \
  -e GTA_STEPS=summaries \
  -e GTA_DATA_SOURCE=volume_dir \
  -e GTA_DATA_PATH=/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -e GTA_TUNAATLAS_LEVEL0_CATCH=jobs/LEVEL0_JOB \
  -e GTA_TUNAATLAS_LEVEL1_CATCH=jobs/LEVEL1_JOB \
  -e GTA_TUNAATLAS_LEVEL2_CATCH=jobs/LEVEL2_JOB \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-reporting:2d93b5a
```

Replace `LEVEL0_JOB`, `LEVEL1_JOB` and `LEVEL2_JOB` with the corresponding job directory names.

---

## 9. Generate comparison reports

Report generation uses `gta-reporting`.

To generate the Level 2 versus nominal comparison from existing jobs:

```bash
mkdir -p runtime/extracted/dataoutputpreharmo runtime/jobs runtime/cache

sudo chown -R "$(id -u):$(id -g)" runtime
chmod -R u+rwX runtime

docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=reports \
  -e GTA_DATA_SOURCE=volume_dir \
  -e GTA_DATA_PATH=/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -e GTA_TUNAATLAS_NOMINAL=jobs/NOMINAL_JOB \
  -e GTA_TUNAATLAS_LEVEL2_CATCH=jobs/LEVEL2_JOB \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-reporting:2d93b5a
```

Replace `NOMINAL_JOB` and `LEVEL2_JOB` with the actual job directory names.

Use matching `gta-workflow` and `gta-reporting` tags when reproducing a specific release.

---

## 10. Network access

| Operation                       | Network required |
| ------------------------------- | ---------------- |
| Local directory input           | No               |
| Local ZIP input                 | No               |
| First Zenodo DOI download       | Yes              |
| Reusing a cached Zenodo archive | No               |
| Database publication            | Yes              |
| Building Docker images          | Yes              |

Local-data runs can therefore use:

```text
--network none
```

Do not use `--network none` for the first Zenodo download or when database publication is required.

---

## 11. Build the images locally

Pre-built GHCR images are recommended for reproducing an existing release.

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

Build the workflow image first, then:

```bash
docker build \
  -f docker/Dockerfile.reporting \
  --build-arg BASE_IMAGE=gta-workflow:latest \
  -t gta-reporting:latest \
  .
```

---

## 12. Outputs and reproducibility

`geoflow` job directories are written under:

```text
/home/rstudio/geoflow-tunaatlas/jobs
```

With the standard mount, they are persisted under:

```text
runtime/jobs/
```

Pre-harmonisation and intermediate datasets are written to the active `GTA_2026` working directory.

For `volume_dir`, this is the mounted local data directory.

For ZIP or Zenodo runs using `runtime/extracted`, they are persisted under:

```text
runtime/extracted/
```

including pre-harmonisation datasets under:

```text
runtime/extracted/dataoutputpreharmo/
```

The complete runtime can be located on another disk or external volume when local disk space is limited. Replace the host-side `$PWD/runtime/...` paths consistently while keeping the container paths unchanged.

For an auditable production run, retain:

* the repository commit SHA;
* the Docker image tag or digest;
* the input Zenodo DOI and archive name, or checksum of local inputs;
* the exact `GTA_*` parameters;
* generated job directories;
* execution logs.

Scientific and technical acceptance checks are described in [VALIDATION.md](VALIDATION.md).
