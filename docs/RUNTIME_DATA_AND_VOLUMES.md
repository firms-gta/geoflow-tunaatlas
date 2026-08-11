# Runtime parameters, data inputs, and persistent volumes

## Standard interface

The Docker entry point reads the following environment variables.

| Variable | Default | Meaning |
|---|---|---|
| `GTA_STEPS` | `rawdata` | Comma-separated workflow stages |
| `GTA_DATA_SOURCE` | `auto` | `auto`, `volume_dir`, `volume_zip`, or `doi` |
| `GTA_DATA_PATH` | empty | Container path to a mounted directory or ZIP archive |
| `GTA_DOI` | empty | Zenodo DOI, record URL, or numeric record ID |
| `GTA_DOI_FILE` | empty | Optional exact archive name when the record is ambiguous |
| `GTA_SUMMARISE_INVALID_RAW` | `false` | Generate invalid-record summaries after raw stages |
| `GTA_STOP_ON_MISSING_INPUTS` | `true` | Stop before processing when a required input is missing |
| `GTA_BOOTSTRAP_RESTORE_RENV` | `false` in Docker | Restore packages at runtime; normally unnecessary in the image |
| `GTA_DOWNLOAD_CACHE_DIR` | `/cache/downloads` in Docker | Persistent cache for downloaded Zenodo archives |

Boolean values accept `true`, `1`, `yes`, or `y`, without regard to case.

## Input resolution order

With `GTA_DATA_SOURCE=auto`, the launcher uses the first available source:

1. `/data/GTA_2026` if it is a directory;
2. `/data/GTA_2026.zip` if it is a file;
3. `GTA_DOI` if supplied;
4. otherwise the run stops with an explicit error.

For predictable production runs, set `GTA_DATA_SOURCE` explicitly.

## Local directory

Mount the directory read-only and point `GTA_DATA_PATH` to the directory inside
the container:

```bash
-v /host/raw-data:/data/GTA_2026:ro \
-v gta-working-input:/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
-e GTA_DATA_SOURCE=volume_dir \
-e GTA_DATA_PATH=/data/GTA_2026
```

The path must directly contain the source files rather than an additional
wrapper directory. The bootstrap copies the top-level files into the writable
working-input volume and applies the pre-harmonisation file replacements there;
the read-only source is not modified.

## Local ZIP archive

```bash
-v /host/all_raw_data_GTA.zip:/data/GTA_2026.zip:ro \
-v gta-extracted-input:/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
-e GTA_DATA_SOURCE=volume_zip \
-e GTA_DATA_PATH=/data/GTA_2026.zip
```

ZIP and `.tar.gz` archives are supported by the R launcher. The automatic
`/data/GTA_2026.zip` convention is ZIP-specific; use `GTA_DATA_PATH` explicitly
for a `.tar.gz` file.

## Zenodo DOI

```bash
-v gta-extracted-input:/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
-v gta-download-cache:/cache \
-e GTA_DATA_SOURCE=doi \
-e GTA_DOI=10.5281/zenodo.20834708
```

The launcher queries the Zenodo record, selects `all_raw_data_GTA.zip` when it
exists, verifies the published MD5 checksum, extracts the archive, and returns
the extracted directory.

If another record contains several possible raw-data archives, set the exact
name:

```bash
-e GTA_DOI_FILE=chosen_raw_archive.zip
```

The first DOI run requires internet access. A validated archive is kept under
`/cache/downloads`; the same mounted cache can be reused for later runs.

## Remote data spaces

EDITO Data Space, object storage, and infrastructure-specific storage clients
are intentionally kept outside the scientific code. Materialise or mount the
remote dataset as a directory or archive, then use the same `volume_dir` or
`volume_zip` interface.

This adapter boundary avoids adding storage credentials or provider-specific
SDKs to the scientific image. A new remote source needs only to produce one of
the two supported filesystem forms.

## Persistent volume architecture

| Category | Container path | Access | Persistence | Contents |
|---|---|---|---|---|
| Mounted input | `/data/GTA_2026` or `/data/GTA_2026.zip` | Read-only when possible | Source-controlled externally | Raw input directory or archive |
| Working input | `/home/rstudio/geoflow-tunaatlas/data/GTA_2026` | Read/write | Required for directory, ZIP, and DOI runs | Copied or extracted inputs plus intermediate data outputs |
| Results | `/home/rstudio/geoflow-tunaatlas/jobs` | Read/write | Required | geoflow jobs, entities, reports, logs |
| Download cache | `/cache/downloads` | Read/write | Recommended | Checksum-verified Zenodo archives |
| Application code | `/home/rstudio/geoflow-tunaatlas` | Read-only in normal operation | Image layer | R code, JSON configurations, static reference files |
| FDI mappings cache | `/opt/fdi-mappings-cache` | Read-only at runtime | Image layer | Mappings pinned during image build |

Do not mount an empty volume over the entire project `data/` directory: it would
hide static reference files included in the image. Mount only the
`data/GTA_2026` subdirectory for extracted runtime inputs.

## File ownership and permissions

The workflow image runs as the unprivileged `rstudio` user. On a Linux host,
either give the mounted result directories to the container user or run with the
current host UID and GID:

```bash
mkdir -p runtime/jobs runtime/cache runtime/extracted
chmod -R u+rwX runtime

docker run --rm \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  gta-workflow:latest
```

On Kubernetes or SSP Cloud, use the platform's non-root security context and
`fsGroup` mechanism instead of granting world-writable permissions.

## Existing job paths for partial reruns

| Environment variable | `existing_paths` entry |
|---|---|
| `GTA_RAW_NOMINAL_CATCH` | `raw_nominal_catch` |
| `GTA_RAW_DATA_GEOREF` | `raw_data_georef` |
| `GTA_RAW_DATA_GEOREF_EFFORT` | `raw_data_georef_effort` |
| `GTA_TUNAATLAS_EFFORT` | `tunaatlas_effort` |
| `GTA_TUNAATLAS_NOMINAL` | `tunaatlas_nominal` |
| `GTA_TUNAATLAS_LEVEL0_CATCH` | `tunaatlas_level0_catch` |
| `GTA_TUNAATLAS_LEVEL1_CATCH` | `tunaatlas_level1_catch` |
| `GTA_TUNAATLAS_LEVEL2_CATCH` | `tunaatlas_level2_catch` |

Paths are resolved inside the container and must therefore be reachable through
a mounted results volume.
