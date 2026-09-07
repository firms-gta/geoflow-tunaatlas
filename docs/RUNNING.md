# Running the Global Tuna Atlas workflow

This page provides the shortest way to run the Global Tuna Atlas (GTA) production workflow with Docker.

The workflow is intended for **reproducing or updating GTA datasets**.
No local R installation is required.

## Requirements

You need:

* Docker;
* approximately 30 GB of free disk space;
* the GTA raw input data.

Pre-built Docker images are available from the GitHub Container Registry:

```bash
docker pull ghcr.io/firms-gta/gta-workflow:2d93b5a
```

For reproducibility, use a fixed image tag rather than `latest`.

---

# 1. Get the GTA input data

The GTA raw-data archive is available from Zenodo record:

```markdown
The GTA raw-data archive is available from
[Zenodo record 20834708](https://zenodo.org/records/20834708)
(DOI: `10.5281/zenodo.20834708`).
```

The required file is:

```text
all_raw_data_GTA.zip
```

It can be downloaded from the Zenodo web interface or directly with:

```bash
wget -O all_raw_data_GTA.zip \
  "https://zenodo.org/records/20834708/files/all_raw_data_GTA.zip?download=1"
```

Extract it:

```bash
unzip all_raw_data_GTA.zip
```

The directory used by the workflow must contain the raw files directly, for example:

```text
all_raw_data_GTA/
├── iotc_nominal_catch_firms_level0_2026-04-13.csv
├── EF_RAW.csv
├── CEData_Longline.xlsx
└── ...
```

If the ZIP creates an additional `all_raw_data_GTA` directory, simply use that directory in the command below.

---

# 2. Create the output directory

From the directory where you want to run the workflow:

```bash
mkdir -p runtime/jobs
```

Make sure the raw-data directory and output directory are writable by your user:

```bash
chmod -R u+rwX /absolute/path/to/all_raw_data_GTA
chmod -R u+rwX runtime
```

---

# 3. Run the workflow

Replace:

```text
/absolute/path/to/all_raw_data_GTA
```

with the location of your extracted GTA input directory.

Run:

```bash
docker run --rm --network none \
  --user "$(id -u):$(id -g)" \
  -v /absolute/path/to/all_raw_data_GTA:/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -e GTA_STEPS=rawdata,nominal,effort,level0,level1,level2 \
  -e GTA_DATA_SOURCE=volume_dir \
  -e GTA_DATA_PATH=/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -e GTA_SUMMARISE_INVALID_RAW=false \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  ghcr.io/firms-gta/gta-workflow:2d93b5a
```

The input directory is also used as the GTA working directory.

The workflow therefore reads the source files and writes intermediate GTA datasets into the same directory. **Do not mount it with `:ro`.**

`--network none` can be used because the input data and software dependencies are already available locally.

---

# 4. Outputs

`geoflow` jobs, logs and workflow outputs are written to:

```text
runtime/jobs/
```

Intermediate and pre-harmonisation GTA datasets are written inside the mounted data directory, including:

```text
all_raw_data_GTA/dataoutputpreharmo/
```

---

# Run only part of the workflow

The complete production chain above uses:

```text
GTA_STEPS=rawdata,nominal,effort,level0,level1,level2
```

For example, to run only the three pre-harmonisation workflows:

```text
GTA_STEPS=rawdata
```

Main available stages are:

```text
rawdata
nominal
effort
level0
level1
level2
```

Several stages can be supplied as a comma-separated list.

---

# Alternative: let Docker download the data from Zenodo

Instead of downloading and extracting `all_raw_data_GTA.zip` manually, Docker can prepare a writable working directory from Zenodo.

Create:

```bash
mkdir -p runtime/extracted runtime/jobs runtime/cache
```

Then run:

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

Network access is required for the first download.

The downloaded archive is cached under:

```text
runtime/cache/
```

and the extracted GTA working directory is stored under:

```text
runtime/extracted/
```

---

# Using the standalone Zenodo Docker image

A standalone Docker image archive may also be distributed with a GTA release.

If you downloaded:

```text
gta-workflow.tar.gz
```

load it with:

```bash
docker load < gta-workflow.tar.gz
```

Then replace:

```text
ghcr.io/firms-gta/gta-workflow:2d93b5a
```

in the commands above with:

```text
gta-workflow:latest
```

No R installation is required.

---

## Advanced usage

For partial runs, existing-job reuse, reporting, runtime configuration,
local image builds and other advanced execution options, see
[RUNNING_ADVANCED.md](RUNNING_ADVANCED.md).

For the processing architecture and scientific workflow stages, see
[WORKFLOW.md](WORKFLOW.md).
