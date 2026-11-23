# Global Tuna Atlas data-production workflow

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11563961.svg)](https://doi.org/10.5281/zenodo.11563961)

This repository produces the Global Tuna Atlas (GTA) datasets from the public
data of the five tuna Regional Fisheries Management Organisations (tRFMOs:
CCSBT, IATTC, ICCAT, IOTC, WCPFC): harmonised nominal catch, georeferenced
catch (levels 0, 1 and 2) and fishing effort.

It is both:

* the **`gta` R package** (`DESCRIPTION`, `R/`): all the processing, as plain R
  functions, with `create_global_tuna_atlas()` to run the whole chain;
* the **`geoflow` workflow** around it (`workflow/`, `config/`), which runs these
  functions, loads the PostGIS database and publishes the datasets.

Everything runs in Docker: no local R installation is needed.

## Run it: four commands

You need Docker (and about 30 GB of disk for a complete run).

```bash
./gta.sh test      # tests on the control data (a few minutes)
./gta.sh sample    # whole workflow on the sample data, offline, with the database
./gta.sh produce   # all the datasets from the input archive on Zenodo (no database)
./gta.sh publish   # complete run with the database and the publication
```

| Command | Input | What you get | Time |
| --- | --- | --- | --- |
| `test` | control data of the repository | pass / fail | minutes |
| `sample` | `tests/sample_data` | jobs in `runtime/jobs`, a database in Docker | ~10 min |
| `produce` | archive on Zenodo (downloaded once, cached) | datasets in `runtime/extracted/dataoutputGTA` | hours |
| `publish` | `GTA_DATA_DIR` in `.env` | datasets, database, GeoServer, GeoNetwork, Zenodo | hours |

The first command creates `.env` (the single settings file) from
`env.example`. **Check `.env` before `publish`**: with `RUN_ZENODO_T_F=TRUE`
it deposits on the Zenodo instance of `ZENODO_URL` (a deposit on zenodo.org
creates a permanent DOI); `./gta.sh publish` asks for a confirmation.

Options for a single run are environment variables:

```bash
GTA_STEPS=rawdata,nominal ./gta.sh sample                          # some steps only
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:sha-<commit> ./gta.sh produce  # a fixed image
```

The steps are `DB`, `rawdata`, `nominal`, `effort`, `level0`, `level1`,
`level2`, `services`. More options: [docs/RUNNING_ADVANCED.md](docs/RUNNING_ADVANCED.md)
and [docker/README.md](docker/README.md).

## Use the R package

The `gta` functions take data frames or file paths and return data frames;
they need neither geoflow nor the database. From the repository:

```r
pkgload::load_all()   # the gta package of this repository

atlas <- create_global_tuna_atlas(
  data = list(
    raw   = gta_raw_datasets(data_dir = "tests/sample_data"),          # raw files of the tRFMOs
    files = list.files("tests/sample_data", full.names = TRUE)        # conversion factors, code lists...
  ),
  steps = c("rawdata", "nominal", "level0", "effort", "level1")
)
atlas$level0          # level 0 georeferenced catch (data frame)
atlas$raw$catch_iotc_level0
```

Each step can also be run on its own: `gta_rawdata_wcpfc_catch_ps()` (and the
other `gta_rawdata_*()`), `gta_tidy_raw_dataset()`, `gta_nominal_catch()`,
`gta_georef_dataset()`, `gta_level1()`. The options of the levels and their
defaults are in `gta_default_options()`.

## How the datasets are made

```text
raw data of the tRFMOs
  └─ rawdata   one function per dataset (gta_rawdata_<tRFMO>_<type>_<dataset>),
               then curation and mapping to the CWP code lists (gta_tidy_raw_dataset)
       ├─ nominal   global nominal catch (gta_nominal_catch)
       ├─ level0    georeferenced catch: tRFMO datasets bound, overlaps handled (gta_georef_dataset)
       ├─ effort    georeferenced effort (gta_georef_dataset)
       └─ level1    level 0 with units harmonised to tonnes (gta_level1)
            └─ level2   level 1 raised to the nominal catch (gta_georef_dataset)
```

Each raw dataset has its folder in `R/tunaatlas_scripts/pre-harmonization/<tRFMO>/<type>/<dataset>/`,
with its geoflow action, its harmonisation report (`report_*.Rmd`, kept up to
date with its function) and a sample of its data. Details:
[docs/WORKFLOW.md](docs/WORKFLOW.md).

## Repository structure

```text
geoflow-tunaatlas/
├── gta.sh               # the four commands above
├── DESCRIPTION, NAMESPACE, R/*.R   # the gta R package (business logic)
├── R/tunaatlas_scripts/ # geoflow actions and reports of the raw datasets, other scripts
├── workflow/            # geoflow side: launchers, actions, PostGIS, NetCDF
├── config/              # declarative only: geoflow JSON, entity tables, SQL
├── tests/               # testthat tests (tests/testthat), control and sample data
├── data/                # static reference data
├── docker/              # launch scripts, geoflow patches, database initialisation
├── docs/                # documentation
├── analyses/            # one-off analyses, not part of the workflow
├── Dockerfile, compose.yml, env.example, renv.lock
```

Input data, jobs and outputs are not part of the repository: they are written
to `runtime/` and mounted at run time.

## Develop

```bash
pip install pre-commit && pre-commit install   # checks at each commit
Rscript -e 'testthat::test_local()'            # package tests, with a local R (renv)
```

The pre-commit hooks parse the R files, check the configuration paths, run the
tests of the raw datasets and of the package, and keep the reports of the raw
datasets in line with their functions. The same tests run in the
[continuous integration](docs/CI.md), with the whole workflow on the sample
data. To compare the outputs of two runs (e.g. `dev` and a branch):
`Rscript tests/compare_runs.R <jobs of run A> <jobs of run B>`.

Branches start from `dev` (integration); `master` is the production branch,
which publishes the `latest` image.

## Docker images

Published on the GitHub Container Registry, `ghcr.io/firms-gta/gta-workflow`:

| Tag | Origin |
| --- | --- |
| `sha-<commit>` / `latest` | built and tested by the CI from `master` |
| `sha-<commit>-dev` / `<branch>-dev` | built and tested by the CI from a feature branch |

Use an immutable `sha-…` tag (or a digest) for a reproducible run. The input
data also exist as an image, `ghcr.io/firms-gta/tunaatlas-data`.

## Reproducibility

R dependencies are pinned in `renv.lock`, R and the system libraries in the
Docker image. For an auditable run, keep the repository commit, the image tag
or digest, the input DOI or checksum, the `GTA_*` options and the job folders.
Scientific checks: [docs/VALIDATION.md](docs/VALIDATION.md).

## Documentation

* [Running](docs/RUNNING.md) and [advanced options](docs/RUNNING_ADVANCED.md)
* [Full stack and publication](docker/README.md)
* [Workflow architecture](docs/WORKFLOW.md)
* [Incremental runs with {targets}](docs/TARGETS.md)
* [Validation](docs/VALIDATION.md)
* [Continuous integration](docs/CI.md)
* [Deployment](docs/DEPLOYMENT.md) (SSP Cloud / Kubernetes)

## Licence and citation

The code is under the MIT licence ([LICENSE.md](LICENSE.md)); authors: Bastien
Grasset, Paul Taconet, Julien Barde, Emmanuel Blondel. Each published dataset
has its own licence.

To use Global Tuna Atlas data, cite the DOI of the dataset release, not the
DOI of this repository.
