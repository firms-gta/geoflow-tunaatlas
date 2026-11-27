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
they need neither geoflow nor the database. The chain has two halves, which
can be run together or separately:

```r
pkgload::load_all()   # the gta package of this repository

# 1. Raw datasets of the tRFMOs -> harmonised datasets (as dataoutputpreharmo/)
harmonised <- gta_harmonise_raw_datasets(gta_raw_datasets(data_dir = "tests/sample_data"))

# 2. Harmonised datasets -> nominal, level 0, effort, level 1, level 2
atlas <- gta_create_levels(
  harmonised,                                                   # or paths of <id>.csv files
  files = list.files("tests/sample_data", full.names = TRUE),  # conversion factors, code lists...
  steps = c("nominal", "level0", "effort", "level1")
)
atlas$level0          # level 0 georeferenced catch (data frame)

# Both at once
atlas <- create_global_tuna_atlas(
  data = list(raw = gta_raw_datasets(data_dir = "tests/sample_data"),
              files = list.files("tests/sample_data", full.names = TRUE)),
  steps = c("rawdata", "nominal", "level0", "effort", "level1")
)
```

What each level reads and its options are in `gta_default_options()` (the
same as the entity tables of the workflow, `config/entities/`): for each
level, its `datasets`, the `nominal` catch for the level 2, its auxiliary
`files` and its `options`. The files are found by their exact name; a missing
name, or two different files with the same name, is an error. Each step can
also be run on its own: `gta_rawdata_wcpfc_catch_ps()` (and the other
`gta_rawdata_*()`), `gta_tidy_raw_dataset()`, `gta_nominal_catch()`,
`gta_georef_dataset()`, `gta_level1()`.

## How the datasets are made

```text
raw data of the tRFMOs
  └─ rawdata   one function per dataset (gta_rawdata_<tRFMO>_<type>_<dataset>),
               then curation and mapping to the CWP code lists (gta_tidy_raw_dataset)
       ├─ nominal   global nominal catch (gta_nominal_catch)
       ├─ level0    georeferenced catch: tRFMO datasets bound, overlaps handled (gta_georef_dataset)
       ├─ effort    georeferenced effort (gta_georef_dataset)
       ├─ level1    level 0 with units harmonised to tonnes (gta_level1)
       └─ level2   level 0 raised to the nominal catch (gta_georef_dataset)
```

Each raw dataset has its folder in `R/tunaatlas_scripts/pre-harmonization/<tRFMO>/<type>/<dataset>/`,
with its geoflow action, its harmonisation report (`report_*.Rmd`, kept up to
date with its function) and a sample of its data. Details:
[docs/WORKFLOW.md](docs/WORKFLOW.md).

## Repository structure

```text
geoflow-tunaatlas/
├── gta.sh               # the four commands above
├── DESCRIPTION, NAMESPACE, R/*.R, man/   # the gta R package (business logic)
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
pip install pre-commit && pre-commit install   # fast checks at each commit
pre-commit run --hook-stage manual             # R checks, before pushing
Rscript -e 'testthat::test_local()'            # package tests, with a local R (renv)
Rscript -e 'roxygen2::roxygenise()'            # documentation (man/) and NAMESPACE
R CMD build . && R CMD check --no-manual gta_*.tar.gz   # package check
```

The documentation and the `NAMESPACE` are generated by roxygen2 from the
comments of `R/*.R`: the exported functions (`@export`) are the public API
listed above; the other functions are internal (`@noRd`).

The pre-commit hooks are of two kinds. At each commit, for everyone, fast
checks that need no R package: R files parse, configuration paths, secrets,
end of files, large files. By hand (`pre-commit run --hook-stage manual`),
the R checks that need the renv library of the project: tests of the raw
datasets and of the package, `man/` and `NAMESPACE` up to date, reports of
the raw datasets in line with their functions. They do not block a commit:
the [continuous integration](docs/CI.md) runs them on `dev` and `master`,
with `R CMD check` (no error, warning or note) and the whole workflow on the
sample data. The hooks call R through `tests/rscript.sh` (the R of the
RStudio session when there is one). To compare the outputs of two runs (e.g. `dev` and a branch):
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
