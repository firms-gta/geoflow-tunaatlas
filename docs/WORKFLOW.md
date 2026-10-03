# Global Tuna Atlas workflow

This document describes the processing architecture of the Global Tuna Atlas data-production workflow.

For installation, Docker commands, runtime parameters, input methods and partial reruns, see [RUNNING.md](RUNNING.md).

For technical and scientific acceptance checks, see [VALIDATION.md](VALIDATION.md).

## 1. Scope

The Global Tuna Atlas workflow transforms source datasets published by the five tuna Regional Fisheries Management Organisations (tRFMOs) into harmonised catch and fishing-effort products.

The workflow covers:

* source-data preparation;
* pre-harmonisation of nominal catch, georeferenced catch and fishing effort;
* creation of harmonised nominal and effort datasets;
* production of catch Levels 0, 1 and 2;
* dataset summaries and QA outputs;
* comparison of Level 2 georeferenced catch against nominal catch;
* optional publication to the GTA database.

The scientific processing logic remains defined through dedicated `geoflow` configuration files.

The launcher provides a common interface for selecting and executing these processing stages.

## 2. Processing architecture

The overall processing chain is:

```text
Raw tRFMO source data
        │
        ▼
Input validation
        │
        ▼
Pre-harmonisation
├── Nominal catch
├── Georeferenced catch
└── Georeferenced fishing effort
        │
        ▼
Harmonised products
├── Nominal catch
├── Fishing effort
├── Catch Level 0
├── Catch Level 1
└── Catch Level 2
        │
        ▼
Quality control and derived outputs
├── Dataset summaries
├── Pre-harmonisation QA documentation
└── Level 2 versus nominal comparison
        │
        ▼
Optional database publication
```

The workflow can execute the full chain or selected stages.

## 3. Entry points

The main launcher code is located under:

```text
R/launching_workflows/
```

The principal entry points are:

| File                          | Purpose                                                                 |
| ----------------------------- | ----------------------------------------------------------------------- |
| `GTA_2026_creation.R`         | Main workflow API and interactive launcher                              |
| `run_gta_2026_workflow_cli.R` | Command-line and Docker interface                                       |
| `workflow_helpers.R`          | Input preparation, validation, database handling and workflow utilities |

The main R interface is:

```r
run_gta_workflow()
```

The Docker CLI translates environment variables such as `GTA_STEPS` and `GTA_DATA_SOURCE` into arguments for this function.

Runtime usage is documented in [RUNNING.md](RUNNING.md).

## 4. Workflow stages

Each production stage is associated with one or more `geoflow` configurations.

| Stage                     | `GTA_STEPS` selector | Main configuration                        | Purpose                                                               |
| ------------------------- | -------------------- | ----------------------------------------- | --------------------------------------------------------------------- |
| Input validation          | automatic            | Pre-harmonisation configurations          | Verify required source files before long processing stages            |
| Raw nominal catch         | `raw_nominal`        | `config/Nominal_catch_2026.json`          | Prepare source-specific nominal catch data                            |
| Raw georeferenced catch   | `raw_georef`         | `config/All_raw_data_georef.json`         | Prepare source-specific spatial catch data                            |
| Raw fishing effort        | `raw_effort`         | `config/All_raw_data_georef_effort.json`  | Prepare source-specific georeferenced fishing-effort data             |
| All raw branches          | `rawdata`            | Three configurations above                | Run the complete pre-harmonisation group                              |
| Harmonised nominal catch  | `nominal`            | `config/create_nominal_dataset_2026.json` | Produce the harmonised nominal reference dataset                      |
| Harmonised fishing effort | `effort`             | `config/create_effort_dataset_2026.json`  | Produce the harmonised fishing-effort dataset                         |
| Catch Level 0             | `level0`             | `config/catch_ird_level0_local.json`      | Produce harmonised georeferenced catch before Level 1 transformations |
| Catch Level 1             | `level1`             | `config/catch_ird_level1_local.json`      | Standardise measurement units and apply GTA Level 1 processing        |
| Catch Level 2             | `level2`             | `config/catch_ird_level2_local.json`      | Raise georeferenced catch against nominal catch                       |
| Summaries                 | `summaries`          | Product configurations / existing jobs    | Regenerate dataset summaries                                          |
| Comparison reports        | `reports`            | Existing nominal and Level 2 jobs         | Compare Level 2 outputs with nominal catch                            |
| QA documentation          | `qa_rmd`             | Existing raw jobs                         | Generate pre-harmonisation QA documentation                           |
| Complete production       | `all`                | All applicable configurations             | Execute the full production chain                                     |

Several stages can be requested in a single run.

The launcher preserves the scientific definition contained in each `geoflow` configuration rather than rebuilding those configurations dynamically.

## 5. Pre-harmonisation

Pre-harmonisation is divided into three independent branches.

### Nominal catch

The nominal branch prepares nominal catch declarations from the source tRFMO datasets.

Its output is subsequently used to create the harmonised nominal dataset and as a reference for later processing, including Level 2 raising.

### Georeferenced catch

The georeferenced catch branch prepares spatially resolved catch observations from the source organisations.

These data form the basis of the Level 0–2 catch products.

### Fishing effort

The effort branch prepares georeferenced fishing-effort observations.

Catch and fishing effort are processed separately because their source structures, variables and harmonisation rules differ.

Running:

```text
GTA_STEPS=rawdata
```

executes all three branches.

## 6. Harmonised nominal catch

The harmonised nominal dataset provides the global nominal catch reference used by the GTA workflow.

The nominal processing stage standardises the outputs from the pre-harmonisation branch into the GTA common data model.

This dataset is also an important reference for:

* checking consistency between nominal and georeferenced catch;
* Level 2 raising;
* final comparison and validation.

## 7. Catch processing levels

The GTA georeferenced catch workflow is organised into three processing levels.

### Level 0

Level 0 creates the harmonised georeferenced catch product from the pre-harmonised source datasets.

It represents the first common-format catch dataset before the additional transformations applied at Levels 1 and 2.

### Level 1

Level 1 standardises measurements and applies the GTA processing rules required to express georeferenced catches consistently.

This stage includes measurement-unit harmonisation and associated conversion procedures where required.

Level 1 does not perform the global raising applied at Level 2.

### Level 2

Level 2 raises the harmonised georeferenced catch against the corresponding nominal catch reference.

The objective is to produce georeferenced catch totals that account for the difference between available spatial catch information and the corresponding nominal declarations according to the GTA Level 2 methodology.

Level 2 outputs must therefore be checked against the harmonised nominal dataset before publication.

## 8. Fishing-effort product

Fishing effort is processed independently from the catch Levels 0–2.

The `effort` stage consumes the pre-harmonised effort branch and produces a harmonised fishing-effort dataset using the GTA common structure.

Its processing logic is defined in:

```text
config/create_effort_dataset_2026.json
```

## 9. Dependencies between stages

Workflow stages can be selected independently, but scientific dependencies still exist between products.

A simplified dependency graph is:

```text
raw_nominal
    │
    ▼
 nominal ─────────────────────┐
                              │
raw_georef                    │
    │                         │
    ▼                         │
 level0                       │
    │                         │
    ▼                         │
 level1                       │
    │                         │
    └──────────────┐          │
                   ▼          ▼
                     level2
                       │
                       ▼
                    reports


raw_effort
    │
    ▼
  effort
```

Some configurations refer to upstream job resources through their `geoflow` metadata.

For this reason, selecting a downstream stage without rerunning its upstream stage requires the expected upstream resource to already exist and be accessible.

The launcher does not silently modify scientific configuration metadata to redirect dependencies.

Existing job reuse is documented in [RUNNING.md](RUNNING.md).

## 10. Input validation

Before expensive processing starts, the launcher checks that the inputs required by the selected pre-harmonisation configurations are available.

The validation layer is intended to:

* fail early when mandatory source files are absent;
* identify incorrect input-directory structures;
* warn about unexpected or outdated source files where relevant;
* avoid starting multi-hour processing runs with incomplete inputs.

Input validation does not replace the scientific validation performed by the individual `geoflow` workflows.

## 11. Summaries and QA outputs

The workflow can generate additional outputs without rerunning the complete processing chain.

### Summaries

The `summaries` stage regenerates summary products from existing Level 0, Level 1 and Level 2 job outputs.

This is useful when the scientific datasets already exist but summary material needs to be recreated.

### Pre-harmonisation QA

The `qa_rmd` stage regenerates QA documentation associated with the raw nominal, georeferenced catch and fishing-effort workflows.

These outputs document the behaviour and results of pre-harmonisation processing.

## 12. Level 2 versus nominal comparison

The `reports` stage compares the final Level 2 georeferenced catch product against the harmonised nominal catch dataset.

This comparison is an important final consistency check because Level 2 uses nominal catch as its raising reference.

The comparison outputs are intended to help identify:

* unexpected differences between Level 2 and nominal totals;
* missing or inconsistent dimensions;
* anomalous results requiring scientific investigation before publication.

Report-generation dependencies are provided by the `gta-reporting` Docker image.

Execution examples are documented in [RUNNING.md](RUNNING.md).

## 13. Database publication

Database publication is optional.

The scientific processing chain can run without access to the GTA database.

Publication is enabled only when the launcher confirms that:

* the expected database context is configured;
* the required connection parameters are present;
* a DBI connection can be established;
* a test query succeeds.

When the database is unavailable or the context is not validated, the workflow disables publication rather than preventing the scientific processing stages from running.

Temporary database-free configurations may be generated during execution, but the source `geoflow` JSON configurations are not modified.

Database credentials must never be stored in the repository or embedded in Docker images.

## 14. Reproducibility

The workflow uses several mechanisms to improve reproducibility.

### R environment

R package versions are pinned through:

```text
renv.lock
```

### R runtime

The workflow Docker environment uses a fixed R runtime.

### External mappings

External mapping resources required by the workflow are pinned to a specific revision during the Docker build.

### Runtime inputs

Raw data are kept separate from the Docker image and supplied at runtime.

This makes it possible to record independently:

* the source-code revision;
* the runtime image;
* the input-data release;
* the generated outputs.

### Persistent outputs

`geoflow` jobs are written outside the disposable container layer when the recommended persistent volumes are used.

For an auditable production run, retain:

* repository commit SHA;
* Docker image tag or digest;
* input DOI or checksum;
* selected workflow stages;
* generated job directories;
* execution logs;
* validation results.

## 15. Repository components

The main workflow-related repository components are:

```text
R/
├── launching_workflows/
└── ...

config/
├── Nominal_catch_2026.json
├── All_raw_data_georef.json
├── All_raw_data_georef_effort.json
├── create_nominal_dataset_2026.json
├── create_effort_dataset_2026.json
├── catch_ird_level0_local.json
├── catch_ird_level1_local.json
└── catch_ird_level2_local.json

docker/
├── Dockerfile.workflow
└── Dockerfile.reporting
```

The R launcher orchestrates execution, while the `geoflow` configurations remain the source of truth for the scientific processing definitions.

## 16. Related documentation

* [README](../README.md) — project overview and quick start
* [RUNNING.md](RUNNING.md) — Docker images, input data, runtime parameters and execution examples
* [VALIDATION.md](VALIDATION.md) — technical and scientific validation
