# GTA 2026 technical workflow

## Scope

The workflow transforms source datasets published by the five tuna Regional
Fisheries Management Organisations into harmonised Global Tuna Atlas catch and
effort products. It also produces validation material, summaries, comparison
reports, and optional database publications.

The operational entry points are:

- `R/launching_workflows/GTA_2026_creation.R`: workflow API and interactive
  launcher;
- `R/launching_workflows/run_gta_2026_workflow_cli.R`: Docker and command-line
  adapter based on environment variables;
- `R/launching_workflows/workflow_helpers.R`: input checks, database fallback,
  workflow execution, and reporting helpers.

## Processing architecture

```text
Input acquisition
  -> mandatory input checks
  -> pre-harmonisation
       -> raw nominal catch
       -> raw georeferenced catch
       -> raw georeferenced effort
  -> harmonised products
       -> nominal catch
       -> effort
       -> catch Level 0
       -> catch Level 1
       -> catch Level 2
  -> summaries and Level 2/nominal comparison reports
  -> optional publication to the validated GTA database
```

Each scientific stage remains configured by a dedicated geoflow JSON file. The
launcher controls which configurations are executed and returns the paths of the
jobs created during the run.

## Functional stages

| Stage | Selector | Main configuration | Purpose |
|---|---|---|---|
| Input validation | automatic | Three pre-harmonisation JSON files | Fail early when required source files are missing and warn about old files |
| Raw nominal | `raw_nominal` | `config/Nominal_catch_2026.json` | Source-specific nominal catch preparation |
| Raw georeferenced catch | `raw_georef` | `config/All_raw_data_georef.json` | Source-specific spatial catch preparation |
| Raw georeferenced effort | `raw_effort` | `config/All_raw_data_georef_effort.json` | Source-specific fishing-effort preparation |
| All raw branches | `rawdata` | The three files above | Run the complete pre-harmonisation group |
| Harmonised effort | `effort` | `config/create_effort_dataset_2026.json` | Create the effort product |
| Harmonised nominal | `nominal` | `config/create_nominal_dataset_2026.json` | Create the nominal reference product |
| Catch Level 0 | `level0` | `config/catch_ird_level0_local.json` | Produce the Level 0 catch product |
| Catch Level 1 | `level1` | `config/catch_ird_level1_local.json` | Standardise units and apply GTA harmonisation rules |
| Catch Level 2 | `level2` | `config/catch_ird_level2_local.json` | Raise georeferenced catch against nominal catch |
| Summaries | `summaries` | Level 0/1/2 configurations | Recreate summary outputs from current or existing jobs |
| Comparisons | `reports` | Existing nominal and Level 2 jobs | Compare nominal and raised georeferenced totals |
| QA documentation | `qa_rmd` | Existing raw job paths | Regenerate pre-harmonisation function documentation |

## Modular execution

`run_gta_workflow()` accepts a character vector through `steps_to_run`. The CLI
converts the comma-separated `GTA_STEPS` value to this vector.

Examples:

```r
source("R/launching_workflows/GTA_2026_creation.R")

run_gta_workflow(
  steps_to_run = c("raw_nominal"),
  data_source = "volume_dir",
  data_path = "/absolute/path/to/all_raw_data_GTA",
  bootstrap_restore_renv = FALSE
)
```

```r
run_gta_workflow(
  steps_to_run = c("summaries"),
  data_source = "volume_dir",
  data_path = "/absolute/path/to/all_raw_data_GTA",
  bootstrap_restore_renv = FALSE,
  existing_paths = list(
    tunaatlas_level0_catch = "jobs/LEVEL0_JOB",
    tunaatlas_level1_catch = "jobs/LEVEL1_JOB",
    tunaatlas_level2_catch = "jobs/LEVEL2_JOB"
  )
)
```

Existing paths can also be passed with `GTA_TUNAATLAS_LEVEL0_CATCH`,
`GTA_TUNAATLAS_LEVEL1_CATCH`, and the other variables listed in
[RUNTIME_DATA_AND_VOLUMES.md](RUNTIME_DATA_AND_VOLUMES.md).

### Partial rerun boundary

The launcher can independently select stages and can reuse existing jobs for
summaries, reports, and QA documentation. Some production configurations still
refer to their upstream resources through geoflow metadata. Therefore, skipping
an upstream production stage is safe only when its expected resource already
exists at the path referenced by the downstream configuration. The launcher
does not silently rewrite scientific JSON metadata.

This boundary is deliberate: modifying a job path must not also modify the
scientific definition of the dataset. A future improvement can introduce a
validated configuration-templating layer for upstream job references.

## Database behaviour

Database access is optional. `should_upload_to_db()` enables publication only
when all of the following are true:

- the configured driver, host, port, database, and user match the expected GTA
  production or sandbox context;
- the DBI connection is valid;
- a test query succeeds.

If DBI initialisation fails, the workflow writes a temporary `_nodb.json`
configuration with database software and publication actions disabled. The
source JSON is not modified.

Secrets must be supplied through environment variables or the infrastructure's
secret mechanism. They must never be copied into the image or committed.

## Reproducibility controls

- `renv.lock` pins R packages.
- `rocker/r-ver:4.2.3` pins the R runtime used by the workflow image.
- `FDI_MAPPINGS_REF` pins the mappings revision cached during the image build.
- source code is copied only after the dependency layer, which improves Docker
  cache reuse without weakening dependency pinning.
- input validation runs before long scientific stages.
- Zenodo downloads use the checksum published in record metadata when present.
- the jobs directory remains outside the container when a persistent volume is
  mounted.

## Image separation

`docker/Dockerfile.workflow` contains the runtime needed for scientific data
processing. `docker/Dockerfile.reporting` extends it with Pandoc, LaTeX, and
fonts. This avoids maintaining separate scientific environments while keeping
the heavier report toolchain out of processing-only deployments.

Raw datasets are runtime inputs and are not baked into either image.

## Logs and outputs

geoflow writes one job directory per execution under `jobs/`. The launcher also
prints the selected stages and returns an invisible named list containing the
input directory and every output path created during the run.

For an auditable run, preserve:

- the image tag or immutable digest;
- the repository commit SHA;
- the exact launch command or environment-variable file;
- the Zenodo DOI and selected archive name, or a checksum of local inputs;
- the full job directories and logs;
- the validation checklist completed for the run.
