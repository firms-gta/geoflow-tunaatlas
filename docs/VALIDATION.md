# Global Tuna Atlas validation

This document defines the minimum technical and scientific checks required to validate a Global Tuna Atlas workflow run.

For workflow architecture, see [WORKFLOW.md](WORKFLOW.md).

For Docker commands and runtime execution, see [RUNNING.md](RUNNING.md).

## 1. Validation principles

A workflow run should be considered valid only when:

* the selected workflow stages complete without unhandled errors;
* the expected job directories and outputs are created;
* required source authorities and time ranges are present;
* measurement units and processing levels match the expected product;
* dataset totals and record counts are consistent with the approved reference release or documented scientific expectations;
* invalid-record summaries and comparison outputs have been reviewed;
* any accepted deviation is documented.

Technical success alone is not sufficient for scientific validation.

Likewise, scientific output should not be accepted when the runtime environment or input provenance cannot be reproduced.

## 2. Automated repository checks

The repository includes lightweight checks intended to detect launcher and configuration problems before running expensive workflows.

Run:

```bash
Rscript tests/smoke_test_launcher.R
```

These checks cover the launcher interface and basic runtime assumptions, including:

* parsing of the main launcher scripts;
* workflow-stage selection;
* DOI parsing and archive selection;
* expected runtime paths;
* basic input-resolution behaviour.

Relevant checks are also executed through GitHub Actions.

Automated smoke tests do not replace a full production run.

## 3. Docker image validation

Before using a new workflow release for production, verify that the Docker images can be retrieved and started.

For a tagged release:

```bash
docker pull ghcr.io/firms-gta/gta-workflow:2d93b5a
docker pull ghcr.io/firms-gta/gta-reporting:2d93b5a
```

Check that both images are available locally:

```bash
docker images | grep -E 'gta-workflow|gta-reporting'
```

When validating locally built images instead, build them using the commands documented in [RUNNING.md](RUNNING.md).

For reproducible validation, record the exact image tag or immutable digest.

## 4. Input validation

Before running long processing stages, confirm that the selected input package is complete.

For a local directory, a simple container-level inspection can be performed with:

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -v /absolute/path/to/all_raw_data_GTA:/data/GTA_2026:ro \
  ghcr.io/firms-gta/gta-workflow:2d93b5a \
  bash -lc '
    echo "Input directory:"
    ls -lah /data/GTA_2026 | head -50
  '
```

The workflow launcher also performs mandatory input checks before expensive pre-harmonisation stages.

Validation should confirm:

* expected source files are present;
* files are mounted at the expected directory level;
* the intended source-data release is being used;
* stale or unexpected source files are investigated before production.

For DOI-based runs, record:

* Zenodo DOI;
* selected archive name;
* published checksum when available.

## 5. Runtime validation

A runtime scenario is accepted when:

1. the command exits successfully;
2. expected job directories are created;
3. logs contain no unhandled error;
4. expected dataset files are present;
5. the corresponding scientific checks are completed.

A useful minimum runtime matrix is:

| Scenario      | Expected result                                   |
| ------------- | ------------------------------------------------- |
| `raw_nominal` | Raw nominal catch job created                     |
| `raw_georef`  | Raw georeferenced catch job created               |
| `raw_effort`  | Raw fishing-effort job created                    |
| `rawdata`     | All three pre-harmonisation jobs created          |
| `nominal`     | Harmonised nominal dataset created                |
| `effort`      | Harmonised effort dataset created                 |
| `level0`      | Catch Level 0 dataset created                     |
| `level1`      | Catch Level 1 dataset created                     |
| `level2`      | Catch Level 2 dataset created                     |
| `summaries`   | Summary outputs regenerated                       |
| `reports`     | Level 2 versus nominal comparison outputs created |
| `qa_rmd`      | Pre-harmonisation QA documentation regenerated    |

Not every release needs to rerun every scenario independently if the full production chain already provides equivalent evidence.

## 6. Save execution logs

Validation runs should preserve their logs.

For example:

```bash
mkdir -p validation
```

Then run the selected workflow while recording stdout and stderr:

```bash
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -v /absolute/path/to/all_raw_data_GTA:/data/GTA_2026:ro \
  -v "$PWD/runtime/extracted":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
  -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS=rawdata \
  -e GTA_DATA_SOURCE=volume_dir \
  -e GTA_DATA_PATH=/data/GTA_2026 \
  ghcr.io/firms-gta/gta-workflow:2d93b5a \
  2>&1 | tee validation/rawdata.log
```

For long-running production runs, also retain the exact command or environment file used.

## 7. Scientific validation minimum

Every product-producing workflow should be checked at the scientific level.

At minimum, validate:

### Source authorities

Confirm that all expected source authorities are present.

Unexpected missing or additional authorities should be investigated.

### Temporal coverage

Confirm that the expected year range is present.

Check for:

* missing years;
* unexpected future years;
* unexpected truncation of recent years.

### Measurement units

Confirm that units correspond to the intended processing level.

In particular, check transitions between number-based and weight-based measurements where applicable.

### Processing level

Confirm that output records contain the expected processing level and associated metadata.

### Record counts

Compare the number of records with:

* the previous approved release;
* a known reference run;
* or documented expected changes.

Large changes should be explainable.

### Measurement totals

Compare total catch or effort measurements against the previous approved reference.

Differences should be decomposed by relevant dimensions such as:

* source authority;
* species;
* fishing fleet;
* gear type;
* year;
* measurement unit.

Thresholds must come from scientific expectations or an approved reference release.

The deployment layer should not invent arbitrary acceptance thresholds.

## 8. Pre-harmonisation QA

The three raw branches should be reviewed before accepting downstream products.

Check:

* records rejected by input-validation rules;
* records with missing mandatory dimensions;
* unexpected species or gear mappings;
* temporal anomalies;
* invalid spatial information;
* unusually large changes compared with the previous release.

When available, use the outputs generated by:

```text
GTA_STEPS=qa_rmd
```

and the invalid-record summaries produced by the workflow.

Any unexplained increase in invalid records should be investigated before continuing to publication.

## 9. Level 0 validation

For Level 0, confirm that:

* expected georeferenced source authorities are present;
* harmonisation has not unexpectedly removed valid source records;
* spatial and temporal dimensions are preserved;
* measurement units correspond to the expected source-level representation;
* totals remain consistent with the pre-harmonised georeferenced input, apart from documented harmonisation effects.

## 10. Level 1 validation

For Level 1, confirm that:

* measurement-unit transformations were applied as expected;
* conversion factors are available for the records requiring conversion;
* records are not silently dropped because of missing conversion metadata;
* totals before and after conversion are scientifically plausible;
* conversion-related differences can be explained by species, gear, year and source authority.

The Level 1 validation should explicitly quantify the impact of unit conversion.

## 11. Level 2 validation

Level 2 requires particular attention because it raises georeferenced catch against nominal catch.

Validate:

* presence of the expected nominal reference;
* presence of all expected Level 1 dimensions;
* raising factors and their applicable groups;
* records with no valid raising reference;
* differences between Level 2 and nominal catch.

The final Level 2 output should be compared with the harmonised nominal dataset before publication.

## 12. Level 2 versus nominal comparison

Generate the comparison outputs using the reporting workflow described in [RUNNING.md](RUNNING.md).

The comparison should be reviewed at several aggregation levels.

At minimum:

* global total;
* year;
* source authority;
* species;
* fishing fleet when relevant;
* major gear groups when relevant.

The objective is not necessarily exact equality at every aggregation level.

The objective is to identify unexplained discrepancies between:

* nominal declarations;
* georeferenced Level 2 catch;
* expected effects of the raising procedure.

Large discrepancies should be traced back to the relevant source authority, species, fleet, gear or period.

## 13. Fishing-effort validation

The fishing-effort product should be validated separately from catch.

Check:

* expected source authorities;
* year range;
* effort units;
* georeferenced coverage;
* missing or invalid effort measurements;
* major differences relative to the previous approved release.

Catch-based validation thresholds should not automatically be reused for effort.

## 14. Database publication validation

Database publication is optional and should be validated separately from scientific processing.

Before publication, confirm:

* the intended database environment;
* valid connection parameters;
* successful connection test;
* expected destination schema and tables;
* publication is not accidentally targeting another environment.

When the database is unavailable, the workflow should continue without publication if the selected scientific stages do not require database access.

A successful database upload does not replace scientific dataset validation.

## 15. Reproducibility record

For each validated production release, retain:

* repository commit SHA;
* Docker image tag or digest;
* `renv.lock` version or hash;
* pinned external mappings revision;
* input DOI and archive name, or local-input checksum;
* selected `GTA_STEPS`;
* relevant runtime environment variables;
* job directories;
* execution logs;
* validation outputs;
* known deviations and their justification.

This information should be sufficient to identify the exact code, environment, inputs and outputs associated with the release.

## 16. Release checklist

Before publication, confirm:

* [ ] repository smoke tests pass;
* [ ] Docker image version is recorded;
* [ ] input-data release is recorded;
* [ ] required pre-harmonisation jobs completed successfully;
* [ ] nominal dataset validated;
* [ ] effort dataset validated when applicable;
* [ ] Level 0 validated;
* [ ] Level 1 conversion effects reviewed;
* [ ] Level 2 raising reviewed;
* [ ] invalid-record summaries reviewed;
* [ ] Level 2 versus nominal comparison reviewed;
* [ ] unexpected differences documented;
* [ ] execution logs retained;
* [ ] database destination validated before publication;
* [ ] final release metadata recorded.

A release should not be considered validated solely because the workflow completed without an error.
