# Validation plan and test record

## Acceptance principle

A scenario is accepted only when the command exits successfully, expected job
files are present on persistent storage, logs contain no unhandled error, and a
representative scientific output passes the checks already defined by its
geoflow configuration.

Static review alone does not validate a multi-hour scientific run. This document
separates checks performed on the repository from runtime acceptance checks that
must be executed on a Docker host or SSP Cloud with the real input package.

## Repository checks

| Check | Expected result | Current record |
|---|---|---|
| R launcher interface | Main and CLI scripts parse; smoke tests pass | Automated by `workflow-launcher-checks.yml`; run locally with `Rscript tests/smoke_test_launcher.R` |
| DOI identification | DOI, Zenodo URL, and numeric ID resolve to the same record | Covered by launcher smoke tests |
| DOI archive choice | `all_raw_data_GTA.zip` is selected by default | Covered by launcher smoke tests |
| DOI metadata | Record `20834708` exposes `all_raw_data_GTA.zip` | Confirmed against the Zenodo API during documentation audit on 2026-07-21 |
| Docker runtime paths | Input, job, and cache paths are distinct | Checked by CI grep assertions and documented volume table |
| YAML syntax | Compose and Kubernetes templates parse | Checked during documentation audit on 2026-07-21 |
| Shell entry point | Bash syntax is valid | Checked with `bash -n` during documentation audit on 2026-07-21 |
| Documentation links | New repository-local documentation links resolve | Checked during documentation audit on 2026-07-21 |

## Runtime scenario matrix

Complete the final three columns for the image release being delivered.

| ID | Scenario | Expected evidence | Environment | Result | Date/log reference |
|---|---|---|---|---|---|
| W01 | Build `gta-workflow` from a clean checkout | Image built; package-loading smoke test succeeds | Local/CI | Pending execution | |
| W02 | Build `gta-reporting` from W01 | Image built; Pandoc and LaTeX commands available | Local/CI | Pending execution | |
| W03 | `volume_dir` plus `GTA_STEPS=raw_nominal` | One raw nominal job and timing log | Local | Pending execution | |
| W04 | `volume_dir` plus `GTA_STEPS=raw_georef` | One raw georeferenced catch job | Local | Pending execution | |
| W05 | `volume_dir` plus `GTA_STEPS=raw_effort` | One raw effort job | Local | Pending execution | |
| W06 | `volume_zip` plus `GTA_STEPS=rawdata` | Archive extracted; three raw jobs persisted | Local | Pending execution | |
| W07 | DOI `20834708` plus `GTA_STEPS=rawdata` | Archive downloaded, checksum verified, three jobs persisted | Networked Docker host | Pending execution | |
| W08 | Repeat W07 with populated cache | Cached archive reused; no full re-download | Networked Docker host | Pending execution | |
| W09 | `nominal,level0,level1,level2` | Four product jobs created; configured checks succeed | Server-sized host | Pending execution | |
| W10 | `summaries` with existing paths | Available Level 0/1/2 summaries regenerated | Reporting image | Pending execution | |
| W11 | `reports` with nominal and Level 2 paths | Comparison PNG and RDS outputs created | Reporting image | Pending execution | |
| W12 | Database unavailable | Processing continues with database publication disabled | Local | Pending execution | |
| W13 | Valid GTA sandbox database | Upload enabled only after context and test-query validation | Authorised server | Pending execution | |
| W14 | Kubernetes Job from supplied template | Job completes; outputs persist in PVC | SSP Cloud | Pending execution | |
| W15 | Kubernetes partial relaunch | New Job reuses existing PVC jobs and cache | SSP Cloud | Pending execution | |

## Commands for the acceptance run

Use the exact commands in `GTA_2026_Docker_Workflow_Guide.Rmd`. Save logs instead
of relying on terminal history:

```bash
docker run --name gta-validation-W03 ... 2>&1 | tee validation-W03.log
docker inspect gta-validation-W03 > validation-W03-container.json
docker rm gta-validation-W03
```

For Kubernetes:

```bash
kubectl logs job/gta-workflow > validation-W14.log
kubectl get job gta-workflow -o yaml > validation-W14-job.yaml
```

## Scientific validation minimum

For every product-producing scenario:

1. confirm that all expected source authorities are present;
2. confirm that the expected year range is present;
3. confirm that measurement units and processing levels match the target
   product;
4. compare record count and total measurements against the approved reference;
5. inspect invalid-record summaries;
6. inspect Level 2 versus nominal differences before publication;
7. record any accepted deviation and its scientific justification.

Thresholds must come from the approved reference release or domain decision;
they must not be invented by the deployment layer.

## Release record

Attach the following to a completed release:

- repository commit SHA;
- image tags and digests;
- `renv.lock` hash;
- `FDI_MAPPINGS_REF` value;
- DOI and selected Zenodo filename, or local-input checksum;
- completed W01-W15 matrix for applicable scenarios;
- logs and job-file inventory;
- known deviations and approval decision.

## Current limitation

The documentation workspace used for this update does not provide R or Docker,
and it has no authenticated SSP Cloud namespace. Consequently, repository and
metadata checks were performed here, while image builds and scientific runtime
scenarios remain deliberately marked pending. The included CI workflow and
runtime matrix make those checks reproducible on the intended infrastructure.
