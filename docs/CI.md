# Continuous integration

How the workflow is tested on GitHub Actions, and how the tested image is
published to the GitHub Container Registry (GHCR).

## Workflows

| Workflow file | Trigger | What it checks |
| --- | --- | --- |
| `workflow-launcher-checks.yml` | pull requests touching the launcher or the Dockerfile | launcher scripts parse, `tests/smoke_test_launcher.R`, required patches present in `compose/Dockerfile.workflow` |
| `test_whole_compose.yml` | push on `master` and the feature branch, manual | end-to-end run of the full stack on the sample data, then publication of the tested image |

## End-to-end test (`test_whole_compose.yml`)

1. **Build** `compose/Dockerfile.workflow` from the commit (tag
   `gta-workflow:ci-<sha>`, local to the runner). Layers are cached between runs:
   when only R code changes, `renv::restore()` is not re-run.
2. **Prepare the runner**: write `docker_local.env.compose` and
   `zenodo_secrets.env`, create the output folders of the sample data, give the
   mounted folders to uid 1000 (the `rstudio` user of the image).
3. **Run** `./compose/run_workflow_retry.sh` with
   `GTA_STEPS=DB,rawdata,nominal,effort,level0,services`,
   `GTA_RUN_USER=1000:1000` and `GTA_MOUNT_CODE=false`, so the code inside the
   image is tested, not the checkout. With `CI=true`, the script adds
   `compose/compose.ci.yml` (smaller Elasticsearch heap).
4. **On failure**, print the logs of all services.
5. **On success of a push**, publish the image that was just tested.

Covered: database model and code lists, the three pre-harmonisation branches,
nominal catch, effort, Level 0, and publication to PostGIS, GeoServer,
GeoNetwork and **sandbox** Zenodo.

The containers exist only on the runner during the job: GeoServer and
GeoNetwork cannot be opened from outside. Checks must run inside the job, and
anything worth keeping must be uploaded with `actions/upload-artifact`.

## Secrets and test data

| Item | Where | Notes |
| --- | --- | --- |
| `ZENODO_SANDBOX_TOKEN` | repository *Settings → Secrets and variables → Actions* | token from **sandbox.zenodo.org** (`deposit:write`, `deposit:actions`). The URL is hard-coded to the sandbox, so the CI never deposits on zenodo.org. |
| `GITHUB_TOKEN` | automatic | pushes to GHCR (`permissions: packages: write`) |
| `tests/sample_data/` | repository | a few rows of each raw file; regenerate with `Rscript tests/make_sample_data.R` from `runtime/extracted/all_raw_data_GTA` |

A missing secret does not fail the job by itself: it becomes an empty string,
which then shows up as a Zenodo URL or authentication error.

## Published image

Only after a successful run triggered by a push; the image is the one that was
tested (re-tagged, not rebuilt).

Package: `ghcr.io/firms-gta/gta-workflow`

| Branch | Tags |
| --- | --- |
| `master` | `latest`, `sha-<7 chars>` |
| any other branch | `<branch-name>-dev`, `sha-<7 chars>-dev` (`/` replaced by `-`) |

Labels: `org.opencontainers.image.source` (links the package to this
repository) and `org.opencontainers.image.revision` (commit SHA).

`denied: permission_denied: write_package` means the repository cannot write to
the package: on the package page, *Package settings → Manage Actions access →
add `geoflow-tunaatlas` with the **Write** role*. Making the package public only
affects reading.

## Reproducing the CI locally

```bash
docker build -f compose/Dockerfile.workflow -t gta-workflow:local .
CI=true GTA_IMAGE=gta-workflow:local GTA_MOUNT_CODE=false ./compose/run_workflow_retry.sh
```

See [compose/README.md](../compose/README.md) for the scripts and their options.
