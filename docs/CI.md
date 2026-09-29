# Continuous integration

This page describes how the workflow is tested on GitHub Actions and how the
tested Docker image is published to the GitHub Container Registry (GHCR).

## 1. Workflows

| Workflow file | Trigger | What it checks |
| --- | --- | --- |
| `workflow-launcher-checks.yml` | pull requests touching the launcher | R launcher parses, `tests/smoke_test_launcher.R` (Zenodo helpers, required variables), key lines of `compose/Dockerfile.workflow` |
| `test_whole_compose.yml` | push on `master` and the feature branch, manual | End-to-end run of the full stack on the sample data, then publication of the tested image |

The smoke tests take seconds and need no Docker; the end-to-end test builds the
image and runs the whole stack.

## 2. End-to-end test (`test_whole_compose.yml`)

1. **Build** `compose/Dockerfile.workflow` from the commit, tagged
   `gta-workflow:ci-<sha>` (local to the runner). Layers are cached between runs
   (`cache-from/to: type=gha`): when only R code changes, `renv::restore()` is
   not re-run.
2. **Prepare the runner**: generate `docker_local.env.compose` and
   `zenodo_secrets.env`, create the output folders of the sample data, and give
   the mounted folders to uid `1000` (the `rstudio` user of the image; the runner
   user is `1001`).
3. **Run** `./compose/run_workflow_retry.sh` with
   `GTA_STEPS=DB,rawdata,nominal,level0,services` and `GTA_RUN_USER=1000:1000`.
   Because `CI=true`, the script adds `compose/compose.ci.yml` (smaller
   Elasticsearch heap).
4. **On failure**, print the logs of all services.
5. **On success of a push**, publish the image that was just tested (see §4).

What the test covers: loading the database model and code lists, the three
pre-harmonisation branches, harmonised nominal catch, Level 0, and publication
to PostGIS, GeoServer, GeoNetwork and **sandbox** Zenodo.

The containers only exist on the runner during the job: GeoServer and
GeoNetwork are not reachable from outside. Checks must therefore be done inside
the job (e.g. `curl` against `localhost:8080`), and anything worth keeping must
be exported with `actions/upload-artifact` (for instance
`runtime/jobs/*/job-logs.txt`).

## 3. Secrets and test data

| Item | Where | Notes |
| --- | --- | --- |
| `ZENODO_SANDBOX_TOKEN` | Repository *Settings → Secrets and variables → Actions* | Token created on **sandbox.zenodo.org** with `deposit:write` and `deposit:actions`. The URL is hard-coded to the sandbox in the workflow, so the CI can never deposit on zenodo.org. |
| `GITHUB_TOKEN` | provided automatically | Used to push to GHCR (`permissions: packages: write`) |
| `tests/sample_data/` | repository | Small extract of the raw files, regenerated with `Rscript tests/make_sample_data.R` (see [COMPOSE_STACK.md](COMPOSE_STACK.md#3-test-run-on-the-sample-data)) |

A missing secret does not raise an error in GitHub Actions: it is replaced by an
empty string, which then shows up as an authentication or URL error in the
Zenodo step.

## 4. Published image

The image is pushed only after a successful run triggered by a push, and it is
exactly the image that was tested (re-tagged, not rebuilt).

Package: `ghcr.io/firms-gta/geoflow-tunaatlas`

| Branch | Tags |
| --- | --- |
| `master` | `latest`, `sha-<7 chars>` |
| any other branch | `<branch-name>-dev`, `sha-<7 chars>-dev` (`/` replaced by `-`) |

Each image carries the labels `org.opencontainers.image.source` (links the
package to this repository) and `org.opencontainers.image.revision` (commit SHA).

Use it with the Compose stack:

```bash
GTA_IMAGE=ghcr.io/firms-gta/geoflow-tunaatlas:sha-abc1234 ./compose/run_full_workflow.sh
```

For reproducible production runs, prefer a `sha-…` tag over `latest`.

### Push permissions

`denied: permission_denied: write_package` means the repository is not allowed
to write to the package. In the package page on GitHub: *Package settings →
Manage Actions access → add `geoflow-tunaatlas` with the **Write** role*.
Pushing to a new package name requires the organisation to allow package
creation by GitHub Actions; otherwise create it once with a personal access
token (`write:packages`) and then grant Actions access as above.

## 5. Reproducing the CI locally

The CI runs the same script as local development:

```bash
./compose/run_workflow_retry.sh                    # local image and stack
CI=true GTA_IMAGE=gta-workflow:local ./compose/run_workflow_retry.sh   # with the CI override
```

To test exactly the image of a commit, build it first:

```bash
docker build -f compose/Dockerfile.workflow -t gta-workflow:local .
```

## 6. Related documentation

* [COMPOSE_STACK.md](COMPOSE_STACK.md) — the full Compose stack and launch scripts
* [VALIDATION.md](VALIDATION.md) — scientific validation procedures
