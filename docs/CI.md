# Continuous integration

How the workflow is tested on GitHub Actions, and how the tested image is
published to the GitHub Container Registry (GHCR).

## Workflows

| Workflow file | Trigger | What it checks |
| --- | --- | --- |
| `workflow-launcher-checks.yml` | pull requests touching the launcher or the Dockerfile | launcher scripts parse, `tests/smoke_test_launcher.R`, required patches present in the `Dockerfile` |
| `test_whole_compose.yml` | push on `master` and the feature branch, manual | end-to-end run of the full stack on the sample data, then publication of the tested image |
| `reports.yml` | tags `v*`, manual | complete run on the real data, reports on GitHub Pages, comparison with an earlier run |

## End-to-end test (`test_whole_compose.yml`)

1. **Build** the `workflow` target of the `Dockerfile` from the commit (tag
   `gta-workflow:ci-<sha>`, local to the runner). Layers are cached on GHCR
   (`gta-workflow:buildcache`, shared by all branches): when only R code
   changes, `renv::restore()` is not re-run.
2. **Prepare the runner**: create `.env` from `env.example` (publication on the
   Zenodo sandbox, smaller Elasticsearch heap), create the output folders of the
   sample data, give the mounted folders to uid 1000 (the `rstudio` user of the
   image).
3. **Run** `./docker/run_workflow.sh` with
   `GTA_STEPS=DB,rawdata,nominal,effort,level0,level1,services`,
   `GTA_RUN_USER=1000:1000` and `GTA_MOUNT_CODE=false`, so the code inside the
   image is tested, not the checkout.
4. **On failure**, print the logs of all services.
5. **On success of a push**, publish the image that was just tested.

Covered: database model and code lists, the three pre-harmonisation branches,
nominal catch, effort, Level 0, and publication to PostGIS, GeoServer,
GeoNetwork and **sandbox** Zenodo.

The containers exist only on the runner during the job: GeoServer and
GeoNetwork cannot be opened from outside. Checks must run inside the job, and
anything worth keeping must be uploaded with `actions/upload-artifact`.

## Complete run and comparison with an earlier run (`reports.yml`)

Runs the whole workflow on the real data (input archive on Zenodo), renders
the reports of the final datasets and publishes them on GitHub Pages. Then it
compares the run with an earlier one:

1. **Summary** of the final datasets (`gta_run_summary()`): values summed by
   year, 5° square and all the other dimensions, about 20 MB in all.
2. **Reference**: the last summary archived in the GitHub release
   `run-summaries`, or the one named in the `compare_with` input.
3. **Comparison** (`gta_compare_summaries()`, report
   `docs/reports/run_comparison.Rmd`): for each dataset and each tRFMO, the
   years present in both runs are compared (totals, strata present in one run
   only, differences by dimension, with CWP.dataset); the years present in one
   run only (the new year of a release) are listed apart. The page is
   `run_comparison.html` on GitHub Pages, and the artifact
   `gta-run-comparison-<sha>` (90 days).
4. **Archive**: on `dev`, `master` and the tags, the summary is added to the
   release `run-summaries` (no time limit, unlike the artifacts). Other
   branches archive nothing and do not deploy GitHub Pages.

To check the differences of a branch before merging it into `dev`:

```bash
gh workflow run reports.yml --ref feature/no-ref/my-branch
```

The comparison is then in the artifact of the run. The summary of the
datasets of an earlier run can be archived without running the workflow
again, as long as its artifacts exist (14 days): run `reports.yml` on `dev`
with `datasets_from_run=<run id>`.

The same functions work locally:

```r
pkgload::load_all()
a <- gta_run_summary(list.files("<outputs of run A>", "\\.csv$", full.names = TRUE))
b <- gta_run_summary(list.files("<outputs of run B>", "\\.csv$", full.names = TRUE))
cmp <- gta_compare_summaries(a, b, labels = c("A", "B"))
```

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
docker build -t gta-workflow:local .     # workflow target (default)
cp env.example .env                      # once
GTA_IMAGE=gta-workflow:local GTA_MOUNT_CODE=false ./docker/run_workflow.sh
```

See [docker/README.md](../docker/README.md) for the scripts and their options.
