# Incremental runs with {targets}

This document explains how to re-run only the parts of the GTA workflow that
changed, using the [{targets}](https://books.ropensci.org/targets/) package.
It is meant for **development and debugging** (typically from RStudio).

Publication (PostGIS, GeoServer, GeoNetwork, Zenodo) never uses this cache: it
always runs from scratch with the classic launcher, see
[RUNNING_ADVANCED.md](RUNNING_ADVANCED.md) and [compose/README.md](../compose/README.md).

---

## 1. What {targets} does here

Each workflow step is a *target*. {targets} remembers, for every step, the
files it depended on and the job directory it produced. On the next run, a step
is skipped if nothing it depends on has changed, and re-run otherwise.

A step re-runs when one of these changed:

* its geoflow configuration (JSON) or any file it names, recursively: entity
  CSVs, action and generation scripts, R files those scripts source, code-list
  files (`data/*_code_lists.csv`, ...);
* for the raw steps, the raw input files under `data/GTA_2026/` named in their
  entity CSV;
* the launcher code (`GTA_2026_creation.R`, `workflow_helpers.R`, ...);
* an upstream step (see the graph below).

The list of files is rebuilt at every run, so a script newly sourced by a
generation script is picked up automatically.

### Steps and dependencies

```
 raw_nominal     raw_effort     raw_georef ──────────┐
      │              │              │                │
      v              v              v                │
   nominal         effort         level0             │
      │                             │                │
      │                             v                │
      │                          level1              │
      │                                              v
      └───────────────────────────────────────>   level2
```

* a change in the raw nominal data re-runs `nominal` and `level2`;
* a change in the raw effort data re-runs `effort` only;
* a change in the raw georeferenced catch data re-runs `level0`, `level1` and
  `level2`.

### Without database

Every step runs with `GTA_NO_DB=true`: the database software is removed from
the configuration, nothing is uploaded, and the actions that need the database
(`enrich_metadata`, `enrich_for_db_services`, `load_metadata`, and
`create_geopackage` / `create_geoparquet` without a `grid_file` option) are
switched off. Results therefore never depend on whether PostGIS was
reachable, and deleting the database volume does not invalidate the cache.

The step outputs are the usual geoflow job directories under `jobs/`.

---

## 2. Running from RStudio

From the root of the project:

```r
source("R/launching_workflows/targets/targets_rstudio.R")

gta_tar_graph()                     # dependency graph, outdated steps in colour
gta_tar_outdated()                  # steps that would re-run
gta_tar_make("level0")              # brings level0 and what it needs up to date
gta_tar_make(c("level1", "nominal"))
gta_tar_make("rawdata")             # the three raw steps
gta_tar_job("level0")               # job directory of the last level0 run
gta_tar_invalidate("level1")        # force level1 (and downstream) to re-run
```

Available steps: `rawdata` (= `raw_nominal`, `raw_effort`, `raw_georef`),
`nominal`, `effort`, `level0`, `level1`, `level2`.

Running `gta_tar_make("level0")` twice in a row: the first run computes
`raw_georef` then `level0`; the second one skips everything.

### Debugging a step

By default the steps run in a separate R process. To run them in the RStudio
session, so that `browser()`, `debug()` and `traceback()` work:

```r
gta_tar_make("level0", in_session = TRUE)
```

This sets `GTA_NO_DB=true` in the session: restart R before running a
publication from the same session.

When a step fails, {targets} prints the path of the geoflow log
(`jobs/<id>/job-logs.txt`). `targets::tar_meta(fields = c(error, warnings),
store = .gta_tar_store())` lists the errors and warnings of the last run.

---

## 3. Running from a terminal

```bash
GTA_STEPS=level0 Rscript R/launching_workflows/targets/run_targets_cli.R
```

| Variable            | Default                                    | Meaning                                    |
| ------------------- | ------------------------------------------ | ------------------------------------------ |
| `GTA_STEPS`         | `rawdata`                                  | steps to bring up to date (comma-separated) |
| `GTA_TARGETS_STORE` | `/cache/_targets` in Docker, else `_targets` | where {targets} keeps its state            |
| `GTA_TARGETS_RESET` | `false`                                    | `true` forgets the state and re-runs all   |

The other `GTA_*` variables (data source, data path, ...) are the same as for
`run_gta_2026_workflow_cli.R`, see [RUNNING_ADVANCED.md](RUNNING_ADVANCED.md).

---

## 4. State and reset

The state is kept in `/cache/_targets` in the Docker stack (`runtime/cache` on
the host, so it survives the container), otherwise in `_targets/` at the root
of the repository (git-ignored).

To start again from scratch, delete that folder, or set `GTA_TARGETS_RESET=true`
for the command-line launcher. Deleting job directories under `jobs/` does not
tell {targets} anything: invalidate the matching step with
`gta_tar_invalidate()`.

---

## 5. Known limits

* Scripts reached only through a path computed at run time (never written as a
  file name anywhere) are not tracked: invalidate the step by hand.
* Raw files downloaded from a DOI (`GTA_DATA_SOURCE=doi`) are not on disk when
  the files are listed, so they are not tracked.
* `summaries`, `reports` and `qa_rmd` are not part of the pipeline: use the
  classic launcher for them, pointing to the job directories given by
  `gta_tar_job()` (see §7–8 of [RUNNING_ADVANCED.md](RUNNING_ADVANCED.md)).
* Without database, no GeoPackage or GeoParquet is produced unless a CWP grid
  file is given with the `grid_file` option of those actions.

---

## 6. Files

All in `R/launching_workflows/targets/`:

| File                 | Role                                                       |
| -------------------- | ---------------------------------------------------------- |
| `_targets.R`         | pipeline definition (steps and dependencies)               |
| `targets_deps.R`     | lists the files a configuration depends on                 |
| `targets_rstudio.R`  | `gta_tar_*()` helpers for RStudio                          |
| `run_targets_cli.R`  | command-line launcher                                      |

The `GTA_NO_DB` switch itself is in `R/launching_workflows/workflow_helpers.R`.
