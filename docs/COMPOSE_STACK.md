# Running the full GTA stack with Docker Compose

This page describes `compose/compose.bd.rstudio.newversiongeoflow.yml`: the
**complete stack** used to run the workflow *and* publish its outputs
(PostgreSQL/PostGIS database, GeoServer, GeoNetwork, Zenodo).

It is different from [`compose.workflow.yml`](DOCKER_COMPOSE.md), which runs the
workflow container alone, without any publication service.

| Use case | File | Documentation |
| --- | --- | --- |
| Scientific processing only (no database, no services) | `compose.workflow.yml` | [DOCKER_COMPOSE.md](DOCKER_COMPOSE.md) |
| Processing + database + GeoServer/GeoNetwork/Zenodo publication | `compose/compose.bd.rstudio.newversiongeoflow.yml` | this page |
| Automated test on GitHub Actions | same stack + `compose/compose.ci.yml` | [CI.md](CI.md) |

## 1. Services

| Service | Image | Host port | Role |
| --- | --- | --- | --- |
| `workflow` | `${GTA_IMAGE}` (built from `compose/Dockerfile.workflow`) | – | R / geoflow workflow, started on demand with `run` |
| `postgres` | `postgis/postgis:16-3.5` | `5430` | GTA database (`gta`) and GeoNetwork database (`geonetwork`) |
| `geoserver` | `docker.osgeo.org/geoserver` | `8080` | OGC services (WMS/WFS) |
| `geonetwork` | `geonetwork:4.4` | `8081` | Metadata catalogue |
| `elasticsearch` | `elasticsearch:8` | – | Search index required by GeoNetwork |
| `rstudio` | built from `compose/Dockerfile.rstudio` | `127.0.0.1:8787` | Interactive development |
| `shiny` (profile `app`) | `ghcr.io/firms-gta/tunaatlas_pie_map_shiny` | `127.0.0.1:3838` | Read-only visualisation app |

The `workflow` service waits until `postgres`, `geoserver` and `geonetwork` are
*healthy* before starting. GeoNetwork can take one to two minutes to become
ready on a fresh volume.

Data produced by the stack (tables, layers, metadata records) live in Docker
volumes (`postgres_data`, `geoserver_data`, `geonetwork_data`,
`elasticsearch_data`), not in the images.

## 2. Configuration files

Two files are mounted into the workflow container. Neither is baked into the
image.

### `docker_local.env.compose`

Loaded by the geoflow configurations through their `"environment"` block.
It must point to the services of the stack:

```text
DB_DRV=PostgreSQL
DB_HOST=postgres
DB_PORT=5432
DB_NAME=gta
DB_USER=gta
DB_PASSWORD=gta
DB_USER_READONLY=gta
GTA_DB_UPLOAD=true
RUN_ZENODO_T_F=true
```

`DB_HOST=postgres` is the service name on the Compose network, not `localhost`.

> geoflow **unloads** the variables of this file at the end of each workflow.
> R code running between two geoflow workflows must not assume `DB_*` are still
> set: the launcher reloads `docker_local.env.compose` before the `services`
> step (which calls `ensure_geoserver_ready()`) for this reason.

### `zenodo_secrets.env`

Used by the `services` step (`config/tunaatlas_qa_services.json` reads
`{{ ZENODO_URL }}` and `{{ ZENODO_TOKEN }}`):

```text
ZENODO_URL=https://sandbox.zenodo.org/api
ZENODO_TOKEN=<token created on sandbox.zenodo.org>
```

This file is ignored by Git (`*.env`). Use the **sandbox** for any test: a
deposit published on `zenodo.org` receives a permanent DOI and cannot be deleted.
A sandbox token does not work on `zenodo.org`, which is a useful safeguard.

## 3. Test run on the sample data

```bash
./compose/run_workflow_retry.sh
```

This runs `DB,rawdata,nominal,level0,services` on `tests/sample_data`, a small
extract of the raw files (a few lines per file, same file names). It is the
same command as the CI. The whole stack is started automatically.

Change the steps with `GTA_STEPS`:

```bash
GTA_STEPS=rawdata,nominal ./compose/run_workflow_retry.sh
```

### Regenerating the sample data

The sample is produced from the full raw data by:

```bash
Rscript tests/make_sample_data.R 20   # 20 random rows per file / sheet (fixed seed)
```

It reads `runtime/extracted/all_raw_data_GTA`, keeps every file name, and
recreates the output sub-folders (`dataoutputpreharmo/`, `dataoutputGTA/`) with a
`.gitkeep`. Their content is ignored by Git.

## 4. Full workflow on the real data

1. Download and extract the raw data (Zenodo record
   [20834708](https://zenodo.org/records/20834708), file `all_raw_data_GTA.zip`)
   into `runtime/extracted/all_raw_data_GTA/`.
2. Check `docker_local.env.compose` and `zenodo_secrets.env` (see §2), in
   particular the Zenodo target.
3. Run:

```bash
./compose/run_full_workflow.sh
```

Defaults: data from `runtime/extracted/all_raw_data_GTA`, steps
`DB,rawdata,effort,nominal,level0,level1,level2,services`.

Examples:

```bash
# data stored elsewhere
GTA_DATA_DIR=/data/gta/all_raw_data_GTA ./compose/run_full_workflow.sh

# only the downstream steps, reusing the database already loaded
GTA_STEPS=level1,level2,services ./compose/run_full_workflow.sh

# use the code baked in the image instead of the local R/ and config/
GTA_MOUNT_CODE=false GTA_IMAGE=ghcr.io/firms-gta/geoflow-tunaatlas:latest \
  ./compose/run_full_workflow.sh
```

The order of the steps in `GTA_STEPS` does not matter: the launcher always runs
them in the order of the processing chain (see [WORKFLOW.md](WORKFLOW.md)).
`GTA_STEPS=all` runs every stage, including summaries, QA documents and reports.

## 5. Launch-script options

Both scripts accept the same variables; `run_full_workflow.sh` only changes the
defaults.

| Variable | `run_workflow_retry.sh` default | `run_full_workflow.sh` default | Purpose |
| --- | --- | --- | --- |
| `GTA_DATA_DIR` | `tests/sample_data` | `runtime/extracted/all_raw_data_GTA` | Host data directory mounted as `data/GTA_2026` |
| `GTA_STEPS` | `DB,rawdata,nominal,level0,services` | `DB,rawdata,effort,nominal,level0,level1,level2,services` | Workflow steps |
| `GTA_IMAGE` | `gta-workflow:geoflow-1.3.0` | same | Workflow image (built locally if missing) |
| `GTA_COMPOSE_PROJECT` | *(default project `gta`)* | same | Separate project: own containers, network and volumes |
| `GTA_RUN_USER` | current `uid:gid` | same | User inside the container (`1000:1000` in CI) |
| `GTA_MOUNT_CODE` | `true` | `true` | Mount `R/` and `config/` from the repository |
| `GTA_MAX_ATTEMPTS` | `3` | `3` | Attempts when R hangs (see §8) |
| `GTA_GC_TIMEOUT` | `60` | `300` | Seconds without normal output after a GC message before killing |

Outputs are written to `runtime/jobs/` (geoflow jobs and logs) and, for some
pre-harmonisation outputs, to `dataoutputpreharmo/` and `dataoutputGTA/` inside
the data directory.

## 6. Keeping a test stack separate from your working stack

The same service names can run twice under different Compose *projects*:

```bash
GTA_COMPOSE_PROJECT=gta-test ./compose/run_workflow_retry.sh
```

`gta-test` gets its own containers and volumes (`gta-test_postgres_data`, …):
the database of the default `gta` project is never touched, even with identical
database and table names.

Both projects publish the same host ports (5430, 8080, 8081). Stop one before
starting the other, or change the ports in a small override file.

Remove a test project and **its** volumes only:

```bash
docker compose -p gta-test -f compose/compose.bd.rstudio.newversiongeoflow.yml down -v
```

## 7. Inspecting the results

| What | How |
| --- | --- |
| Database | pgAdmin / psql on `localhost:5430`, database `gta`, user `gta` |
| GeoServer | <http://localhost:8080/geoserver> (`admin` / `geoserver`) |
| GeoNetwork | <http://localhost:8081/geonetwork> (`admin` / `admin`) |
| Workflow logs | `runtime/jobs/<job>/job-logs.txt` |

With pgAdmin, register one server per project and distinguish them by port if
you changed it (e.g. `5430` for `gta`, `5431` for `gta-test`).

### RStudio and the Shiny app

```bash
docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml up -d rstudio        # http://127.0.0.1:8787
docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml --profile app up -d shiny   # http://127.0.0.1:3838
```

The Shiny app connects with a read-only user (`gta_reader`); create it in the
database if it does not exist yet.

## 8. Troubleshooting

| Symptom | Cause | Fix |
| --- | --- | --- |
| Output stuck on `*** recursive gc invocation` | Intermittent R garbage-collector bug while loading geoflow's dependencies | Handled by the scripts: the container is killed and restarted when no normal line follows for `GTA_GC_TIMEOUT` s. A single isolated message is harmless. |
| `dependency failed to start: container …-geonetwork-1 is unhealthy` | GeoNetwork health endpoint answers 500 without `Accept: application/json` | The healthcheck already sends the header; recreate the container after editing the compose file: `docker compose … up -d --force-recreate geonetwork` |
| `Failed to copy file to: …/dataoutputpreharmo/…` | Output folder missing in the data directory | The scripts create `dataoutputpreharmo/` and `dataoutputGTA/`; check they are writable |
| `permission denied` / `EACCES` on mounted folders | Container user cannot write to the host folder | Run with an owner matching `GTA_RUN_USER` (`sudo chown -R "$(id -u):$(id -g)" runtime`) |
| `ensure_geoserver_ready(): missing required value(s): db_host…` | `DB_*` unloaded by geoflow at the end of a previous workflow | Reload `docker_local.env.compose` before the call (already done for the `services` step) |
| `URL using bad/illegal format or missing URL` in `my-zenodo` | `ZENODO_URL` missing from `zenodo_secrets.env` | Add `ZENODO_URL` (see §2) |

## 9. Related documentation

* [CI.md](CI.md) — how the stack is tested and the image published
* [DOCKER_COMPOSE.md](DOCKER_COMPOSE.md) — workflow-only Compose file
* [WORKFLOW.md](WORKFLOW.md) — stages and dependencies
* [RUNNING.md](RUNNING.md) — explicit `docker run` commands
