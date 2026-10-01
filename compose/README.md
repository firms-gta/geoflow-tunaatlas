# GTA workflow with Docker Compose

This folder runs the Global Tuna Atlas workflow together with the services it
publishes to: a PostGIS database, GeoServer, GeoNetwork and Zenodo.

Everything starts with one script. You only need **Docker**.

> To run the processing only (no database, no publication), you do not need
> this folder: see [docs/RUNNING.md](../docs/RUNNING.md).

All commands below are run from the **repository root**.

---

## 1. Get the code and the image (once)

```bash
git clone https://github.com/firms-gta/geoflow-tunaatlas.git
cd geoflow-tunaatlas
git checkout feature/mapping-codelist-cloned-in-docker-image

docker pull ghcr.io/firms-gta/gta-workflow:sha-30576d6-dev
export GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:sha-30576d6-dev
```

Images are built and tested by the CI, then published in
`ghcr.io/firms-gta/gta-workflow`:

| Tag | Meaning |
| --- | --- |
| `sha-<commit>-dev` | image of one commit of a feature branch (never moves) |
| `<branch-name>-dev` | latest tested image of that branch (moves on each push) |
| `sha-<commit>` / `latest` | images built from `master` |

Prefer a `sha-…` tag for reproducible runs. Without `GTA_IMAGE`, Compose builds
the image locally from `compose/Dockerfile.workflow`.

## 2. Run the test

```bash
./compose/run_workflow_retry.sh
```

It starts PostGIS, GeoServer and GeoNetwork, then runs
`DB,rawdata,nominal,level0,services` on the small sample in `tests/sample_data`
(a few rows of each raw file, same file names). It ends with
`Workflow terminé avec succès`.

## 3. Run the full workflow

Download the raw data (once):

```bash
wget -O all_raw_data_GTA.zip "https://zenodo.org/records/20834708/files/all_raw_data_GTA.zip?download=1"
unzip all_raw_data_GTA.zip -d runtime/extracted/
ls runtime/extracted/all_raw_data_GTA | head   # the raw files must be listed here
```

If the files land directly in `runtime/extracted/`, add
`GTA_DATA_DIR=runtime/extracted` in front of the command below.

Create the Zenodo credentials file (once), with a token from
**sandbox.zenodo.org** (*Applications → Personal access tokens*, scopes
`deposit:write` and `deposit:actions`):

```bash
cat > zenodo_secrets.env <<'EOF'
ZENODO_URL=https://sandbox.zenodo.org/api
ZENODO_TOKEN=paste_your_token_here
EOF
```

Then run:

```bash
./compose/run_full_workflow.sh
```

Default steps: `DB,rawdata,effort,nominal,level0,level1,level2,services`.

> **Zenodo:** a deposit published on `zenodo.org` gets a permanent DOI and
> cannot be deleted. Keep `ZENODO_URL` on the sandbox unless you are making a
> real release. The script prints the Zenodo target before starting.

## 4. Look at the results

| What | Where |
| --- | --- |
| Workflow outputs and logs | `runtime/jobs/<job>/` (log: `job-logs.txt`) |
| Database | `localhost:15430`, database `gta`, user `gta` / `gta` |
| GeoServer | <http://localhost:18080/geoserver> (`admin` / `geoserver`) — *Layer Preview* to check layers |
| GeoNetwork | <http://localhost:18081/geonetwork> (`admin` / `admin`) |

Stop everything:

```bash
docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml down
```

Add `-v` to also delete the volumes (database, layers, records) and start from
scratch next time.

## 5. Build the image yourself

Only needed if you change `Dockerfile.workflow`, `renv.lock` or the patches:

```bash
docker build -f compose/Dockerfile.workflow -t gta-workflow:local .
export GTA_IMAGE=gta-workflow:local
```

Then run step 2 or 3 again. Otherwise, just push to the branch: the CI tests the
commit and publishes `ghcr.io/firms-gta/gta-workflow:sha-<commit>-dev`
(see [docs/CI.md](../docs/CI.md)).

## 6. Work from RStudio

RStudio runs in the same environment as the workflow (same R, packages, patched
geoflow), with the database, GeoServer and GeoNetwork next to it. Use it to
explore, debug or run steps by hand.

Start it (the first time takes a few minutes: RStudio Server is installed on top
of the workflow image):

```bash
docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml up -d --build rstudio
```

Open <http://127.0.0.1:18787>, log in as `rstudio` / `test`, then open the project
`geoflow-tunaatlas/geoflow-tunaatlas.Rproj`.

What RStudio sees:

| In RStudio | On your machine |
| --- | --- |
| `R/`, `config/`, `compose/` | the same folders of your checkout: edits are saved in your repository |
| `data/GTA_2026` | `runtime/extracted/all_raw_data_GTA` (extract the data first) |
| `jobs/` | `runtime/jobs/` |
| database host `postgres`, GeoServer `http://geoserver:8080/geoserver` | the services of the stack |

Run the workflow from the R console:

```r
source("R/launching_workflows/GTA_2026_creation.R")

run_gta_workflow(
  steps_to_run = c("rawdata", "nominal"),   # any steps, e.g. c("services")
  data_source  = "volume_dir",
  data_path    = "/home/rstudio/geoflow-tunaatlas/data/GTA_2026",
  bootstrap_restore_renv = FALSE
)
```

Outputs go to `runtime/jobs/`, exactly as with the scripts.

Good to know:

- If the session freezes on `*** recursive gc invocation`, restart R
  (*Session → Restart R*) and run again: there is no automatic restart in
  RStudio.
- After changing `GTA_IMAGE`, keep `--build` so RStudio is rebuilt on the new
  image.
- The port only listens on `127.0.0.1`. On a remote server, use an SSH tunnel:
  `ssh -L 18787:localhost:18787 <server>`.
- Stop RStudio alone with
  `docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml stop rstudio`.

## 7. Run the Shiny app on the database

The visualisation app ([tunaatlas_pie_map_shiny](https://github.com/firms-gta/tunaatlas_pie_map_shiny))
is part of the stack, as the optional service `shiny` (Compose profile `app`).
It reads the `gta` database with the read-only user `gta_reader`.

Run the workflow first (step 3), so that the database contains the datasets.
Then start the app:

```bash
docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml --profile app up -d shiny
```

Open <http://127.0.0.1:13838>. Follow the app logs with:

```bash
docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml --profile app logs -f shiny
```

The connection settings are set by the compose file (`DB_HOST=postgres`,
`DB_NAME=gta`, `DB_USER=gta_reader`): nothing to configure. Choose the app
version with `SHINY_TAG` (default `latest`).

Stop the app alone with
`docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml --profile app stop shiny`.

---

## Options

Both scripts accept the same variables; `run_full_workflow.sh` only changes the
defaults.

| Variable | Default (test / full) | Purpose |
| --- | --- | --- |
| `GTA_IMAGE` | `gta-workflow:geoflow-1.3.0` | Workflow image to run |
| `GTA_DATA_DIR` | `tests/sample_data` / `runtime/extracted/all_raw_data_GTA` | Raw data folder on your machine |
| `GTA_STEPS` | see above | Steps to run, e.g. `GTA_STEPS=services` (order does not matter) |
| `GTA_MOUNT_CODE` | `true` | `true`: use `R/` and `config/` of your checkout (no rebuild needed after a code change). `false`: use the code inside the image (what the CI tests) |
| `GTA_COMPOSE_PROJECT` | *(none)* | Separate stack with its own containers and volumes, e.g. `gta-test` |
| `GTA_RUN_USER` | your `uid:gid` | User inside the container (use `1000:1000` if your uid is not 1000) |
| `GTA_GC_TIMEOUT` | `60` / `300` | Seconds to wait before restarting R when it hangs (see Troubleshooting) |

Examples:

```bash
GTA_STEPS=services ./compose/run_workflow_retry.sh            # only publish again
GTA_DATA_DIR=/data/gta ./compose/run_full_workflow.sh          # data stored elsewhere
GTA_COMPOSE_PROJECT=gta-test ./compose/run_workflow_retry.sh   # do not touch your usual stack
```

A separate project (`gta-test`) uses the same host ports: stop one stack before
starting the other, or give it other ports (see [Ports](#ports)). Remove it with
its volumes:

```bash
docker compose -p gta-test -f compose/compose.bd.rstudio.newversiongeoflow.yml down -v
```

## Ports

The stack uses **uncommon host ports**, bound to `127.0.0.1` only (not reachable
from the network), to avoid clashes with other tools on your machine:

| Service | Host port | Variable |
| --- | --- | --- |
| PostGIS | `15430` | `GTA_PG_PORT` |
| GeoServer | `18080` | `GTA_GEOSERVER_PORT` |
| GeoNetwork | `18081` | `GTA_GEONETWORK_PORT` |
| RStudio | `18787` | `GTA_RSTUDIO_PORT` |
| Shiny app | `13838` | `GTA_SHINY_PORT` |

They are set in [`compose/.env`](.env), which `docker compose` reads automatically
(it sits next to the compose file) and which is versioned, so everybody gets the
same ports. Inside the Docker network the services keep their usual ports
(`postgres:5432`, `geoserver:8080`…): the workflow and the geoflow configurations
do not depend on these values.

To use other ports, set the variable for one run (it takes precedence over the
file):

```bash
GTA_GEOSERVER_PORT=28080 GTA_GEONETWORK_PORT=28081 ./compose/run_workflow_retry.sh
```

or edit `compose/.env` without committing it. Check the result with:

```bash
docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml config | grep -B1 -A3 published
```

If a port is already taken, `docker compose` stops with
`port is already allocated`: pick another value.

## What is in this folder

| File | Role |
| --- | --- |
| `compose.bd.rstudio.newversiongeoflow.yml` | The stack (see services below) |
| `compose.ci.yml` | Small override added automatically in CI (smaller Elasticsearch heap) |
| `Dockerfile.workflow` | The workflow image: R 4.2.3, `renv.lock`, patched geoflow, FDI code lists, project code |
| `Dockerfile.rstudio` | Builds a second image: the workflow image + RStudio Server, so RStudio has exactly the workflow environment |
| `run_workflow_retry.sh` | Test run; restarts R if it hangs |
| `run_full_workflow.sh` | Same script with full-run defaults |
| `init-db/` | On the first start of PostGIS: creates the GeoNetwork database and the read-only user `gta_reader` |
| `patches/` | Fixes for geoflow 1.3.0, geometa and zen4R |

| Service | Host port (default) | Role |
| --- | --- | --- |
| `workflow` | – | R / geoflow workflow, started by the scripts |
| `postgres` | `15430` | PostGIS: databases `gta` and `geonetwork` |
| `geoserver` | `18080` | WMS/WFS services (CORS enabled for GeoNetwork) |
| `geonetwork` | `18081` | Metadata catalogue |
| `elasticsearch` | – | Search index used by GeoNetwork |
| `rstudio` | `18787` | Interactive development, see [§6](#6-work-from-rstudio) |
| `shiny` (profile `app`) | `13838` | Visualisation app on the database, see [§7](#7-run-the-shiny-app-on-the-database) |

The workflow waits until `postgres`, `geoserver` and `geonetwork` are healthy.
Tables, layers and records live in Docker volumes and persist between runs.

### Configuration files

- `docker_local.env.compose` (in the repository): database connection used by
  the geoflow configurations (`DB_HOST=postgres`, `DB_NAME=gta`, …). geoflow
  unloads these variables at the end of each workflow, so R code running between
  two workflows must reload them if it needs them.
- `zenodo_secrets.env` (not committed, `*.env` is ignored): `ZENODO_URL` and
  `ZENODO_TOKEN`, read by the `services` step.

## Troubleshooting

| Symptom | Fix |
| --- | --- |
| `Permission denied` / renv fails to install | Your uid is not 1000. Run `sudo chown -R 1000:1000 runtime tests/sample_data R config` and `export GTA_RUN_USER=1000:1000`. |
| Output stuck on `*** recursive gc invocation` | Known intermittent R bug. The scripts restart the run automatically; a single isolated message is harmless. |
| `container …-geonetwork-1 is unhealthy` | GeoNetwork is slow on first start. Run `docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml up -d --force-recreate geonetwork`, wait until healthy, then retry. |
| `Failed to copy file to: …/dataoutputpreharmo/…` | The data folder is not writable. The scripts create `dataoutputpreharmo/` and `dataoutputGTA/` there. |
| `URL using bad/illegal format or missing URL` (Zenodo) | `ZENODO_URL` is missing from `zenodo_secrets.env`. |
| `133 per 1 minute` (Zenodo) | Zenodo rate limit. Wait a few minutes, then rerun with `GTA_STEPS=services`. |
| `File with key … already exists` (Zenodo) | A draft from a previous run exists on the sandbox. Delete it on sandbox.zenodo.org before rerunning. |
| No map preview in GeoNetwork | Local only: records point to `http://geoserver:8080`. Add `127.0.0.1 geoserver` to `/etc/hosts` **and** start the stack with `GTA_GEOSERVER_PORT=8080`, so that this URL also works from your browser. |
| Old layers or records from previous runs | Volumes persist between runs. Use `GTA_COMPOSE_PROJECT=gta-test`, or `down -v` to reset. |

For a real deployment, serve GeoServer and GeoNetwork behind public HTTPS URLs
and publish those URLs in the records. The `/etc/hosts` entry, the GeoServer
CORS origin (`http://localhost:<GeoNetwork port>`) and the GeoNetwork option
`-Dproxy.allowPorts=80,443,8080` in the compose file are tuned for local use.
