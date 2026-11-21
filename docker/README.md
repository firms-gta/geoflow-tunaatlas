# GTA workflow with Docker Compose

The stack of [`compose.yml`](../compose.yml) (repository root) runs the Global
Tuna Atlas workflow together with the services it publishes to: a PostGIS
database, GeoServer, GeoNetwork and Zenodo. This folder holds its launch
scripts, the geoflow patches and the database initialisation.

Everything starts with one script. You only need **Docker**.

> To run the processing only (no database, no publication), you do not need
> the stack: see [docs/RUNNING.md](../docs/RUNNING.md).

All commands below are run from the **repository root**.

---

## 1. Get the code, the image and the settings (once)

```bash
git clone https://github.com/firms-gta/geoflow-tunaatlas.git
cd geoflow-tunaatlas

cp env.example .env      # all the settings, in one file (never committed)
docker pull ghcr.io/firms-gta/gta-workflow:latest
```

`.env` holds every setting of the stack and of the workflow: database, GeoServer,
GeoNetwork, Zenodo, images, ports. The values of `env.example` are those of the
local stack: you only need to edit `.env` to publish on Zenodo (section 3) or
to change an image or a port. A missing variable stops `docker compose` with
its name.

`latest` is the image of the last tested commit of `master`, and the value of
`GTA_IMAGE` in `env.example`. Docker does not download a tag it already has:
run the `docker pull` line again to get a newer image.

Images are built and tested by the CI, then published in
`ghcr.io/firms-gta/gta-workflow`:

| Tag | Meaning |
| --- | --- |
| `latest` | last tested commit of `master` (moves on each push) |
| `sha-<commit>` | image of one commit of `master` (never moves) |
| `<branch-name>-dev` | last tested commit of another branch (moves on each push) |
| `sha-<commit>-dev` | image of one commit of another branch (never moves) |

To use another image than `latest`, set `GTA_IMAGE` in `.env`, or for one run:

```bash
GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:sha-<commit> ./docker/run_workflow.sh
```

Use a `sha-…` tag for any run you want to be able to reproduce, and keep a note
of it: `latest` will point to another image after the next push.

### Without cloning the repository

You only need `compose.yml`, `env.example` and the `docker/` folder. The image
contains them, so you can copy them out of the image instead of cloning:

```bash
mkdir gta && cd gta

export GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:latest
docker pull $GTA_IMAGE

docker run --rm --entrypoint tar $GTA_IMAGE \
  -C /home/rstudio/geoflow-tunaatlas -c compose.yml env.example docker | tar -x
cp env.example .env
```

Then follow this guide from section 2, running the commands from `gta/`
instead of the repository root.

- Nothing else is needed: the workflow uses the code of the image, and the
  script copies the sample data out of the image the first time it runs.
- The files are those of the commit the image was built from. After pulling a
  newer image, run the `docker run … | tar -x` command again so that the
  scripts and the image match (it does not touch your `.env`).
- RStudio (section 6) mounts `R/`, `config/` and `workflow/` from the host:
  clone the repository to use it.

## 2. Run the test

```bash
./docker/run_workflow.sh
```

It starts PostGIS, GeoServer and GeoNetwork, then runs
`DB,rawdata,nominal,effort,level0,level1,services` (everything but level2) on
the small sample in `tests/sample_data` (a few rows of each raw file, same file
names). It ends with `Workflow completed successfully`.

To check that the run needs nothing from the internet (only the images and the
data), run it offline:

```bash
GTA_OFFLINE=true ./docker/run_workflow.sh
```

## 3. Run the full workflow

Download the raw data (once):

```bash
wget -O all_raw_data_GTA.zip "https://zenodo.org/records/20834708/files/all_raw_data_GTA.zip?download=1"
unzip all_raw_data_GTA.zip -d runtime/extracted/
ls runtime/extracted/all_raw_data_GTA | head   # the raw files must be listed here
```

The data folder is `GTA_DATA_DIR` in `.env` (`./runtime/extracted/all_raw_data_GTA`).
If the files land directly in `runtime/extracted/`, set
`GTA_DATA_DIR=./runtime/extracted` there.

To publish on Zenodo, put a token from **sandbox.zenodo.org** (*Applications →
Personal access tokens*, scopes `deposit:write` and `deposit:actions`) in `.env`:

```bash
RUN_ZENODO_T_F=TRUE
ZENODO_URL=https://sandbox.zenodo.org/api
ZENODO_TOKEN=paste_your_token_here
```

Then run:

```bash
./docker/run_full_workflow.sh
```

Default steps: `DB,rawdata,effort,nominal,level0,level1,level2,services`.

> **Zenodo:** a deposit published on `zenodo.org` gets a permanent DOI and
> cannot be deleted. Keep `ZENODO_URL` on the sandbox unless you are making a
> real release. The script prints the Zenodo target before starting.

## 4. Look at the results

| What | Where |
| --- | --- |
| Workflow outputs and logs | `runtime/jobs/<job>/` (log: `job-logs.txt`) |
| Database | `localhost:15430`, database `gta`, user `gta` / `gta` (`DB_*` in `.env`) |
| GeoServer | <http://localhost:18080/geoserver> (`admin` / `geoserver`) — *Layer Preview* to check layers |
| GeoNetwork | <http://localhost:18081/geonetwork> (`admin` / `admin`) |

Start the services without running the workflow (database, GeoServer,
GeoNetwork, RStudio, Shiny):

```bash
docker compose up -d --no-build
```

The workflow itself only runs through the scripts (`docker compose run workflow`):
it has the Compose profile `workflow`, so a plain `up` never launches it.

Stop everything:

```bash
docker compose down
```

Add `-v` to also delete the volumes (database, layers, records) and start from
scratch next time.

## 5. Build the image yourself

Only needed if you change the `Dockerfile`, `renv.lock` or the patches:

```bash
docker build -t gta-workflow:local .     # workflow target (default)
```

Then set `GTA_IMAGE=gta-workflow:local` (in `.env` or for one run) and run step
2 or 3 again. Otherwise, just push: the CI tests the commit and publishes the
image (`latest` and `sha-<commit>` from `master`, `<branch-name>-dev` and
`sha-<commit>-dev` from another branch) (see [docs/CI.md](../docs/CI.md)).

The single `Dockerfile` has one target per image: `workflow` (default),
`rstudio`, `reporting` (workflow + LaTeX) and `netcdf` (NetCDF creation for the
CCP / EDITO platforms). Everything up to the `renv` restore is cached: as long
as `renv.lock` does not change, a rebuild downloads almost nothing.

To test with a renv library you already have (for example mounted in a
container) instead of reinstalling everything, run
`Rscript docker/repair_renv_library.R` from the repository root: it installs
only the missing packages, then rebuilds only those that fail to load
(`--check` to report without changing anything).

## 6. Work from RStudio

RStudio runs in the same environment as the workflow (same R, packages, patched
geoflow), with the database, GeoServer and GeoNetwork next to it. Use it to
explore, debug or run steps by hand.

Start it with the image published by the CI (workflow image + RStudio Server):

```bash
docker compose pull rstudio
docker compose up -d --no-build rstudio
```

`--no-build` makes sure the published image (`GTA_RSTUDIO_IMAGE` in `.env`) is
used: if it is missing, the command stops instead of building. To build it
locally instead (`rstudio` target of the `Dockerfile`):

```bash
docker compose up -d --build rstudio
```

Open <http://127.0.0.1:18787>, log in as `rstudio` / `RSTUDIO_PASSWORD` (`test`
in `env.example`), then open the project
`geoflow-tunaatlas/geoflow-tunaatlas.Rproj`.

What RStudio sees:

| In RStudio | On your machine |
| --- | --- |
| `R/`, `config/`, `workflow/`, `docker/`, `data/` | the same folders of your checkout: edits are saved in your repository |
| `data/GTA_2026` | `GTA_DATA_DIR` of `.env` (extract the data first) |
| `jobs/` | `runtime/jobs/` |
| `.env` | your `.env` (read-only) |
| database host `postgres`, GeoServer `http://geoserver:8080/geoserver` | the services of the stack |

Run the workflow from the R console:

```r
source("workflow/launchers/GTA_2026_creation.R")

run_gta_workflow(
  steps_to_run = c("rawdata", "nominal"),   # any steps, e.g. c("services")
  data_source  = "volume_dir",
  data_path    = "/home/rstudio/geoflow-tunaatlas/data/GTA_2026",
  bootstrap_restore_renv = FALSE
)
```

Outputs go to `runtime/jobs/`, exactly as with the scripts.

Good to know:

- The port only listens on `127.0.0.1`. On a remote server, use an SSH tunnel:
  `ssh -L 18787:localhost:18787 <server>`.
- Stop RStudio alone with `docker compose stop rstudio`.

## 7. Run the Shiny app on the database

The visualisation app ([tunaatlas_pie_map_shiny](https://github.com/firms-gta/tunaatlas_pie_map_shiny))
is part of the stack (service `shiny`): `docker compose up -d` starts it with
the other services.

Run the workflow first (step 2 or 3), so that the database contains the
datasets. Then start the app (alone, if the stack is not running):

```bash
docker compose up -d shiny
```

Open <http://127.0.0.1:13838>. In the app, choose the database source, select
the database **`gta`** in *Choose database*, then click *Connect*.

How the app connects (from `.env`, nothing else to configure):

| Setting | Value |
| --- | --- |
| host / port | `postgres` / `5432` (`DB_HOST`, `DB_PORT`) |
| user | `gta_reader`, read-only (`DB_USER_READONLY`) |
| password | `gta_reader` (`DB_PASSWORD_READONLY`) |
| database | chosen in the app (`gta`) |

The user `gta_reader` is created by the workflow itself, in the `DB` step
(`deploy_database_model.R`), together with its read access to everything the
workflow loads. Name and password come from `DB_USER_READONLY` and
`DB_PASSWORD_READONLY` in `.env`, which the app uses too.

| Message in the app | Fix |
| --- | --- |
| `password authentication failed for user "gta_reader"` | The database was deployed before the user existed, or with another password. Run the `DB` step again (it redeploys the database), then the steps that load the datasets. |
| `database "tunaatlas_sandbox" does not exist` | Select `gta` in *Choose database*. |

Follow the logs with `docker compose logs -f shiny`; stop the app alone with
`docker compose stop shiny`.

---

## Options

Both scripts accept the same variables; `run_full_workflow.sh` only changes the
defaults. They choose what one run does; the settings stay in `.env`.

| Variable | Default (test / full) | Purpose |
| --- | --- | --- |
| `GTA_IMAGE` | value in `.env` | Workflow image to run |
| `GTA_DATA_DIR` | `tests/sample_data` / value in `.env` | Raw data folder on your machine |
| `GTA_STEPS` | see above | Steps to run, e.g. `GTA_STEPS=services` (order does not matter) |
| `GTA_OFFLINE` | value in `.env` (`false`) | `true`: the stack has no internet access |
| `GTA_MOUNT_CODE` | `false` | `false`: use the code inside the image (what the CI tests). `true`: use `R/`, `config/` and `workflow/` of your checkout, to try a code change without rebuilding the image |
| `GTA_MOUNT_DATA` | `false` | `true`: also use the files of `data/` of your checkout (code lists, parameters). Sub-folders are not mounted |
| `GTA_COMPOSE_PROJECT` | *(none)* | Separate stack with its own containers and volumes, e.g. `gta-test` |
| `GTA_RUN_USER` | your `uid:gid` | User inside the container (use `1000:1000` if your uid is not 1000) |

Examples:

```bash
GTA_STEPS=services ./docker/run_workflow.sh                 # only publish again
GTA_DATA_DIR=/data/gta ./docker/run_full_workflow.sh        # data stored elsewhere
GTA_COMPOSE_PROJECT=gta-test ./docker/run_workflow.sh       # do not touch your usual stack
```

A separate project (`gta-test`) uses the same host ports: stop one stack before
starting the other, or give it its own ports to run both side by side (see
[Ports](#ports)):

```bash
GTA_COMPOSE_PROJECT=gta-test \
GTA_PG_PORT=25430 GTA_GEOSERVER_PORT=28080 GTA_GEONETWORK_PORT=28081 \
./docker/run_workflow.sh
```

Remove it with its volumes:

```bash
docker compose -p gta-test down -v
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

They are set in `.env`. Inside the Docker network the services keep their usual
ports (`postgres:5432`, `geoserver:8080`…): the workflow and the geoflow
configurations do not depend on these values. If you change
`GTA_GEOSERVER_PORT`, change the port of `GEOSERVER_PUBLIC_URL` too.

To use other ports for one run, set the variable (it takes precedence over
`.env`):

```bash
GTA_GEOSERVER_PORT=28080 GTA_GEONETWORK_PORT=28081 ./docker/run_workflow.sh
```

Check the result with:

```bash
docker compose config | grep -B1 -A3 published
```

If a port is already taken, `docker compose` stops with
`port is already allocated`: pick another value.

## What is where

| File | Role |
| --- | --- |
| `compose.yml` (root) | The stack (see services below) |
| `Dockerfile` (root) | The images, one target each: `workflow` (R 4.2.3, `renv.lock`, patched geoflow, FDI code lists, project code), `rstudio` (workflow + RStudio Server), `reporting`, `netcdf` |
| `env.example` (root) | Template of `.env`, the single settings file |
| `docker/run_workflow.sh` | Runs the workflow in the stack; defaults to the test on the sample data (what the CI runs) |
| `docker/run_full_workflow.sh` | `run_workflow.sh` with full-run defaults (real data, all steps) |
| `docker/repair_renv_library.R` | Makes an existing renv library usable: installs the missing packages, rebuilds only those that fail to load |
| `docker/init-db/` | SQL run when the PostGIS volume is created: creates the GeoNetwork database |
| `docker/patches/` | Fixes for geoflow 1.3.0, geometa and zen4R |

| Service | Host port (default) | Role |
| --- | --- | --- |
| `workflow` (profile `workflow`) | – | R / geoflow workflow, started by the scripts |
| `postgres` | `15430` | PostGIS: databases `gta` and `geonetwork` |
| `geoserver` | `18080` | WMS/WFS services (CORS enabled for GeoNetwork) |
| `geonetwork` | `18081` | Metadata catalogue |
| `elasticsearch` | – | Search index used by GeoNetwork (`ES_JAVA_OPTS` in `.env`) |
| `rstudio` | `18787` | Interactive development, see [§6](#6-work-from-rstudio) |
| `shiny` | `13838` | Visualisation app on the database, see [§7](#7-run-the-shiny-app-on-the-database) |

The workflow waits until `postgres`, `geoserver` and `geonetwork` are healthy.
Tables, layers and records live in Docker volumes and persist between runs.

### Settings: `.env`

`.env` (not committed, `*.env` is ignored; template: `env.example`) is read by
`docker compose`, passed to the workflow and RStudio containers, and loaded by
the geoflow configurations (`"environment": {"file": ".env"}`). It holds the
database connection (`DB_HOST=postgres`, `DB_NAME=gta`, …) and the read-only
user created by the `DB` step (`DB_USER_READONLY`, `DB_PASSWORD_READONLY`),
GeoServer, GeoNetwork, Zenodo (`RUN_ZENODO_T_F`, `ZENODO_URL`, `ZENODO_TOKEN`),
the images and the ports. geoflow unloads these variables at the end of each
workflow, so R code running between two workflows must reload them if it needs
them.

### Coming from the former settings files

`docker_local.env.compose`, `zenodo_secrets.env` and `compose/.env` are no
longer read (the scripts and the launcher say so if they are still there). Copy
into `.env` the values you had changed, typically the Zenodo token:

```bash
cp env.example .env       # if not done yet
# edit .env: ZENODO_URL / ZENODO_TOKEN (from zenodo_secrets.env), any port or
# image you had changed
rm zenodo_secrets.env     # no longer read
```

## Troubleshooting

| Symptom | Fix |
| --- | --- |
| `required variable … is missing a value` | That variable is missing from `.env`: compare it with `env.example`. |
| `No .env at the repository root` | `cp env.example .env`, then edit it. |
| `Permission denied` / renv fails to install | Your uid is not 1000. Run `sudo chown -R 1000:1000 runtime tests/sample_data R config` and `export GTA_RUN_USER=1000:1000`. |
| Output stuck on `*** recursive gc invocation` | R bug fixed in the patched geoflow of `renv.lock`. If it comes back, check that the image uses that geoflow. |
| `container …-geonetwork-1 is unhealthy` | GeoNetwork is slow on first start. Run `docker compose up -d --force-recreate geonetwork`, wait until healthy, then retry. |
| `Failed to copy file to: …/dataoutputpreharmo/…` | The data folder is not writable. The scripts create `dataoutputpreharmo/` and `dataoutputGTA/` there. |
| `URL using bad/illegal format or missing URL` (Zenodo) | `ZENODO_URL` is missing from `.env`. |
| `133 per 1 minute` (Zenodo) | Zenodo rate limit. Wait a few minutes, then rerun with `GTA_STEPS=services`. |
| `File with key … already exists` (Zenodo) | A draft from a previous run exists on the sandbox. Delete it on sandbox.zenodo.org before rerunning. |
| No map preview in GeoNetwork | Local only: records point to `http://geoserver:8080`. Add `127.0.0.1 geoserver` to `/etc/hosts` **and** set `GTA_GEOSERVER_PORT=8080` (and the port of `GEOSERVER_PUBLIC_URL`) in `.env`, so that this URL also works from your browser. |
| Old layers or records from previous runs | Volumes persist between runs. Use `GTA_COMPOSE_PROJECT=gta-test`, or `down -v` to reset. |

For a real deployment, serve GeoServer and GeoNetwork behind public HTTPS URLs
and publish those URLs in the records. The `/etc/hosts` entry, the GeoServer
CORS origin (`http://localhost:<GeoNetwork port>`) and the GeoNetwork option
`-Dproxy.allowPorts=80,443,8080` in `compose.yml` are tuned for local use.
