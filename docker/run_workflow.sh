#!/usr/bin/env bash
# =============================================================================
# docker/run_workflow.sh
#
# Runs the GTA workflow in the Docker Compose stack of compose.yml (Postgres,
# GeoServer, GeoNetwork): prepares the mounts and the user, then runs a single
# "docker compose run workflow". The settings come from .env (template:
# env.example); the variables below only choose what this run does.
#
# Default: TEST run on the tests/sample_data sample.
# For the full workflow on the real data: docker/run_full_workflow.sh
#
# Usage (from any folder):
#   ./docker/run_workflow.sh
#   GTA_STEPS=rawdata,nominal ./docker/run_workflow.sh
#   GTA_DATA_DIR=/path/to/data ./docker/run_workflow.sh
#   GTA_OFFLINE=true ./docker/run_workflow.sh
#
# Run options (environment variables, all optional):
#   GTA_DATA_DIR         data folder on the host           (default: tests/sample_data)
#   GTA_STEPS            workflow steps                    (default: DB,rawdata,nominal,effort,level0,level1,services)
#   GTA_COMPOSE_PROJECT  compose project name, to isolate the stack (containers + volumes)
#   GTA_RUN_USER         container uid:gid                 (default: current user; CI: 1000:1000)
#   GTA_MOUNT_CODE       true: mounts R/, config/ and workflow/ from the repository, to test
#                        a change without rebuilding the image (default: false)
#   GTA_MOUNT_DATA       true: also mounts the files of data/ from the repository (default: false)
#   GTA_OFFLINE          true: stack without internet access (default: the value in .env)
#   GTA_IMAGE            workflow image                    (default: the value in .env)
#   GTA_SUMMARISE_INVALID_RAW  overrides the value in .env for this run
# =============================================================================
set -uo pipefail
cd "$(dirname "$0")/.."   # repository root

# --- Settings: .env ----------------------------------------------------------------
if [[ ! -f .env ]]; then
  echo ">>> No .env at the repository root: cp env.example .env, then fill it in" >&2
  exit 2
fi
for old in docker_local.env.compose zenodo_secrets.env; do
  [[ -e "$old" ]] && echo ">>> $old is no longer read: its values belong in .env" >&2
done
env_value() {   # env_value <NAME>: value of NAME in .env (quotes removed)
  grep -E "^$1=" .env | tail -1 | cut -d= -f2- | sed -E 's/^"(.*)"$/\1/' || true
}

DATA_DIR="${GTA_DATA_DIR:-tests/sample_data}"
STEPS="${GTA_STEPS:-DB,rawdata,nominal,effort,level0,level1,services}"
PROJECT="${GTA_COMPOSE_PROJECT:-}"
RUN_USER="${GTA_RUN_USER:-$(id -u):$(id -g)}"
MOUNT_CODE="${GTA_MOUNT_CODE:-false}"
MOUNT_DATA="${GTA_MOUNT_DATA:-false}"
OFFLINE="${GTA_OFFLINE:-$(env_value GTA_OFFLINE)}"
IMAGE="${GTA_IMAGE:-$(env_value GTA_IMAGE)}"

NAME="${PROJECT:-gta}-workflow-run"
CONTAINER_ROOT=/home/rstudio/geoflow-tunaatlas
CONTAINER_DATA="$CONTAINER_ROOT/data/GTA_2026"

# --- Sample data missing: taken from the image ------------------------------------
# Lets the stack run without cloning the repository (see docker/README.md,
# "Without cloning the repository").
copy_from_image() {   # copy_from_image <path relative to the repository root>
  echo ">>> $1 missing: copying it from image $IMAGE"
  docker run --rm --entrypoint tar "$IMAGE" -C "$CONTAINER_ROOT" -c "$1" | tar -x \
    || { echo ">>> Could not copy $1 from image $IMAGE" >&2; exit 2; }
}
if [[ -z "${GTA_DATA_DIR:-}" && ! -d tests/sample_data ]]; then
  copy_from_image tests/sample_data
fi

# Code: the image's by default; the repository's with GTA_MOUNT_CODE=true.
if [[ "$MOUNT_CODE" == "true" && ! ( -d R && -d config && -d workflow ) ]]; then
  echo ">>> GTA_MOUNT_CODE=true but R/, config/ or workflow/ is missing" >&2
  exit 2
fi

# --- Absolute paths (docker -v only accepts absolute paths) --------------------
case "$DATA_DIR" in
  /*) ;;
  *)  DATA_DIR="$PWD/$DATA_DIR" ;;
esac

if [[ ! -d "$DATA_DIR" ]] || [[ -z "$(ls -A "$DATA_DIR" 2>/dev/null)" ]]; then
  echo ">>> Data folder missing or empty: $DATA_DIR" >&2
  exit 2
fi

# --- Compose command -------------------------------------------------------------
# GTA_OFFLINE and GTA_IMAGE, when set here, take precedence over .env in compose.
export GTA_OFFLINE="$OFFLINE" GTA_IMAGE="$IMAGE"
COMPOSE=(docker compose -f compose.yml)
[[ -n "$PROJECT" ]] && COMPOSE+=(-p "$PROJECT")

EXTRA=()
if [[ "$MOUNT_CODE" == "true" ]]; then
  EXTRA+=(
    -v "$PWD/R":"$CONTAINER_ROOT/R"
    -v "$PWD/config":"$CONTAINER_ROOT/config"
    -v "$PWD/workflow":"$CONTAINER_ROOT/workflow"
  )
fi

# Reference files of data/ (code lists, parameters...). They are mounted one by
# one, not the folder: in the image only, data/ also holds fdi-codelists/ and
# fdi-mappings/, which a mount of the whole folder would hide.
if [[ "$MOUNT_DATA" == "true" ]]; then
  [[ -d data ]] || { echo ">>> GTA_MOUNT_DATA=true but data/ is missing" >&2; exit 2; }
  for f in data/*; do
    [[ -f "$f" ]] && EXTRA+=(-v "$PWD/$f":"$CONTAINER_ROOT/$f")
  done
fi

[[ -n "${GTA_SUMMARISE_INVALID_RAW:-}" ]] && EXTRA+=(-e GTA_SUMMARISE_INVALID_RAW="$GTA_SUMMARISE_INVALID_RAW")

# --- Host preparation ------------------------------------------------------------
mkdir -p runtime/jobs runtime/cache
# Output folders that the workflow writes INSIDE the data folder
mkdir -p "$DATA_DIR/dataoutputpreharmo" "$DATA_DIR/dataoutputGTA"

echo ">>> Data    : $DATA_DIR"
echo ">>> Steps   : $STEPS"
echo ">>> Project : ${PROJECT:-gta} | user: $RUN_USER | code mounted: $MOUNT_CODE | data/ mounted: $MOUNT_DATA | offline: $OFFLINE"

docker rm -f "$NAME" >/dev/null 2>&1   # leftover of an interrupted run

"${COMPOSE[@]}" run --rm --name "$NAME" \
  --user "$RUN_USER" \
  ${EXTRA[@]+"${EXTRA[@]}"} \
  -v "$DATA_DIR":"$CONTAINER_DATA" \
  -e GTA_STEPS="$STEPS" \
  -e GTA_DATA_PATH="$CONTAINER_DATA" \
  workflow \
  Rscript workflow/launchers/run_gta_2026_workflow_cli.R
status=$?

if [[ $status -eq 0 ]]; then
  echo ">>> Workflow completed successfully"
else
  echo ">>> Workflow failed (exit code $status)"
fi
exit "$status"
