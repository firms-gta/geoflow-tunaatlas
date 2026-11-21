#!/usr/bin/env bash
# =============================================================================
# compose/run_workflow.sh
#
# Runs the GTA workflow in the Docker Compose stack (Postgres, GeoServer,
# GeoNetwork): prepares the mounts, the user and the environment files, then
# runs a single "docker compose run workflow".
#
# Default: TEST run on the tests/sample_data sample.
# For the full workflow on the real data: compose/run_full_workflow.sh
#
# Usage (from any folder):
#   ./compose/run_workflow.sh
#   GTA_STEPS=rawdata,nominal ./compose/run_workflow.sh
#   GTA_DATA_DIR=/path/to/data ./compose/run_workflow.sh
#
# Variables (all optional):
#   GTA_DATA_DIR         data folder on the host           (default: tests/sample_data)
#   GTA_STEPS            workflow steps                    (default: DB,rawdata,nominal,effort,level0,level1,services)
#   GTA_COMPOSE_PROJECT  compose project name, to isolate the stack (containers + volumes)
#   GTA_RUN_USER         container uid:gid                 (default: current user; CI: 1000:1000)
#   GTA_MOUNT_CODE       false: code of the image; true: mounts R/, config/ and workflow/ from the
#                        repository, to test a change without rebuilding the image (default: false)
#   GTA_MOUNT_DATA       true: also mounts the files of data/ from the repository (default: false)
#   GTA_OFFLINE          true: stack without internet access (compose/compose.offline.yml),
#                        to check that the workflow only needs the image and the data
#                        (default: false)
# =============================================================================
set -uo pipefail
cd "$(dirname "$0")/.."   # repository root

DATA_DIR="${GTA_DATA_DIR:-tests/sample_data}"
STEPS="${GTA_STEPS:-DB,rawdata,nominal,effort,level0,level1,services}"
PROJECT="${GTA_COMPOSE_PROJECT:-}"
RUN_USER="${GTA_RUN_USER:-$(id -u):$(id -g)}"
MOUNT_CODE="${GTA_MOUNT_CODE:-false}"
MOUNT_DATA="${GTA_MOUNT_DATA:-false}"
OFFLINE="${GTA_OFFLINE:-false}"

NAME="${PROJECT:-gta}-workflow-run"
CONTAINER_ROOT=/home/rstudio/geoflow-tunaatlas
CONTAINER_DATA="$CONTAINER_ROOT/data/GTA_2026"

# --- Missing files: taken from the image ----------------------------------------
# Lets the stack run without cloning the repository, with only the compose/
# folder copied (see compose/README.md, "Without cloning the repository").
COMPOSE_FILE_PATH=compose/compose.yml
IMAGE="${GTA_IMAGE:-$(sed -n 's/.*image: \${GTA_IMAGE:-\([^}]*\)}.*/\1/p' "$COMPOSE_FILE_PATH" | head -1)}"

copy_from_image() {   # copy_from_image <path relative to the repository root>
  echo ">>> $1 missing: copying it from image $IMAGE"
  docker run --rm --entrypoint tar "$IMAGE" -C "$CONTAINER_ROOT" -c "$1" | tar -x \
    || { echo ">>> Could not copy $1 from image $IMAGE" >&2; exit 2; }
}

[[ -e docker_local.env.compose ]] || copy_from_image docker_local.env.compose
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
COMPOSE=(docker compose)
[[ -n "$PROJECT" ]] && COMPOSE+=(-p "$PROJECT")
COMPOSE+=(-f compose/compose.yml)
[[ -n "${CI:-}" ]] && COMPOSE+=(-f compose/compose.ci.yml)
[[ "$OFFLINE" == "true" ]] && COMPOSE+=(-f compose/compose.offline.yml)

CODE_MOUNTS=()
if [[ "$MOUNT_CODE" == "true" ]]; then
  CODE_MOUNTS=(
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
    [[ -f "$f" ]] && CODE_MOUNTS+=(-v "$PWD/$f":"$CONTAINER_ROOT/$f")
  done
fi

# --- Host preparation ------------------------------------------------------------
mkdir -p runtime/jobs runtime/cache
# Output folders that the workflow writes INSIDE the data folder
mkdir -p "$DATA_DIR/dataoutputpreharmo" "$DATA_DIR/dataoutputGTA"
# Without these files, docker would create a folder in their place
[[ -e zenodo_secrets.env ]] || touch zenodo_secrets.env
[[ -e docker_local.env.compose ]] || { echo ">>> docker_local.env.compose is missing" >&2; exit 2; }

echo ">>> Data    : $DATA_DIR"
echo ">>> Steps   : $STEPS"
echo ">>> Project : ${PROJECT:-(default)} | user: $RUN_USER | code mounted: $MOUNT_CODE | data/ mounted: $MOUNT_DATA | offline: $OFFLINE"

docker rm -f "$NAME" >/dev/null 2>&1   # leftover of an interrupted run

"${COMPOSE[@]}" run --rm --name "$NAME" \
  --user "$RUN_USER" \
  ${CODE_MOUNTS[@]+"${CODE_MOUNTS[@]}"} \
  -v "$PWD/docker_local.env.compose":"$CONTAINER_ROOT/docker_local.env.compose:ro" \
  -v "$DATA_DIR":"$CONTAINER_DATA" \
  -v "$PWD/runtime/jobs":"$CONTAINER_ROOT/jobs" \
  -v "$PWD/zenodo_secrets.env":"$CONTAINER_ROOT/zenodo_secrets.env:ro" \
  -v "$PWD/runtime/cache":/cache \
  -e GTA_STEPS="$STEPS" \
  -e GTA_DATA_SOURCE=volume_dir \
  -e GTA_DATA_PATH="$CONTAINER_DATA" \
  -e GTA_SUMMARISE_INVALID_RAW="${GTA_SUMMARISE_INVALID_RAW:-false}" \
  -e GTA_BOOTSTRAP_RESTORE_RENV=false \
  workflow \
  Rscript workflow/launchers/run_gta_2026_workflow_cli.R
status=$?

if [[ $status -eq 0 ]]; then
  echo ">>> Workflow completed successfully"
else
  echo ">>> Workflow failed (exit code $status)"
fi
exit "$status"
