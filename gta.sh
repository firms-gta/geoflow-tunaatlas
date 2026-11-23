#!/usr/bin/env bash
# =============================================================================
# gta.sh: single entry point to run the Global Tuna Atlas workflow (Docker
# only, no local R needed). Run ./gta.sh help for the commands.
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")"

ROOT=/home/rstudio/geoflow-tunaatlas
DOI_DEFAULT=10.5281/zenodo.20834708
DOI_FILE_DEFAULT=all_raw_data_GTA.zip

usage() {
  cat <<'EOF'
Usage: ./gta.sh <command>

  test      Tests of the gta package and of the raw datasets, on the control
            data, with the code of this repository (a few minutes)
  sample    Whole workflow on the sample data, offline, with the database
            (same as the CI): checks that everything runs
  produce   Produce all the datasets from the input archive on Zenodo,
            without database or publication; outputs in runtime/
  publish   Complete run on the real data with the database and the
            publication (GeoServer, GeoNetwork, Zenodo according to .env)
  help      This help

Settings: .env (created from env.example the first time). Options for a
single run are environment variables, e.g.
  GTA_STEPS=rawdata,nominal ./gta.sh sample
  GTA_IMAGE=ghcr.io/firms-gta/gta-workflow:sha-<commit> ./gta.sh produce
See README.md and docker/README.md.
EOF
}

ensure_env() {
  if [[ ! -f .env ]]; then
    cp env.example .env
    echo ">>> .env created from env.example: check it before a publication run"
  fi
}

env_value() {   # value of a variable in .env, quotes removed
  grep -E "^$1=" .env 2>/dev/null | tail -1 | cut -d= -f2- | sed -E 's/^"(.*)"$/\1/' || true
}

image() {
  local img="${GTA_IMAGE:-$(env_value GTA_IMAGE)}"
  echo "${img:-ghcr.io/firms-gta/gta-workflow:latest}"
}

cmd_test() {
  local img; img="$(image)"
  echo ">>> Tests in $img, with the code of this repository"
  local mounts=()
  for p in R config workflow tests DESCRIPTION NAMESPACE .Rprofile; do
    mounts+=(-v "$PWD/$p":"$ROOT/$p":ro)
  done
  docker run --rm --network none --user "$(id -u):$(id -g)" "${mounts[@]}" \
    -w "$ROOT" --entrypoint Rscript "$img" -e '
      stopifnot(system2("Rscript", "tests/test_rawdata.R") == 0)
      testthat::test_local(stop_on_failure = TRUE)'
}

cmd_sample() {
  ensure_env
  GTA_OFFLINE="${GTA_OFFLINE:-true}" ./docker/run_workflow.sh
}

cmd_produce() {
  ensure_env
  local img; img="$(image)"
  mkdir -p runtime/extracted runtime/jobs runtime/cache
  echo ">>> Producing the datasets with $img (archive ${GTA_DOI:-$DOI_DEFAULT})"
  docker run --rm \
    --user "$(id -u):$(id -g)" \
    -v "$PWD/runtime/extracted":"$ROOT/data/GTA_2026" \
    -v "$PWD/runtime/jobs":"$ROOT/jobs" \
    -v "$PWD/.env":"$ROOT/.env":ro \
    -v "$PWD/runtime/cache":/cache \
    -e GTA_STEPS="${GTA_STEPS:-rawdata,nominal,effort,level0,level1,level2}" \
    -e GTA_DATA_SOURCE=doi \
    -e GTA_DOI="${GTA_DOI:-$DOI_DEFAULT}" \
    -e GTA_DOI_FILE="${GTA_DOI_FILE:-$DOI_FILE_DEFAULT}" \
    -e GTA_BOOTSTRAP_RESTORE_RENV=false \
    "$img"
  echo ">>> Done: datasets in runtime/extracted/dataoutputGTA, jobs and logs in runtime/jobs"
}

cmd_publish() {
  ensure_env
  if [[ "$(env_value RUN_ZENODO_T_F)" =~ ^(TRUE|true|T)$ ]]; then
    echo ">>> Zenodo publication is ON (RUN_ZENODO_T_F in .env), target: $(env_value ZENODO_URL)"
    read -r -p ">>> Continue? [y/N] " answer
    [[ "$answer" =~ ^[yY]$ ]] || { echo "Cancelled."; exit 1; }
  fi
  ./docker/run_full_workflow.sh
}

case "${1:-help}" in
  test) cmd_test ;;
  sample) cmd_sample ;;
  produce) cmd_produce ;;
  publish) cmd_publish ;;
  help|-h|--help) usage ;;
  *) echo "Unknown command: $1"; echo; usage; exit 1 ;;
esac
