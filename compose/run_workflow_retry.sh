#!/usr/bin/env bash
# compose/run_workflow_retry.sh
# Lance le workflow GTA et le relance si R reste bloqué sur "recursive gc invocation".
# Usage : ./compose/run_workflow_retry.sh   (depuis n'importe quel dossier)
set -uo pipefail
cd "$(dirname "$0")/.."   # racine du repo

MAX_ATTEMPTS=3
GC_TIMEOUT=60             # secondes sans ligne normale après un message GC
NAME=gta-workflow-run

COMPOSE=(docker compose -f compose/compose.bd.rstudio.newversiongeoflow.yml)
[[ -n "${CI:-}" ]] && COMPOSE+=(-f compose/compose.ci.yml)

run_once() {
  "${COMPOSE[@]}" run --rm --name "$NAME" \
    --user "$(id -u):$(id -g)" \
    -v "$PWD/R":/home/rstudio/geoflow-tunaatlas/R \
    -v "$PWD/config":/home/rstudio/geoflow-tunaatlas/config \
    -v "$PWD/docker_local.env.compose":/home/rstudio/geoflow-tunaatlas/docker_local.env.compose:ro \
    -v "$PWD/tests/sample_data":/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
    -v "$PWD/runtime/jobs":/home/rstudio/geoflow-tunaatlas/jobs \
    -v "$PWD/zenodo_secrets.env":/home/rstudio/geoflow-tunaatlas/zenodo_secrets.env:ro \
    -v "$PWD/runtime/cache":/cache \
    -e GTA_STEPS="${GTA_STEPS:-DB,rawdata,nominal,level0,services}" \
    -e GTA_DATA_SOURCE=volume_dir \
    -e GTA_DATA_PATH=/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
    -e GTA_SUMMARISE_INVALID_RAW=false \
    -e GTA_BOOTSTRAP_RESTORE_RENV=false \
    workflow \
    Rscript R/launching_workflows/run_gta_2026_workflow_cli.R 2>&1 |
  {
    gc_since=""
    while true; do
      if IFS= read -r -t 5 line; then
        printf '%s\n' "$line"
        if [[ "$line" == *"recursive gc invocation"* ]]; then
          [[ -z "$gc_since" ]] && gc_since=$SECONDS
        else
          gc_since=""
        fi
      else
        (( $? > 128 )) || break   # > 128 : délai de 5 s écoulé ; sinon fin de sortie
      fi

      if [[ -n "$gc_since" ]] && (( SECONDS - gc_since >= GC_TIMEOUT )); then
        echo ">>> Bug GC bloquant (aucune ligne normale depuis ${GC_TIMEOUT} s), arrêt du conteneur"
        docker kill "$NAME" >/dev/null 2>&1
        exit 42
      fi
    done
  }
}

mkdir -p runtime/jobs runtime/cache
[[ -e zenodo_secrets.env ]] || touch zenodo_secrets.env

for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
  echo ">>> Tentative $attempt/$MAX_ATTEMPTS"
  run_once
  status=$?

  if [[ $status -eq 0 ]]; then
    echo ">>> Workflow terminé avec succès"
    exit 0
  elif [[ $status -eq 42 ]]; then
    docker rm -f "$NAME" >/dev/null 2>&1
    sleep 5
  else
    echo ">>> Échec du workflow (code $status), pas de relance"
    exit "$status"
  fi
done

echo ">>> Bug GC à chaque tentative, abandon"
exit 1