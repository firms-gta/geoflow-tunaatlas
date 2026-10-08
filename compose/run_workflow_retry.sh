#!/usr/bin/env bash
# =============================================================================
# compose/run_workflow_retry.sh
#
# Lance le workflow GTA dans la stack Docker Compose (Postgres, GeoServer,
# GeoNetwork) et le relance si R reste bloqué sur "recursive gc invocation".
#
# Par défaut : run de TEST sur l'échantillon tests/sample_data.
# Pour le workflow complet sur les vraies données : compose/run_full_workflow.sh
#
# Usage (depuis n'importe quel dossier) :
#   ./compose/run_workflow_retry.sh
#   GTA_STEPS=rawdata,nominal ./compose/run_workflow_retry.sh
#   GTA_DATA_DIR=/chemin/vers/donnees ./compose/run_workflow_retry.sh
#
# Variables (toutes facultatives) :
#   GTA_DATA_DIR         dossier de données sur l'hôte     (défaut : tests/sample_data)
#   GTA_STEPS            étapes du workflow                (défaut : DB,rawdata,nominal,level0,services)
#   GTA_COMPOSE_PROJECT  nom de projet compose, pour isoler la stack (conteneurs + volumes)
#   GTA_RUN_USER         uid:gid du conteneur              (défaut : utilisateur courant ; CI : 1000:1000)
#   GTA_MOUNT_CODE       false : code de l'image ; true : monte R/ et config/ du dépôt,
#                        pour tester une modification sans reconstruire l'image (défaut : false)
#   GTA_MOUNT_DATA       true : monte aussi les fichiers de data/ du dépôt (défaut : false)
#   GTA_MAX_ATTEMPTS     nombre de tentatives en cas de blocage GC   (défaut : 3)
#   GTA_GC_TIMEOUT       secondes sans ligne normale après un message GC avant d'abandonner (défaut : 60)
# =============================================================================
set -uo pipefail
cd "$(dirname "$0")/.."   # racine du dépôt

DATA_DIR="${GTA_DATA_DIR:-tests/sample_data}"
STEPS="${GTA_STEPS:-DB,rawdata,nominal,level0,services}"
PROJECT="${GTA_COMPOSE_PROJECT:-}"
RUN_USER="${GTA_RUN_USER:-$(id -u):$(id -g)}"
MOUNT_CODE="${GTA_MOUNT_CODE:-false}"
MOUNT_DATA="${GTA_MOUNT_DATA:-false}"
MAX_ATTEMPTS="${GTA_MAX_ATTEMPTS:-3}"
GC_TIMEOUT="${GTA_GC_TIMEOUT:-60}"

NAME="${PROJECT:-gta}-workflow-run"
CONTAINER_ROOT=/home/rstudio/geoflow-tunaatlas
CONTAINER_DATA="$CONTAINER_ROOT/data/GTA_2026"

# --- Fichiers manquants : on les prend dans l'image ---------------------------
# Permet de lancer la stack sans cloner le dépôt, en n'ayant copié que le
# dossier compose/ (voir compose/README.md, "Without cloning the repository").
COMPOSE_FILE_PATH=compose/compose.bd.rstudio.newversiongeoflow.yml
IMAGE="${GTA_IMAGE:-$(sed -n 's/.*image: \${GTA_IMAGE:-\([^}]*\)}.*/\1/p' "$COMPOSE_FILE_PATH" | head -1)}"

copy_from_image() {   # copy_from_image <chemin relatif à la racine du dépôt>
  echo ">>> $1 absent : copie depuis l'image $IMAGE"
  docker run --rm --entrypoint tar "$IMAGE" -C "$CONTAINER_ROOT" -c "$1" | tar -x \
    || { echo ">>> Impossible de copier $1 depuis l'image $IMAGE" >&2; exit 2; }
}

[[ -e docker_local.env.compose ]] || copy_from_image docker_local.env.compose
if [[ -z "${GTA_DATA_DIR:-}" && ! -d tests/sample_data ]]; then
  copy_from_image tests/sample_data
fi

# Code : celui de l'image par défaut ; celui du dépôt avec GTA_MOUNT_CODE=true.
if [[ "$MOUNT_CODE" == "true" && ! ( -d R && -d config ) ]]; then
  echo ">>> GTA_MOUNT_CODE=true mais R/ ou config/ est absent" >&2
  exit 2
fi

# --- Chemins absolus (docker n'accepte que des chemins absolus pour -v) ------
case "$DATA_DIR" in
  /*) ;;
  *)  DATA_DIR="$PWD/$DATA_DIR" ;;
esac

if [[ ! -d "$DATA_DIR" ]] || [[ -z "$(ls -A "$DATA_DIR" 2>/dev/null)" ]]; then
  echo ">>> Dossier de données absent ou vide : $DATA_DIR" >&2
  exit 2
fi

# --- Commande compose ----------------------------------------------------------
COMPOSE=(docker compose)
[[ -n "$PROJECT" ]] && COMPOSE+=(-p "$PROJECT")
COMPOSE+=(-f compose/compose.bd.rstudio.newversiongeoflow.yml)
[[ -n "${CI:-}" ]] && COMPOSE+=(-f compose/compose.ci.yml)

CODE_MOUNTS=()
if [[ "$MOUNT_CODE" == "true" ]]; then
  CODE_MOUNTS=(
    -v "$PWD/R":"$CONTAINER_ROOT/R"
    -v "$PWD/config":"$CONTAINER_ROOT/config"
  )
fi

# Fichiers de référence de data/ (listes de codes, paramètres...). On monte les
# fichiers un par un, pas le dossier : data/ contient aussi, dans l'image
# seulement, fdi-codelists/ et fdi-mappings/, qu'un montage du dossier masquerait.
if [[ "$MOUNT_DATA" == "true" ]]; then
  [[ -d data ]] || { echo ">>> GTA_MOUNT_DATA=true mais data/ est absent" >&2; exit 2; }
  for f in data/*; do
    [[ -f "$f" ]] && CODE_MOUNTS+=(-v "$PWD/$f":"$CONTAINER_ROOT/$f")
  done
fi

# --- Préparation de l'hôte -----------------------------------------------------
mkdir -p runtime/jobs runtime/cache
# Dossiers de sortie que le workflow écrit DANS le dossier de données
mkdir -p "$DATA_DIR/dataoutputpreharmo" "$DATA_DIR/dataoutputGTA"
# Sans ces fichiers, docker créerait un dossier à leur place
[[ -e zenodo_secrets.env ]] || touch zenodo_secrets.env
[[ -e docker_local.env.compose ]] || { echo ">>> docker_local.env.compose manquant" >&2; exit 2; }

echo ">>> Données : $DATA_DIR"
echo ">>> Étapes  : $STEPS"
echo ">>> Projet  : ${PROJECT:-(défaut)} | utilisateur : $RUN_USER | code monté : $MOUNT_CODE | data/ monté : $MOUNT_DATA"

run_once() {
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
    Rscript R/launching_workflows/run_gta_2026_workflow_cli.R 2>&1 |
  {
    # Un message GC isolé n'est pas bloquant : on ne tue le conteneur que si
    # aucune ligne normale n'a suivi pendant GC_TIMEOUT secondes.
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

docker rm -f "$NAME" >/dev/null 2>&1   # reste éventuel d'un run interrompu

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
