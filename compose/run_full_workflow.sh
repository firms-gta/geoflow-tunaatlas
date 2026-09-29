#!/usr/bin/env bash
# =============================================================================
# compose/run_full_workflow.sh
#
# Lance le workflow GTA COMPLET sur les vraies données, dans la stack Docker
# Compose (Postgres, GeoServer, GeoNetwork), avec relance automatique en cas de
# blocage "recursive gc invocation".
#
# C'est une surcouche de compose/run_workflow_retry.sh : mêmes montages, même
# détection du bug GC, seules les valeurs par défaut changent.
#
# Usage (depuis n'importe quel dossier) :
#   ./compose/run_full_workflow.sh
#   GTA_STEPS=level1,level2,services ./compose/run_full_workflow.sh
#   GTA_COMPOSE_PROJECT=gta-full ./compose/run_full_workflow.sh   # stack isolée
#
# Variables (en plus de celles de run_workflow_retry.sh) :
#   GTA_DATA_DIR   défaut : runtime/extracted/all_raw_data_GTA
#   GTA_STEPS      défaut : DB,rawdata,effort,nominal,level0,level1,level2,services
#   GTA_GC_TIMEOUT défaut : 300 (les étapes sur données complètes peuvent rester
#                  longtemps silencieuses)
#
# ATTENTION : l'étape "services" publie sur GeoServer, GeoNetwork et Zenodo
# selon docker_local.env.compose (RUN_ZENODO_T_F) et zenodo_secrets.env
# (ZENODO_URL / ZENODO_TOKEN). Vérifiez ces deux fichiers avant un run complet :
# un dépôt sur zenodo.org (et non sandbox.zenodo.org) crée un DOI définitif.
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")/.."   # racine du dépôt

export GTA_DATA_DIR="${GTA_DATA_DIR:-runtime/extracted/all_raw_data_GTA}"
export GTA_STEPS="${GTA_STEPS:-DB,rawdata,effort,nominal,level0,level1,level2,services}"
export GTA_GC_TIMEOUT="${GTA_GC_TIMEOUT:-300}"

if [[ ! -d "$GTA_DATA_DIR" ]]; then
  cat >&2 <<EOF
>>> Dossier de données introuvable : $GTA_DATA_DIR
    Extrayez l'archive all_raw_data_GTA.zip (Zenodo 10.5281/zenodo.20834708)
    dans runtime/extracted/, ou indiquez un autre dossier avec GTA_DATA_DIR.
EOF
  exit 2
fi

# Rappel de la cible Zenodo avant de lancer une publication
if [[ ",$GTA_STEPS," == *",services,"* || ",$GTA_STEPS," == *",all,"* ]]; then
  zenodo_url=$(grep -E '^\s*ZENODO_URL=' zenodo_secrets.env 2>/dev/null | tail -1 | cut -d= -f2- || true)
  echo ">>> Étape services incluse : publication Zenodo vers ${zenodo_url:-<ZENODO_URL non défini>}"
fi

exec ./compose/run_workflow_retry.sh
