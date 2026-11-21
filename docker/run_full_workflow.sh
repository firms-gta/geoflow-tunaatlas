#!/usr/bin/env bash
# =============================================================================
# docker/run_full_workflow.sh
#
# Runs the COMPLETE GTA workflow on the real data, in the Docker Compose stack
# (Postgres, GeoServer, GeoNetwork). A wrapper around docker/run_workflow.sh:
# same mounts; the data folder is GTA_DATA_DIR from .env and every step runs.
#
# Usage (from any folder):
#   ./docker/run_full_workflow.sh
#   GTA_STEPS=level1,level2,services ./docker/run_full_workflow.sh
#   GTA_COMPOSE_PROJECT=gta-full ./docker/run_full_workflow.sh   # isolated stack
#
# WARNING: the "services" step publishes to GeoServer, GeoNetwork and Zenodo
# according to .env (RUN_ZENODO_T_F, ZENODO_URL, ZENODO_TOKEN). Check .env
# before a complete run: a deposit on zenodo.org (not sandbox.zenodo.org)
# creates a permanent DOI.
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")/.."   # repository root

if [[ ! -f .env ]]; then
  echo ">>> No .env at the repository root: cp env.example .env, then fill it in" >&2
  exit 2
fi
env_value() {   # env_value <NAME>: value of NAME in .env (quotes removed)
  grep -E "^$1=" .env | tail -1 | cut -d= -f2- | sed -E 's/^"(.*)"$/\1/' || true
}

export GTA_DATA_DIR="${GTA_DATA_DIR:-$(env_value GTA_DATA_DIR)}"
export GTA_STEPS="${GTA_STEPS:-DB,rawdata,effort,nominal,level0,level1,level2,services}"

if [[ -z "$GTA_DATA_DIR" || ! -d "$GTA_DATA_DIR" ]]; then
  cat >&2 <<EOF
>>> Data folder not found: ${GTA_DATA_DIR:-<GTA_DATA_DIR not set in .env>}
    Extract the all_raw_data_GTA.zip archive (Zenodo 10.5281/zenodo.20834708)
    there, or set GTA_DATA_DIR (in .env, or when calling this script).
EOF
  exit 2
fi

# Show the Zenodo target before a publication starts
if [[ ",$GTA_STEPS," == *",services,"* || ",$GTA_STEPS," == *",all,"* ]]; then
  echo ">>> services step included: RUN_ZENODO_T_F=$(env_value RUN_ZENODO_T_F), Zenodo: $(env_value ZENODO_URL)"
fi

exec ./docker/run_workflow.sh
