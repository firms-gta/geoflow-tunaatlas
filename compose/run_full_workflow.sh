#!/usr/bin/env bash
# =============================================================================
# compose/run_full_workflow.sh
#
# Runs the COMPLETE GTA workflow on the real data, in the Docker Compose stack
# (Postgres, GeoServer, GeoNetwork).
#
# It is a wrapper around compose/run_workflow.sh: same mounts, only the
# default values change.
#
# Usage (from any folder):
#   ./compose/run_full_workflow.sh
#   GTA_STEPS=level1,level2,services ./compose/run_full_workflow.sh
#   GTA_COMPOSE_PROJECT=gta-full ./compose/run_full_workflow.sh   # isolated stack
#
# Variables (on top of those of run_workflow.sh):
#   GTA_DATA_DIR   default: runtime/extracted/all_raw_data_GTA
#   GTA_STEPS      default: DB,rawdata,effort,nominal,level0,level1,level2,services
#
# WARNING: the "services" step publishes to GeoServer, GeoNetwork and Zenodo
# according to docker_local.env.compose (RUN_ZENODO_T_F) and zenodo_secrets.env
# (ZENODO_URL / ZENODO_TOKEN). Check both files before a complete run: a
# deposit on zenodo.org (not sandbox.zenodo.org) creates a permanent DOI.
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")/.."   # repository root

export GTA_DATA_DIR="${GTA_DATA_DIR:-runtime/extracted/all_raw_data_GTA}"
export GTA_STEPS="${GTA_STEPS:-DB,rawdata,effort,nominal,level0,level1,level2,services}"

if [[ ! -d "$GTA_DATA_DIR" ]]; then
  cat >&2 <<EOF
>>> Data folder not found: $GTA_DATA_DIR
    Extract the all_raw_data_GTA.zip archive (Zenodo 10.5281/zenodo.20834708)
    into runtime/extracted/, or point GTA_DATA_DIR to another folder.
EOF
  exit 2
fi

# Show the Zenodo target before a publication starts
if [[ ",$GTA_STEPS," == *",services,"* || ",$GTA_STEPS," == *",all,"* ]]; then
  zenodo_url=$(grep -E '^\s*ZENODO_URL=' zenodo_secrets.env 2>/dev/null | tail -1 | cut -d= -f2- || true)
  echo ">>> services step included: Zenodo publication to ${zenodo_url:-<ZENODO_URL not set>}"
fi

exec ./compose/run_workflow.sh
