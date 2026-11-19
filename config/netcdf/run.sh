#!/usr/bin/env bash
set -euo pipefail

# 1) Mode CCP : si la plateforme override la commande avec l'entrypoint CCP, on respecte, on fait ça pour que ca marche
# sur CCP et sur Edito datalab
if [ "${1:-}" = "/ccp_data/ccp-entrypoint.sh" ]; then
  exec "$@"
fi

# 2) Mode "processInputs pour edito ou local" : on lit les variables d'environnement injectees
: "${path:=/ccp_data/outputs}"
: "${name:=majortunasall}"
: "${dimensions:=species_label,gear_type_label,fishing_fleet_label}"
: "${parameterfiltering:=}"
: "${Onebyspecies:=true}"

mkdir -p "$path"

exec Rscript ./config/netcdf/launching_netcdf_creation.R \
  --path "$path" \
  --name "$name" \
  --dimensions "$dimensions" \
  --parameter_filtering "$parameterfiltering" \
  --one_by_species "$Onebyspecies"
