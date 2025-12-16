#!/usr/bin/env bash
set -euo pipefail

exec Rscript ./R/netcdf_creation/launching_netcdf_creation.R "$@"