#!/usr/bin/env bash
# Rscript used by the pre-commit hooks: the R of the current session when
# there is one (RStudio sets R_HOME, e.g. for its Commit button), otherwise
# the Rscript of the PATH. Avoids running a second R installation of the
# machine whose renv library is empty.
if [[ -n "${R_HOME:-}" && -x "$R_HOME/bin/Rscript" ]]; then
  exec "$R_HOME/bin/Rscript" "$@"
fi
exec Rscript "$@"
