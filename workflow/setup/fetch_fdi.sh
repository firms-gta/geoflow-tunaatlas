#!/usr/bin/env bash
# =============================================================================
# workflow/setup/fetch_fdi.sh
#
# FDI code lists and mappings at the pinned commits of config/fdi_versions.conf,
# in data/fdi-codelists and data/fdi-mappings. The Dockerfile runs this same
# script, so the image and a local copy hold exactly the same files.
#
# Usage (from any folder, e.g. the RStudio terminal):
#   ./workflow/setup/fetch_fdi.sh            # outside Docker
#   system2("workflow/setup/fetch_fdi.sh")   # from the R console
#
# Only the pinned commit is fetched (--depth 1), not the whole history.
# Outside Docker the .git folder is kept: a second run with the same versions
# downloads nothing. The image build passes --no-git.
# =============================================================================
set -euo pipefail
root="$(cd "$(dirname "$0")/../.." && pwd)"
keep_git=true
[[ "${1:-}" == "--no-git" ]] && keep_git=false

versions="$root/config/fdi_versions.conf"
[[ -f "$versions" ]] || { echo ">>> $versions not found" >&2; exit 2; }
while IFS='=' read -r key value; do
  case "$key" in FDI_*) printf -v "$key" '%s' "$value" ;; esac
done < <(grep -E '^FDI_[A-Z_]+=' "$versions")

fetch() {   # fetch <repository> <commit> <folder>
  local repo="$1" ref="$2" dir="$3"
  if [[ -d "$dir/.git" ]] && [[ "$(git -C "$dir" rev-parse HEAD 2>/dev/null)" == "$ref" ]]; then
    echo ">>> $dir: already at $ref"
    return
  fi
  rm -rf "$dir"
  mkdir -p "$dir"
  git -C "$dir" init -q
  git -C "$dir" remote add origin "$repo"
  git -C "$dir" fetch -q --depth 1 origin "$ref"
  git -C "$dir" checkout -q FETCH_HEAD
  $keep_git || rm -rf "$dir/.git"
  echo ">>> $dir: $repo at $ref"
}

fetch "$FDI_CODELISTS_REPO" "$FDI_CODELISTS_REF" "$root/data/fdi-codelists"
fetch "$FDI_MAPPINGS_REPO" "$FDI_MAPPINGS_REF" "$root/data/fdi-mappings"
