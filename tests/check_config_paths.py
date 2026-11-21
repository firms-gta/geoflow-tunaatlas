#!/usr/bin/env python3
"""Check that every local file named by the geoflow configuration exists and is
tracked by git.

Catches the "file present on my machine but never committed" mistake before it
reaches the CI (the image only contains tracked files).

Checked:
* config/geoflow/*.json: "script", "onstart_sql" and "onend_sql" are resolved
  like geoflow does, relative to the JSON folder (one level up per "../");
  "source" values starting with R/, config/, workflow/, data/ or compose/ are
  resolved from the repository root.
* config/entities/*.csv: local sources written "./<dir>/..." are resolved from
  the repository root.

Not checked (not in git on purpose):
* data/fdi-codelists, data/fdi-mappings: cloned into the Docker image;
* data/GTA_2026: input data mounted when the workflow runs.

Usage, from the repository root: python3 tests/check_config_paths.py
Exit status 0 when everything is found, 1 otherwise.
"""
import glob
import json
import os
import re
import subprocess
import sys

LOCAL_DIRS = ("R/", "config/", "workflow/", "data/", "compose/")
NOT_IN_GIT = ("data/fdi-codelists", "data/fdi-mappings", "data/GTA_2026")
JSON_RELATIVE_KEYS = ("script", "onstart_sql", "onend_sql")
ENTITY_SOURCE = re.compile(r"\./((?:R|config|workflow|data|compose)/[^,;\"\s@]+)")

tracked = set(
    subprocess.run(["git", "ls-files"], capture_output=True, text=True, check=True)
    .stdout.splitlines()
)
problems = []


def check_file(origin, shown, path):
    path = os.path.normpath(path)
    if path.startswith(NOT_IN_GIT):
        return
    if not os.path.exists(path):
        problems.append(f"{origin}: missing file {shown}")
    elif os.path.isfile(path) and path not in tracked:
        problems.append(f"{origin}: {shown} exists but is not tracked by git")


def from_json_folder(json_file, value):
    folder = os.path.dirname(json_file)
    while value.startswith("../"):
        folder, value = os.path.dirname(folder), value[3:]
    return os.path.join(folder, value)


def walk(node):
    if isinstance(node, dict):
        for key, value in node.items():
            if isinstance(value, str):
                yield key, value
            else:
                yield from walk(value)
    elif isinstance(node, list):
        for value in node:
            yield from walk(value)


for json_file in sorted(glob.glob("config/geoflow/*.json")):
    if json_file.endswith("_nodb.json"):  # generated, not tracked
        continue
    try:
        with open(json_file, encoding="utf-8") as fh:
            config = json.load(fh)
    except ValueError as err:
        problems.append(f"{json_file}: invalid JSON ({err})")
        continue
    for key, value in walk(config):
        if key in JSON_RELATIVE_KEYS and not value.startswith("http"):
            check_file(json_file, value, from_json_folder(json_file, value))
        elif key == "source" and value.startswith(LOCAL_DIRS):
            check_file(json_file, value, value)

for csv_file in sorted(glob.glob("config/entities/*.csv")):
    with open(csv_file, encoding="utf-8", errors="replace") as fh:
        text = fh.read()
    for match in ENTITY_SOURCE.finditer(text):
        path = match.group(1).rstrip("_")  # "_" separates geoflow entries
        check_file(csv_file, "./" + path, path)

if problems:
    print("Configuration paths:", *problems, sep="\n  - ")
    sys.exit(1)
