# Analyses

One-off exploratory analyses (data checks, comparisons, reports).

- Not part of the workflow: nothing in `R/` or `workflow/` sources these files.
- Not maintained: they may rely on old paths or local job directories.
- Not copied into the Docker image (see `.dockerignore`).

A script that becomes part of a regular run moves to `R/` (functions) or
`workflow/` (launching code).
