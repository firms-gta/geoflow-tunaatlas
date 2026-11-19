# =============================================================================
# Files a geoflow configuration depends on, for {targets}.
#
# gta_config_deps("config/geoflow/catch_ird_level0_local.json") returns, recursively:
#   * the JSON itself;
#   * every file it names: entity CSVs, action scripts ("script": "../actions/..."),
#     onstart/onend SQL;
#   * every file named in those entity CSVs: generation scripts
#     (action:...@./R/...R), code-list files (./data/..._code_lists.csv);
#     input data under data/GTA_2026/ only with include_data = TRUE (the
#     outputs of other steps are linked explicitly in the pipeline instead);
#   * every R file sourced by those scripts, including the
#     file.path(url_scripts_create_own_tuna_atlas, "x.R") form, resolved by
#     file name anywhere under R/ or workflow/.
#
# Used with format = "file": a change in any of these files re-runs the step.
# Files are read only to look for other file names when they are small text
# files (R, JSON, SQL, CSV under 2 MB); data files and binaries are tracked
# (their hash is checked) but never read.
#
# It errs on the side of re-running: a file name found in a string literal is
# counted even if the code path that uses it is not taken.
# =============================================================================

gta_config_deps <- function(config_file, root = ".", include_data = FALSE,
                            max_scan_bytes = 2e6) {
  exts <- "\\.(R|csv|json|sql|qs|xlsx|zip)$"
  norm <- function(p) sub("^(\\.\\./|\\./)+", "", trimws(p))
  # In a workflow JSON, geoflow resolves "../x" against the JSON's own folder
  # (geoflow:::get_config_resource_path): one dirname() per "../".
  resolve_from <- function(from, p) {
    p <- trimws(p)
    if (!grepl("^\\.\\./", p)) return(norm(p))
    n <- lengths(regmatches(p, gregexpr("\\.\\./", p)))
    dir <- dirname(from)
    for (i in seq_len(n)) dir <- dirname(dir)
    rel <- gsub("\\.\\./", "", p)
    if (dir == ".") rel else file.path(dir, rel)
  }

  quoted_strings <- function(txt) {
    m <- regmatches(txt, gregexpr('"[^"\n]*"', txt))[[1]]
    gsub('^"|"$', "", m)
  }
  bare_paths <- function(txt) {
    regmatches(txt, gregexpr(
      "(\\.\\./|\\./)?(R|config|workflow|data|compose)/[A-Za-z0-9_./+-]+\\.(R|csv|json|sql|qs|xlsx|zip)",
      txt))[[1]]
  }

  r_files <- unlist(lapply(c("R", "workflow"), function(d)
    file.path(d, list.files(file.path(root, d), pattern = "\\.R$", recursive = TRUE))))
  r_by_name <- split(r_files, basename(r_files))

  seen <- character(0)
  queue <- norm(config_file)
  while (length(queue) > 0) {
    f <- queue[1]
    queue <- queue[-1]
    if (f %in% seen || !file.exists(file.path(root, f))) next
    is_input_data <- startsWith(f, "data/GTA_2026/")
    if (is_input_data && !include_data) next
    seen <- c(seen, f)

    # Only small text files can name other files: data files, binaries and
    # large tables are tracked but never read.
    if (is_input_data || !grepl("\\.(R|json|sql|csv)$", f) ||
        file.size(file.path(root, f)) > max_scan_bytes) next

    txt <- paste(readLines(file.path(root, f), warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
    raw <- unique(c(quoted_strings(txt), bare_paths(txt)))
    found <- unique(if (grepl("\\.json$", f)) vapply(raw, resolve_from, "", from = f) else norm(raw))
    found <- found[grepl(exts, found)]

    with_dir <- found[grepl("/", found)]
    if (grepl("\\.R$", f)) {
      # "x.R" without directory: script sourced through a variable path
      bare <- found[!grepl("/", found) & grepl("\\.R$", found)]
      with_dir <- c(with_dir, unlist(r_by_name[bare], use.names = FALSE))
    }
    queue <- c(queue, setdiff(unique(with_dir), seen))
  }
  sort(file.path(root, seen))
}

# Launcher code shared by every step.
gta_launcher_deps <- function(root = ".") {
  f <- c(
    "workflow/launchers/GTA_2026_creation.R",
    "workflow/launchers/workflow_helpers.R",
    "workflow/launchers/zenodo_helpers.R",
    "R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R",
    "workflow/launchers/executeAndRename.R",
    "workflow/launchers/running_time_of_workflow.R",
    "workflow/launchers/targets/targets_deps.R",
    "docker_local.env.compose"
  )
  file.path(root, f[file.exists(file.path(root, f))])
}
