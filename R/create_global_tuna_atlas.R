#' Harmonise the raw datasets of the tRFMOs
#'
#' First half of the Global Tuna Atlas: each raw dataset goes through its
#' core function gta_rawdata_*(), then gta_tidy_raw_dataset() (curation and
#' mapping to the CWP code lists). The result is what the levels read
#' (gta_create_levels()); the workflow writes it to dataoutputpreharmo/.
#'
#' @param raw The raw datasets, as returned by gta_raw_datasets(): a named
#'   list (names: dataset identifiers), each element a list with `dataset`
#'   (e.g. "wcpfc/catch/ps") and `files` (paths of its raw files).
#' @param work_dir Working folder (created): one subfolder per dataset, and
#'   `outputs/<id>.csv`, the harmonised datasets.
#' @param recap_each_step Write the summary of each step (CWP.dataset), in the
#'   folder of the dataset.
#' @return A named list of data frames, the harmonised datasets (names: their
#'   identifiers). The attribute `files` gives the paths of their files
#'   `<id>.csv`, to pass to gta_create_levels().
#' @examples
#' \dontrun{
#' harmonised <- gta_harmonise_raw_datasets(gta_raw_datasets(data_dir = "tests/sample_data"))
#' }
#' @export
gta_harmonise_raw_datasets <- function(raw, work_dir = tempfile("gta_raw_"), recap_each_step = FALSE) {
  work_dir <- gta_work_dir(work_dir)
  outputs_dir <- file.path(work_dir, "outputs")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  raw <- lapply(raw, function(r) { r$files <- gta_abs_path(r$files); r })

  result <- list()
  for (id in names(raw)) {
    r <- raw[[id]]
    d <- file.path(work_dir, "raw", id)
    dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
    setwd(d)
    files <- file.path("data", basename(r$files))
    file.copy(r$files, files, overwrite = TRUE)
    message("Raw dataset ", id)
    harmonized <- do.call(gta_rawdata_function(r$dataset), as.list(files))
    harmonized_file <- file.path(getwd(), "data",
                                 paste0(tools::file_path_sans_ext(basename(r$files[1])), "_harmonized.csv"))
    utils::write.csv(harmonized, harmonized_file, row.names = FALSE)
    dataset <- gta_tidy_raw_dataset(harmonized_file, nominal = grepl("nominal", harmonized_file),
                                    recap_each_step = recap_each_step, data_dir = "data")
    utils::write.csv(dataset, file.path(outputs_dir, paste0(id, ".csv")), row.names = FALSE)
    result[[id]] <- dataset
  }
  attr(result, "files") <- stats::setNames(file.path(outputs_dir, paste0(names(result), ".csv")), names(result))
  result
}

#' Create the Global Tuna Atlas levels from the harmonised datasets
#'
#' Second half of the Global Tuna Atlas: from the harmonised datasets of the
#' tRFMOs (gta_harmonise_raw_datasets(), or files of an earlier run), the
#' nominal catch (gta_nominal_catch()), the level 0 georeferenced catch and
#' the effort (gta_georef_dataset()), the level 1 catch (gta_level1()) and the
#' level 2 catch (gta_georef_dataset()), in this order: a level reads the
#' outputs of the previous ones.
#'
#' Each level reads the files that `options` names (`datasets`, `nominal`,
#' `files`), found by their exact name among the harmonised datasets, the
#' outputs of the previous levels and `files`. A missing name, a name listed
#' twice, or a name given by two different files, is an error.
#'
#' @param harmonised The harmonised datasets: a named list of data frames
#'   (names: identifiers, as returned by gta_harmonise_raw_datasets()), or
#'   paths of their files `<id>.csv`.
#' @param files Paths of the auxiliary files (code lists, conversion factors,
#'   geographic_identifier_to_nominal.csv...); may also hold outputs of
#'   earlier levels, to run only some of them.
#' @param options Datasets and options of the levels, see gta_default_options().
#' @param steps Levels to create, among "nominal", "level0", "effort",
#'   "level1", "level2".
#' @param work_dir Working folder (created): one subfolder per level, and
#'   `outputs/<id>.csv`, the datasets created.
#' @param con Connection to the Tuna Atlas database, or NULL (default): not
#'   needed with the default options.
#' @param recap_each_step Write the summary of each step (CWP.dataset), in the
#'   folder of the level.
#' @return A named list of the levels created (data frames, their public
#'   version): `nominal`, `level0`, `effort`, `level1`, `level2`. The attribute
#'   `work_dir` gives the working folder.
#' @examples
#' \dontrun{
#' atlas <- gta_create_levels(
#'   harmonised = list.files("dataoutputpreharmo", full.names = TRUE),
#'   files = list.files("tests/sample_data", pattern = "\\.csv$", full.names = TRUE),
#'   steps = c("nominal", "level0", "level1"))
#' }
#' @export
gta_create_levels <- function(harmonised, files = character(0), options = gta_default_options(),
                              steps = c("nominal", "level0", "effort", "level1", "level2"),
                              work_dir = tempfile("gta_levels_"), con = NULL, recap_each_step = FALSE) {
  steps <- match.arg(steps, c("nominal", "level0", "effort", "level1", "level2"), several.ok = TRUE)
  work_dir <- gta_work_dir(work_dir)
  outputs_dir <- file.path(work_dir, "outputs")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  # Harmonised datasets: written to (or copied into) the outputs folder
  if (is.data.frame(harmonised)) stop("`harmonised` must be a named list of data frames, or paths of files")
  if (is.list(harmonised)) {
    if (is.null(names(harmonised)) || any(names(harmonised) == "")) stop("`harmonised` must be a named list")
    for (id in names(harmonised)) {
      utils::write.csv(harmonised[[id]], file.path(outputs_dir, paste0(id, ".csv")), row.names = FALSE)
    }
  } else {
    harmonised <- gta_abs_path(harmonised)
    gta_check_unique_names(harmonised, "`harmonised`")
    file.copy(harmonised, file.path(outputs_dir, basename(harmonised)), overwrite = TRUE)
  }
  files <- gta_abs_path(files)

  # Folder of a level: its data/ subfolder holds the files it reads, found by
  # their exact name among the outputs (harmonised datasets and previous
  # levels) and the auxiliary files
  level_dir <- function(o) {
    d <- file.path(work_dir, o$id)
    dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
    needed <- c(o$datasets, o$nominal, o$files)
    gta_check_unique_names(needed, paste("the files of", o$id))
    available <- c(list.files(outputs_dir, full.names = TRUE), files)
    for (name in needed) {
      file.copy(gta_find_file(name, available, o$id), file.path(d, "data", name), overwrite = TRUE)
    }
    setwd(d)
    d
  }
  # Harmonised and public versions written as the workflow does; the public
  # one is the output of the level
  write_outputs <- function(dataset, id) {
    out <- gta_dataset_outputs(dataset)
    readr::write_csv(out$harmonized, file.path("data", paste0(id, "_harmonized.csv")))
    readr::write_csv(out$public, file.path("data", paste0(id, "_public.csv")))
    file.copy(file.path("data", paste0(id, "_public.csv")), file.path(outputs_dir, paste0(id, ".csv")), overwrite = TRUE)
    out$public
  }
  `%||%` <- function(x, y) if (is.null(x)) y else x
  result <- list()

  # Nominal catch ---------------------------------------------------------------
  if ("nominal" %in% steps) {
    o <- options$nominal
    level_dir(o)
    opts <- o$options
    include <- lapply(c(IOTC = "include_IOTC", ICCAT = "include_ICCAT", WCPFC = "include_WCPFC",
                        CCSBT = "include_CCSBT", IATTC = "include_IATTC"),
                      function(k) opts[[k]] %||% TRUE)
    nominal <- gta_nominal_catch(gta_nominal_catch_files(file.path("data", o$datasets), include),
                                 SBF_data_rfmo_to_keep = opts$SBF_data_rfmo_to_keep %||% "CCSBT",
                                 recap_each_step = recap_each_step)
    result$nominal <- write_outputs(nominal, o$id)
  }

  # Georeferenced datasets: level 0 catch, effort, level 2 catch -------------------
  georef <- function(o) {
    level_dir(o)
    opts <- o$options
    opts$recap_each_step <- recap_each_step
    if (!is.null(o$nominal)) opts$keynominal <- o$nominal
    res <- gta_georef_dataset(o$datasets, opts, con = con, entity = NULL, data_dir = "data")
    write_outputs(res$georef_dataset, o$id)
  }
  if ("level0" %in% steps) result$level0 <- georef(options$level0)
  if ("effort" %in% steps) result$effort <- georef(options$effort)

  # Level 1 catch -----------------------------------------------------------------
  if ("level1" %in% steps) {
    o <- options$level1
    level_dir(o)
    opts <- o$options
    level1 <- gta_level1(
      file.path("data", o$datasets[1]),
      con = con,
      unit_conversion_convert = opts$unit_conversion_convert %||% TRUE,
      unit_conversion_iotc_file = opts$unit_conversion_iotc_file %||% "data/IOTC_conv_fact_mapped.csv",
      unit_conversion_ird_file = opts$unit_conversion_ird_file %||% "data/fact_conv_ird_complete_01_May_2026.csv",
      unit_conversion_codelist_geoidentifiers_conversion_factors =
        opts$unit_conversion_codelist_geoidentifiers_conversion_factors %||% "areas_conversion_factors_numtoweight_ird",
      mapping_map_code_lists = opts$mapping_map_code_lists %||% TRUE,
      fact = opts$fact %||% "catch",
      aggregate_on_5deg_data_with_resolution_inferior_to_5deg =
        opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg %||% FALSE,
      resolution_filter = opts$resolution_filter,
      filtering = opts$filtering,
      gear_filter = opts$gear_filter,
      doilevel0 = opts$doilevel0 %||% "10.5281/zenodo.11460074",
      keylevel0 = o$datasets[1],
      recap_each_step = recap_each_step
    )
    result$level1 <- write_outputs(level1, o$id)
  }

  if ("level2" %in% steps) result$level2 <- georef(options$level2)

  attr(result, "work_dir") <- work_dir
  result
}

#' Create the Global Tuna Atlas datasets
#'
#' The whole Global Tuna Atlas chain without geoflow, in two halves that can
#' also be run separately:
#' 1. gta_harmonise_raw_datasets(): the raw datasets of the tRFMOs, harmonised;
#' 2. gta_create_levels(): from them, the nominal catch, the level 0
#'    georeferenced catch, the effort, the level 1 and the level 2 catch.
#'
#' It reproduces the outputs of the geoflow workflow, which runs the same
#' functions.
#'
#' @param data List of the input data:
#'   * `raw`: the raw datasets, as returned by gta_raw_datasets();
#'   * `files`: paths of the auxiliary files read by the levels (conversion
#'     factors, geographic_identifier_to_nominal.csv, code lists...);
#'   * `outputs` (optional): paths of harmonised datasets or levels of an
#'     earlier run (`<id>.csv`), to run only some steps.
#' @param options Datasets and options of the levels, see gta_default_options().
#' @param steps Steps to run, among "rawdata", "nominal", "level0", "effort",
#'   "level1", "level2".
#' @param work_dir Working folder (created): `raw/` for the raw datasets,
#'   `levels/` for the levels.
#' @param con Connection to the Tuna Atlas database, or NULL (default): not
#'   needed with the default options.
#' @param recap_each_step Write the summary of each step (CWP.dataset).
#' @return A named list: the harmonised datasets under `raw` (named by
#'   identifier), and the levels created, `nominal`, `level0`, `effort`,
#'   `level1`, `level2` (data frames, their public version). The attribute
#'   `work_dir` gives the working folder.
#' @examples
#' \dontrun{
#' atlas <- create_global_tuna_atlas(
#'   data = list(raw = gta_raw_datasets(data_dir = "tests/sample_data"),
#'               files = list.files("tests/sample_data", full.names = TRUE)),
#'   steps = c("rawdata", "nominal", "level0"))
#' }
#' @export
create_global_tuna_atlas <- function(data, options = gta_default_options(),
                                     steps = c("rawdata", "nominal", "level0", "effort", "level1", "level2"),
                                     work_dir = tempfile("gta_"), con = NULL, recap_each_step = FALSE) {
  steps <- match.arg(steps, c("rawdata", "nominal", "level0", "effort", "level1", "level2"), several.ok = TRUE)
  work_dir <- gta_work_dir(work_dir)
  result <- list()
  harmonised <- gta_abs_path(data$outputs)
  if ("rawdata" %in% steps) {
    result$raw <- gta_harmonise_raw_datasets(data$raw, file.path(work_dir, "raw"), recap_each_step)
    new <- attr(result$raw, "files")
    harmonised <- c(unname(new), harmonised[!basename(harmonised) %in% basename(new)])
  }
  level_steps <- setdiff(steps, "rawdata")
  if (length(level_steps)) {
    levels <- gta_create_levels(harmonised, data$files, options, level_steps,
                                file.path(work_dir, "levels"), con, recap_each_step)
    result[names(levels)] <- levels
  }
  attr(result, "work_dir") <- work_dir
  result
}

# Helpers -----------------------------------------------------------------------

# Working folder created, with its outputs/ subfolder; absolute path
gta_work_dir <- function(work_dir) {
  dir.create(file.path(work_dir, "outputs"), recursive = TRUE, showWarnings = FALSE)
  normalizePath(work_dir)
}

# Absolute paths of existing files
gta_abs_path <- function(x) if (length(x)) normalizePath(unlist(x), mustWork = TRUE) else character(0)

# Error if the same file name appears twice
gta_check_unique_names <- function(paths, what) {
  dup <- unique(basename(paths)[duplicated(basename(paths))])
  if (length(dup)) stop(sprintf("%s: names given more than once: %s", what, paste(dup, collapse = ", ")), call. = FALSE)
  invisible(TRUE)
}

# The file named `name` among `available`; error if missing, or if two
# different files have this name
gta_find_file <- function(name, available, level) {
  found <- unique(available[basename(available) == name])
  if (!length(found)) stop(sprintf("%s: file %s not found (neither an output of a previous step nor in `files`)",
                                   level, name), call. = FALSE)
  if (length(found) > 1 && length(unique(tools::md5sum(found))) > 1) {
    stop(sprintf("%s: several different files named %s: %s", level, name, paste(found, collapse = ", ")), call. = FALSE)
  }
  found[1]
}
