#' Create the Global Tuna Atlas datasets
#'
#' Runs the whole Global Tuna Atlas chain without geoflow: the raw datasets of
#' the tRFMOs (their gta_rawdata_*() function, then gta_tidy_raw_dataset()),
#' then the nominal catch (gta_nominal_catch()), the level 0 georeferenced
#' catch and the effort (gta_georef_dataset()), the level 1 catch
#' (gta_level1()) and the level 2 catch (gta_georef_dataset()). It reproduces
#' the outputs of the geoflow workflow, which runs the same functions.
#'
#' Each step runs in its own folder of `work_dir` (with a data/ subfolder, as
#' an entity in a geoflow job); the output of each dataset is written to
#' `work_dir/outputs/<id>.csv`, where the next steps read it (as the workflow
#' does with dataoutputpreharmo/ and dataoutputGTA/).
#'
#' @param data List of the input data:
#'   * `raw`: the raw datasets, as returned by gta_raw_datasets() (a named
#'     list, names = dataset identifiers, elements = list(dataset, files));
#'   * `files`: paths of the auxiliary files read by the levels (conversion
#'     factors, geographic_identifier_to_nominal.csv, code lists...), found by
#'     their name;
#'   * `outputs` (optional): paths of outputs of earlier runs (<id>.csv), to
#'     run only some steps.
#' @param options Datasets and options of the levels, see gta_default_options().
#' @param steps Steps to run, among "rawdata", "nominal", "level0", "effort",
#'   "level1", "level2" (in this order).
#' @param work_dir Working folder (created).
#' @param con Connection to the Tuna Atlas database, or NULL (default): not
#'   needed with the default options.
#' @param recap_each_step Write the summary of each step (CWP.dataset), in the
#'   folder of the step.
#' @return A named list of the datasets created (data frames): the raw
#'   datasets under `raw` (named by identifier), and `nominal`, `level0`,
#'   `effort`, `level1`, `level2` (their public version). The attribute
#'   `work_dir` gives the working folder.
#' @examples
#' \dontrun{
#' atlas <- create_global_tuna_atlas(
#'   data = list(raw = gta_raw_datasets(data_dir = "tests/sample_data"),
#'               files = list.files("tests/sample_data", full.names = TRUE)),
#'   steps = c("rawdata", "nominal", "level0"))
#' }
create_global_tuna_atlas <- function(data, options = gta_default_options(),
                                     steps = c("rawdata", "nominal", "level0", "effort", "level1", "level2"),
                                     work_dir = tempfile("gta_"), con = NULL, recap_each_step = FALSE) {
  steps <- match.arg(steps, c("rawdata", "nominal", "level0", "effort", "level1", "level2"), several.ok = TRUE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  work_dir <- normalizePath(work_dir)
  outputs_dir <- file.path(work_dir, "outputs")
  dir.create(outputs_dir, showWarnings = FALSE)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  for (f in data$outputs) file.copy(f, file.path(outputs_dir, basename(f)), overwrite = TRUE)

  # Folder of a step, with its data/ subfolder holding the files it reads
  # (outputs of the previous steps first, then the auxiliary files)
  step_dir <- function(name, sources = character(0)) {
    d <- file.path(work_dir, name)
    dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
    available <- c(list.files(outputs_dir, full.names = TRUE), unlist(data$files))
    for (s in sources) {
      from <- available[basename(available) == s][1]
      if (!is.na(from)) file.copy(from, file.path(d, "data", s), overwrite = TRUE)
    }
    setwd(d)
    d
  }
  # Harmonised and public versions written as the workflow does; the public
  # one is the output of the dataset
  write_outputs <- function(dataset, id) {
    out <- gta_dataset_outputs(dataset)
    readr::write_csv(out$harmonized, file.path("data", paste0(id, "_harmonized.csv")))
    readr::write_csv(out$public, file.path("data", paste0(id, "_public.csv")))
    file.copy(file.path("data", paste0(id, "_public.csv")), file.path(outputs_dir, paste0(id, ".csv")), overwrite = TRUE)
    out$public
  }
  result <- list()

  # Raw datasets: core function, then tidying and mapping ------------------------
  if ("rawdata" %in% steps) {
    result$raw <- list()
    for (id in names(data$raw)) {
      raw <- data$raw[[id]]
      step_dir(file.path("raw", id))
      files <- file.path("data", basename(raw$files))
      file.copy(raw$files, files, overwrite = TRUE)
      message("Raw dataset ", id)
      harmonized <- do.call(gta_rawdata_function(raw$dataset), as.list(files))
      harmonized_file <- file.path(getwd(), "data",
                                   paste0(tools::file_path_sans_ext(basename(raw$files[1])), "_harmonized.csv"))
      utils::write.csv(harmonized, harmonized_file, row.names = FALSE)
      dataset <- gta_tidy_raw_dataset(harmonized_file, nominal = grepl("nominal", harmonized_file),
                                      recap_each_step = recap_each_step, data_dir = "data")
      utils::write.csv(dataset, file.path(outputs_dir, paste0(id, ".csv")), row.names = FALSE)
      result$raw[[id]] <- dataset
    }
  }

  # Nominal catch ---------------------------------------------------------------
  if ("nominal" %in% steps) {
    o <- options$nominal
    step_dir(o$id, o$sources)
    opts <- o$options
    include <- lapply(c(IOTC = "include_IOTC", ICCAT = "include_ICCAT", WCPFC = "include_WCPFC",
                        CCSBT = "include_CCSBT", IATTC = "include_IATTC"),
                      function(k) if (is.null(opts[[k]])) TRUE else opts[[k]])
    files <- gta_nominal_catch_files(file.path("data", o$sources), include)
    nominal <- gta_nominal_catch(files,
                                 SBF_data_rfmo_to_keep = if (is.null(opts$SBF_data_rfmo_to_keep)) "CCSBT" else opts$SBF_data_rfmo_to_keep,
                                 recap_each_step = recap_each_step)
    result$nominal <- write_outputs(nominal, o$id)
  }

  # Georeferenced datasets: level 0 catch, effort, level 2 catch -------------------
  # (the sources are read from the second one, as the geoflow action does)
  georef <- function(o) {
    step_dir(o$id, o$sources)
    opts <- o$options
    opts$recap_each_step <- recap_each_step
    res <- gta_georef_dataset(o$sources[-1], opts, con = con, entity = NULL, data_dir = "data")
    write_outputs(res$georef_dataset, o$id)
  }
  if ("level0" %in% steps) result$level0 <- georef(options$level0)
  if ("effort" %in% steps) result$effort <- georef(options$effort)

  # Level 1 catch -----------------------------------------------------------------
  if ("level1" %in% steps) {
    o <- options$level1
    step_dir(o$id, o$sources)
    opts <- o$options
    `%||%` <- function(x, y) if (is.null(x)) y else x
    level1 <- gta_level1(
      file.path("data", opts$keylevel0 %||% "global_catch_firms_level0_harmonized.csv"),
      con = con,
      unit_conversion_convert = opts$unit_conversion_convert %||% TRUE,
      unit_conversion_iotc_file = opts$unit_conversion_iotc_file %||% "data/IOTC_conv_fact_mapped.csv",
      unit_conversion_ird_file = opts$unit_conversion_ird_file %||% "data/fact_conv_ird_complete_01_mai_2026.csv",
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
      keylevel0 = opts$keylevel0,
      recap_each_step = recap_each_step
    )
    result$level1 <- write_outputs(level1, o$id)
  }

  if ("level2" %in% steps) result$level2 <- georef(options$level2)

  attr(result, "work_dir") <- work_dir
  result
}
