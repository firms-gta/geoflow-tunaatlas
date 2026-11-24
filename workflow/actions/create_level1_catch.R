#' Create Global Tuna Atlas Level 1 Dataset
#'
#' geoflow action of the level 1 catch entity: reads the action options,
#' loads the level 0 dataset (file in data/, or Zenodo), runs the core function
#' gta_level1() (R/level1.R: unit conversion and optional
#' filtering/aggregation), then writes the outputs and declares the resources
#' (process_and_aggregate_dataset()).
#'
#' @param action Geoflow action object containing options.
#' @param entity Geoflow entity object.
#' @param config Geoflow configuration object.
#'
#' @export
create_level1_catch <- function(action, entity, config) {
  
  # Initialisation ----------------------------------------------------------
  opts <- action$options
  con <- config$software$output$dbi
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  options(encoding = "UTF-8")
  CWP.dataset::write_options_to_csv(opts)
  
  stepnumber <- 1
  
  stepLogger <- function(level, step, msg) {
    log_info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  
  url_scripts_generation <- here::here("R/tunaatlas_scripts/generation")
  
  source(file.path(url_scripts_generation, "process_and_aggregate_dataset.R"))
  
  
  # Options ----------------------------------------------------------------
  
  opts$fact <- opts$fact %||% "catch"
  
  opts$doilevel0 <- opts$doilevel0 %||% "10.5281/zenodo.11460074"
  opts$keylevel0 <- opts$keylevel0 %||% "global_catch_firms_level0_harmonized.csv"
  
  opts$forceuseofdoi <- opts$forceuseofdoi %||% FALSE
  opts$recap_each_step <- opts$recap_each_step %||% TRUE
  
  opts$unit_conversion_convert <- opts$unit_conversion_convert %||% TRUE
  
  opts$unit_conversion_csv_conversion_factor_url_iotc <-
    opts$unit_conversion_csv_conversion_factor_url_iotc %||% NULL
  
  opts$unit_conversion_csv_conversion_factor_url_other <-
    opts$unit_conversion_csv_conversion_factor_url_other %||% NULL
  
  opts$unit_conversion_codelist_geoidentifiers_conversion_factors <-
    opts$unit_conversion_codelist_geoidentifiers_conversion_factors %||%
    "areas_conversion_factors_numtoweight_ird"
  
  opts$unit_conversion_codelist_geoidentifiers_conversion_factors = if(!is.null(opts$unit_conversion_codelist_geoidentifiers_conversion_factors)) opts$unit_conversion_codelist_geoidentifiers_conversion_factors else "areas_conversion_factors_numtoweight_ird"
  
  opts$mapping_map_code_lists <- opts$mapping_map_code_lists %||% TRUE
  opts$mapping_keep_src_code <- opts$mapping_keep_src_code %||% FALSE
  
  opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg <-
    opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg %||% FALSE
  
  opts$resolution_filter <- opts$resolution_filter %||% NULL
  opts$filtering <- opts$filtering %||% NULL
  opts$gear_filter <- opts$gear_filter %||% NULL
  
  # Load Level 0 ------------------------------------------------------------
  
  stepLogger(1, stepnumber, "Load FIRMS Level 0 georeferenced dataset")
  stepnumber <- stepnumber + 1
  
  if (file.exists(file.path("data", opts$keylevel0)) && !opts$forceuseofdoi) {
    
    georef_dataset <- readr::read_csv(
      file.path("data", opts$keylevel0),
      guess_max = 0
    )
    
  } else if (!is.null(opts$doilevel0)) {
    
    rec <- sub("^.*zenodo\\.(\\d+).*$", "\\1", opts$doilevel0)
    
    url <- sprintf(
      "https://zenodo.org/records/%s/files/%s?download=1",
      rec,
      opts$keylevel0
    )
    
    dir.create("data", showWarnings = FALSE)
    download.file(url, file.path("data", opts$keylevel0), mode = "wb")
    
    georef_dataset <- readr::read_csv(
      file.path("data", opts$keylevel0),
      guess_max = 0
    )
    
  } else {
    stop("Please provide a Level 0 georeferenced dataset.")
  }
  
  # Level 1 processing: core function gta_level1() (R/level1.R) ----------
  georef_dataset <- gta_level1(
    georef_dataset,
    con = con,
    unit_conversion_convert = opts$unit_conversion_convert,
    unit_conversion_iotc_file = opts$unit_conversion_iotc_file %||% "data/IOTC_conv_fact_mapped.csv",
    unit_conversion_ird_file = opts$unit_conversion_ird_file %||% "data/fact_conv_ird_complete_01_mai_2026.csv",
    unit_conversion_codelist_geoidentifiers_conversion_factors =
      opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
    mapping_map_code_lists = opts$mapping_map_code_lists,
    fact = opts$fact,
    aggregate_on_5deg_data_with_resolution_inferior_to_5deg =
      opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg,
    resolution_filter = opts$resolution_filter,
    filtering = opts$filtering,
    gear_filter = opts$gear_filter,
    doilevel0 = opts$doilevel0,
    keylevel0 = opts$keylevel0,
    recap_each_step = opts$recap_each_step,
    entity = entity
  )
  stepnumber <- 2 + isTRUE(opts$unit_conversion_convert) +
    isTRUE(opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg) + !is.null(opts$resolution_filter)

  # Final aggregation/export ------------------------------------------------
  
  stepLogger(1, stepnumber, "Process and aggregate Level 1 dataset")
  stepnumber <- stepnumber + 1
  
  process_and_aggregate_dataset(
    georef_dataset,
    entity,
    config,
    opts,
    columns_to_keep = c(
      "source_authority",
      "species",
      "gear_type",
      "fishing_fleet",
      "fishing_mode",
      "time_start",
      "time_end",
      "year",
      "month",
      "quarter",
      "geographic_identifier",
      "measurement_unit",
      "measurement_value",
      "measurement_type",
      "measurement_processing_level",
      "measurement"
    )
  )
  
  log_info("End: Level 1 Tuna Atlas dataset has been created.")
  
  rm(georef_dataset)
  gc()
  
  invisible(TRUE)
}
