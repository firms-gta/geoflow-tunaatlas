cache_fdi_mappings <- function(
    mapping_cache_dir = Sys.getenv("FDI_MAPPINGS_CACHE_DIR", "data/fdi-mappings"),
    codelist_cache_dir = Sys.getenv("FDI_CODELISTS_CACHE_DIR", "data/fdi-codelists"),
    fdi_mappings_ref = "c74ff137ebd28b0367172a8a73821a0d6dada65f",
    fdi_codelists_ref = "f469d9767110c4ea947dbd356a3e4b79b9108d92",
    mapping_dataset = NULL
) {
  source(here::here("R/tunaatlas_scripts/pre-harmonization/get_cached_file.R"))
  
  if (is.null(mapping_dataset)) {
    mapping_dataset <- paste0(
      "https://raw.githubusercontent.com/fdiwg/fdi-mappings/",
      fdi_mappings_ref,
      "/global/firms/gta/codelist_mapping_rfmos_to_global.csv"
    )
  }
  
  mapping_dataset_local <- file.path(
    mapping_cache_dir,
    "global",
    "firms",
    "gta",
    "codelist_mapping_rfmos_to_global.csv"
  )
  
  mapping_dataset_local <- get_cached_file(
    url = mapping_dataset,
    local_path = mapping_dataset_local,
    allow_download = TRUE
  )
  require(dplyr)
  mapping_index <- utils::read.csv(
    mapping_dataset_local,
    stringsAsFactors = FALSE,
    colClasses = "character"
  ) %>%
    dplyr::mutate(
      dimensions_to_map = ifelse(
        dimensions_to_map == "fishingfleet",
        "fishing_fleet",
        dimensions_to_map
      )
    )
  
  base_url <- paste0(
    "https://raw.githubusercontent.com/fdiwg/fdi-mappings/",
    fdi_mappings_ref,
    "/regional-to-global/"
  )
  
  for (i in seq_len(nrow(mapping_index))) {
    
    file_name <- paste0(mapping_index$db_mapping_dataset_name[i], ".csv")
    
    mapping_url <- paste0(
      base_url,
      mapping_index$source_authority[i],
      "/",
      file_name
    )
    
    mapping_local_path <- file.path(
      mapping_cache_dir,
      "regional-to-global",
      mapping_index$source_authority[i],
      file_name
    )
    
    get_cached_file(
      url = mapping_url,
      local_path = mapping_local_path,
      allow_download = TRUE
    )
  }
  
  # -------------------------------------------------------------------------
  # Cache additional FDI / CWP / GTA codelists required by enrich_dataset_if_needed()
  # -------------------------------------------------------------------------
  
  data_cache_dir <- here::here(codelist_cache_dir)
  dir.create(data_cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  cache_raw_file <- function(url, local_path, mode = "wb") {
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    
    if (!file.exists(local_path)) {
      message("Caching file: ", url)
      utils::download.file(url, local_path, mode = mode)
    } else {
      message("Already cached: ", local_path)
    }
    
    invisible(local_path)
  }
  
  codelists_base_url <- paste0(
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/",
    fdi_codelists_ref, "/"
  )
  
  codelist_files <- list(
    list(url = paste0(codelists_base_url, "global/cl_asfis_species.csv"),
         local = here::here(codelist_cache_dir, "cl_asfis_species.csv")),
    
    list(url = paste0(codelists_base_url, "global/fdi/cl_measurement_processing_level.csv"),
         local = here::here(codelist_cache_dir, "cl_measurement_processing_level.csv")),
    
    list(url = paste0(codelists_base_url, "global/fdi/cl_measurement.csv"),
         local = here::here(codelist_cache_dir, "cl_measurement.csv")),
    
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_fishing_mode.csv"),
         local = here::here(codelist_cache_dir, "cl_fishing_mode.csv")),
    
    list(url = paste0(codelists_base_url, "global/cwp/cl_catch_concepts.csv"),
         local = here::here(codelist_cache_dir, "cl_catch_concepts.csv")),
    
    list(url = paste0(codelists_base_url, "global/fdi/cl_measurement_types_effort.csv"),
         local = here::here(codelist_cache_dir, "cl_measurement_types_effort.csv")),
    
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_isscfg_pilot_gear.csv"),
         local = here::here(codelist_cache_dir, "cl_isscfg_pilot_gear.csv")),
    
    # Important: local filename expected by enrich_dataset_if_needed() is cl_fishingfleet_firms.csv
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_fishing_fleet.csv"),
         local = here::here(codelist_cache_dir, "cl_fishingfleet_firms.csv")),
    
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_effortunit_wcpfc.csv"),
         local = here::here(codelist_cache_dir, "cl_effortunit_wcpfc.csv")),
    
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_effortunit_ccsbt.csv"),
         local = here::here(codelist_cache_dir, "cl_effortunit_ccsbt.csv")),
    
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_effortunit_iattc.csv"),
         local = here::here(codelist_cache_dir, "cl_effortunit_iattc.csv")),
    
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_effortunit_iccat.csv"),
         local = here::here(codelist_cache_dir, "cl_effortunit_iccat.csv")),
    
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_effortunit_iotc.csv"),
         local = here::here(codelist_cache_dir, "cl_effortunit_iotc.csv")),
    
    list(url = paste0(codelists_base_url, "global/firms/gta/cl_catchunit_rfmos.csv"),
         local = here::here(codelist_cache_dir, "cl_catchunit_rfmos.csv"))
  )
  
  for (x in codelist_files) {
    cache_raw_file(x$url, x$local)
  }
  
  # CWP grid
  cwp_grid_file <- here::here(codelist_cache_dir, "cl_areal_grid.csv")
  if (!file.exists(cwp_grid_file)) {
    zip_url <- paste0(codelists_base_url, "global/cwp/cl_areal_grid.zip")
    zip_path <- here::here(codelist_cache_dir, "cwp_grid.zip")
    
    cache_raw_file(zip_url, zip_path)
    
    message("Unzipping CWP grid into ", codelist_cache_dir)
    utils::unzip(zip_path, exdir = here::here(codelist_cache_dir))
  }
  
  cache_raw_file(
    url = paste0(
      "https://raw.githubusercontent.com/fdiwg/fdi-mappings/",
      fdi_mappings_ref,
      "/cross-term/codelist_mapping_source_authority_species.csv"
    ),
    local = here::here(codelist_cache_dir, "codelist_mapping_source_authority_species.csv")
  )
  
  invisible(mapping_index)
}