#' Cache FDI Mapping Files Locally
#'
#' This function downloads the FDI mapping index and all mapping CSV files
#' referenced by this index into a local cache directory. It is intended to be
#' run once before offline execution, for example during a Docker image build.
#'
#' @param mapping_cache_dir Local directory where mapping files are cached.
#'        Defaults to the `FDI_MAPPINGS_CACHE_DIR` environment variable, or
#'        `"data/fdi-mappings-cache"` if the variable is not set.
#' @param fdi_mappings_ref Git reference used for the `fdiwg/fdi-mappings`
#'        repository. Can be `"main"`, a branch name, a tag, or a commit hash.
#' @param mapping_dataset URL of the global mapping index. If `NULL`, the URL
#'        is built from `fdi_mappings_ref`.
#'
#' @return Invisibly returns the mapping index data frame.
#'
#' @export

cache_fdi_mappings <- function(
    mapping_cache_dir = Sys.getenv("FDI_MAPPINGS_CACHE_DIR", "data/fdi-mappings-cache"),
    fdi_mappings_ref = "main",
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
  
  data_cache_dir <- here::here("data")
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
  
  codelist_files <- list(
    # Species labels
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv",
      local = here::here("data/cl_asfis_species.csv")
    ),
    
    # Measurement processing level
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement_processing_level.csv",
      local = here::here("data/cl_measurement_processing_level.csv")
    ),
    
    # Measurement
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement.csv",
      local = here::here("data/cl_measurement.csv")
    ),
    
    # Fishing mode
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_mode.csv",
      local = here::here("data/cl_fishing_mode.csv")
    ),
    
    # Measurement type labels
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_catch_concepts.csv",
      local = here::here("data/cl_catch_concepts.csv")
    ),
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement_types_effort.csv",
      local = here::here("data/cl_measurement_types_effort.csv")
    ),
    
    # Gear labels
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_isscfg_pilot_gear.csv",
      local = here::here("data/cl_isscfg_pilot_gear.csv")
    ),
    
    # Fishing fleet labels
    # Important: local filename expected by enrich_dataset_if_needed() is cl_fishingfleet_firms.csv
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_fleet.csv",
      local = here::here("data/cl_fishingfleet_firms.csv")
    ),
    
    # Measurement unit labels
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_effortunit_wcpfc.csv",
      local = here::here("data/cl_effortunit_wcpfc.csv")
    ),
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_effortunit_ccsbt.csv",
      local = here::here("data/cl_effortunit_ccsbt.csv")
    ),
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_effortunit_iattc.csv",
      local = here::here("data/cl_effortunit_iattc.csv")
    ),
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_effortunit_iccat.csv",
      local = here::here("data/cl_effortunit_iccat.csv")
    ),
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_effortunit_iotc.csv",
      local = here::here("data/cl_effortunit_iotc.csv")
    ),
    list(
      url = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_catchunit_rfmos.csv",
      local = here::here("data/cl_catchunit_rfmos.csv")
    )
  )
  
  for (x in codelist_files) {
    cache_raw_file(x$url, x$local)
  }
  
  # CWP grid
  cwp_grid_file <- here::here("data/cl_areal_grid.csv")
  if (!file.exists(cwp_grid_file)) {
    zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
    zip_path <- here::here("data/cwp_grid.zip")
    
    cache_raw_file(zip_url, zip_path)
    
    message("Unzipping CWP grid into data/")
    utils::unzip(zip_path, exdir = here::here("data"))
  }
  
  cache_raw_file(
    url = paste0(
      "https://raw.githubusercontent.com/fdiwg/fdi-mappings/",
      fdi_mappings_ref,
      "/cross-term/codelist_mapping_source_authority_species.csv"
    ),
    local = here::here(
      "data",
      "codelist_mapping_source_authority_species.csv"
    )
  )
  
  invisible(mapping_index)
}
