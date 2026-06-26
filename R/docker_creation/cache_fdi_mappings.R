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
  
  invisible(mapping_index)
}
