#' Map Code Lists Across Datasets Without Database
#'
#' This function maps dimensions such as gear type, species, fishing fleet,
#' fishing mode, and measurement type or unit from one dataset to another
#' using specified mapping datasets sourced from URLs or from a local cache.
#' It is designed to handle mappings based on source authority, with options
#' to retain original codes and generate summary mappings.
#'
#' @param fact A character string indicating the fact type to be mapped,
#'        either `"catch"` or `"effort"`.
#' @param mapping_dataset A dataframe or a path/URL to a CSV file containing the
#'        mapping instructions between different code lists, including
#'        `source_authority`, `dimensions_to_map`, and `db_mapping_dataset_name`.
#' @param dataset_to_map A dataframe containing the dataset for which the code
#'        lists need to be mapped.
#' @param mapping_keep_src_code Logical, indicating whether to keep the source
#'        codes in the mapped dataset.
#' @param summary_mapping Logical, indicating whether to generate a summary
#'        of the mappings performed.
#' @param source_authority_to_map A character vector specifying the source
#'        authorities to be mapped.
#' @param mapping_cache_dir Local directory where mapping files are cached.
#'        Defaults to the `FDI_MAPPINGS_CACHE_DIR` environment variable, or
#'        `"data/fdi-mappings-cache"` if the variable is not set.
#' @param allow_download Logical. If `TRUE`, missing mapping files are downloaded
#'        from GitHub and saved in `mapping_cache_dir`. If `FALSE`, only local
#'        cached files are used.
#' @param fdi_mappings_ref Git reference used for the `fdiwg/fdi-mappings`
#'        repository. Can be `"main"`, a branch name, a tag, or a commit hash.
#'
#' @return A list containing the mapped dataset, a recap of the mappings,
#'         total statistics of the mappings, and details of the codes that
#'         were not mapped.
#'
#' @export

map_codelists_no_DB <- function(
    fact,
    mapping_dataset = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv",
    dataset_to_map,
    mapping_keep_src_code = FALSE,
    summary_mapping = FALSE,
    source_authority_to_map = c("IATTC", "CCSBT", "WCPFC"),
    mapping_cache_dir = Sys.getenv("FDI_MAPPINGS_CACHE_DIR", "data/fdi-mappings-cache"),
    allow_download = TRUE,
    fdi_mappings_ref = "main"
) {
  
  if (!is.data.frame(mapping_dataset)) {
    
    if (grepl("^https?://", mapping_dataset)) {
      
      mapping_dataset_url <- sub(
        "fdiwg/fdi-mappings/main/",
        paste0("fdiwg/fdi-mappings/", fdi_mappings_ref, "/"),
        mapping_dataset,
        fixed = TRUE
      )
      
      mapping_dataset_local <- file.path(
        mapping_cache_dir,
        "global",
        "firms",
        "gta",
        basename(mapping_dataset)
      )
      source(here::here("R/tunaatlas_scripts/pre-harmonization/get_cached_file.R"))
      mapping_dataset_local <- get_cached_file(
        url = mapping_dataset_url,
        local_path = mapping_dataset_local,
        allow_download = allow_download
      )
      
      mapping_dataset <- read.csv(
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
      
    } else {
      
      mapping_dataset <- read.csv(
        mapping_dataset,
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
    }
  }
  
  base_url <- paste0(
    "https://raw.githubusercontent.com/fdiwg/fdi-mappings/",
    fdi_mappings_ref,
    "/regional-to-global/"
  )
  
  if (fact == "catch") {
    dimension_to_map <- c("gear_type", "species", "fishing_fleet", "fishing_mode")
  } else if (fact == "effort") {
    dimension_to_map <- c("gear_type", "fishing_fleet", "fishing_mode", "measurement_unit")
  }
  
  source_authority_mapped <- unique(dataset_to_map$source_authority)
  dimension_to_map <- dimension_to_map[dimension_to_map %in% colnames(dataset_to_map)]
  
  data_not_to_map <- dataset_to_map[
    !dataset_to_map$source_authority %in% source_authority_to_map,
  ]
  
  dataset_to_map <- dataset_to_map[
    dataset_to_map$source_authority %in% source_authority_to_map,
  ]
  
  if (nrow(dataset_to_map) == 0) {
    return(list(
      dataset_mapped = data_not_to_map,
      recap_mapping = NULL,
      stats_total = NULL,
      not_mapped_total = NULL
    ))
  }
  
  recap_mapping <- NULL
  not_mapped_total <- NULL
  stats_total <- NULL
  
  for (dimension in dimension_to_map) {
    
    mapping_dataset_this_dimension <- mapping_dataset %>%
      dplyr::filter(
        dimensions_to_map == dimension,
        source_authority %in% source_authority_to_map
      )
    
    df_mapping_final_this_dimension <- NULL
    
    for (j in seq_len(nrow(mapping_dataset_this_dimension))) {
      
      file_name <- paste0(
        mapping_dataset_this_dimension$db_mapping_dataset_name[j],
        ".csv"
      )
      
      mapping_url <- paste0(
        base_url,
        mapping_dataset_this_dimension$source_authority[j],
        "/",
        file_name
      )
      
      mapping_local_path <- file.path(
        mapping_cache_dir,
        "regional-to-global",
        mapping_dataset_this_dimension$source_authority[j],
        file_name
      )
      
      mapping_local_path <- tryCatch({
        get_cached_file(
          url = mapping_url,
          local_path = mapping_local_path,
          allow_download = allow_download
        )
      }, error = function(e) {
        message(paste("Failed to download or find the mapping file for", mapping_url))
        message(conditionMessage(e))
        return(NULL)
      })
      
      if (is.null(mapping_local_path)) {
        next
      }
      
      df_mapping <- tryCatch({
        readr::read_csv(
          mapping_local_path,
          col_types = readr::cols(.default = readr::col_character()),
          show_col_types = FALSE
        )
      }, error = function(e) {
        message(paste("Failed to read the mapping file for", mapping_local_path))
        return(NULL)
      })
      
      if (!is.null(df_mapping)) {
        if (!"source_authority" %in% colnames(df_mapping)) {
          df_mapping$source_authority <- mapping_dataset_this_dimension$source_authority[j]
        }
        
        df_mapping_final_this_dimension <- rbind(
          df_mapping_final_this_dimension,
          df_mapping
        )
      }
    }
    
    if (!is.null(df_mapping_final_this_dimension) &&
        nrow(df_mapping_final_this_dimension) > 0) {
      
      successful_mappings <- df_mapping_final_this_dimension %>%
        dplyr::filter(src_code %in% dataset_to_map[[dimension]])
      
      if (summary_mapping) {
        recap_mapping <- rbind(successful_mappings, recap_mapping)
      }
      source(here::here("R/sardara_functions/map_codelist.R"))
      mapping <- map_codelist(
        dataset_to_map,
        df_mapping_final_this_dimension,
        dimension,
        mapping_keep_src_code
      )
      
      dataset_to_map <- mapping$df
      
      if (summary_mapping) {
        stats <- mapping$stats
        not_mapped <- mapping$not_mapped
        not_mapped_total <- rbind(not_mapped_total, not_mapped)
        stats_total <- rbind(stats_total, stats)
      }
    }
  }
  
  if (!is.null(recap_mapping)) {
    recap_mapping <- recap_mapping %>%
      dplyr::filter(source_authority %in% source_authority_mapped)
  }
  
  dataset_mapped <- rbind(dataset_to_map, data_not_to_map)
  
  dataset_mapped <- list(
    dataset_mapped = dataset_mapped,
    recap_mapping = recap_mapping,
    stats_total = stats_total,
    not_mapped_total = not_mapped_total
  )
  
  return(dataset_mapped)
}