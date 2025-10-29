#' Enrich a CWP dataset with missing labels and geometries
#'
#' This function enriches a dataset by filling missing descriptive labels
#' (gear type, fishing fleet, measurement units, etc.) using database queries,
#' local fallback files, or online downloads if necessary.
#'
#' @param data A `data.frame` or `sf` object to enrich. It is better if the dataset follows CWP standards
#' @param connectionDB Optional. A database connection object.
#' @param save_prefix Optional. A prefix for saving output files (.qs and .csv).
#'
#' @return A list containing:
#' - `with_geom`: Enriched dataset with geometry (sf object)
#' - `without_geom`: Enriched dataset without geometry (data.frame)
#'
#' @export

enrich_dataset_if_needed <- function(data, connectionDB = NULL, save_prefix = NULL) {
  library(dplyr)
  library(sf)
  library(readr)
  library(stringr)
  library(janitor)
  library(here)
  library(qs)
  library(data.table)
  
  # Fallback functions
  try_db_query <- function(con, query, fallback_file, fallback_read_fun, download_url = NULL) {
    if (!is.null(con) && DBI::dbIsValid(con)) {
      res <- try(DBI::dbGetQuery(con, "SELECT 1"), silent = TRUE)
      if (!inherits(res, "try-error")) {
        message("Database connection successful, retrieving via SQL: ", query)
        return(DBI::dbGetQuery(con, query))
      }
    }
    message("No valid database connection.")
    if (!file.exists(fallback_file)) {
      if (!is.null(download_url)) {
        message("Downloading from ", download_url)
        utils::download.file(download_url, fallback_file, mode = "wb")
      } else {
        stop("Required file missing: ", fallback_file)
      }
    }
    message("Loading from local file: ", fallback_file)
    return(fallback_read_fun(fallback_file))
  }
  
  try_st_read <- function(con, query, fallback_file, download_url = NULL) {
    if (!is.null(con) && DBI::dbIsValid(con)) {
      res <- try(DBI::dbGetQuery(con, "SELECT 1"), silent = TRUE)
      if (!inherits(res, "try-error")) {
        message("Database connection successful, reading via st_read: ", query)
        return(sf::st_read(con, query = query))
      }
    }
    if (!file.exists(fallback_file)) {
      if (!is.null(download_url)) {
        message("Downloading from ", download_url)
        utils::download.file(download_url, fallback_file, mode = "wb")
        if (grepl("\\.zip$", fallback_file)) {
          utils::unzip(fallback_file, exdir = dirname(fallback_file))
        }
      } else {
        stop("Required file missing: ", fallback_file)
      }
    }
    message("Reading local shapefile: ", fallback_file)
    return(sf::st_read(fallback_file))
  }
  
  data$Gear <- NULL
  data$GRIDTYPE <- NULL
  
  data <- data %>% dplyr::mutate(measurement_unit = case_when(measurement_unit == "Tons" ~ "t", 
                                                              measurement_unit == "Number of fish" ~ "no",
                                                              TRUE ~ measurement_unit))
  
  if("geom_wkt" %in% colnames(data)){
    data <- data %>% dplyr::rename(geom = geom_wkt)
  }
  
  # Retrieve required data
  con <- tryCatch(connectionDB, error = function(e) NULL)
  
  # Species labels
  species_group <- try_db_query(
    con,
    "SELECT taxa_order, code FROM species.species_asfis",
    here::here("data/cl_asfis_species.csv"),
    function(f) readr::read_csv(f) %>% janitor::clean_names() %>% dplyr::select(species_group = taxa_order, species = code),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv"
  )
  
  # Other required codelists
  cl_measurement_processing_level <- try_db_query(
    con,
    "SELECT * FROM measurement_processing_level.measurement_processing_level",
    here::here("data/cl_measurement_processing_level.csv"),
    function(f) readr::read_csv(f) %>% janitor::clean_names() %>% dplyr::select(code, label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement_processing_level.csv"
  )
  
  cl_measurement <- try_db_query(
    con,
    "SELECT * FROM measurement.measurement",
    here::here("data/cl_measurement.csv"),
    function(f) readr::read_csv(f) %>% janitor::clean_names() %>% dplyr::select(code, label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement.csv"
  )
  
  cl_fishing_mode <- try_db_query(
    con,
    "SELECT * FROM fishing_mode.fishing_mode",
    here::here("data/cl_fishing_mode.csv"),
    function(f) readr::read_csv(f) %>% janitor::clean_names() %>% dplyr::select(code, label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_mode.csv"
  )
  
  cl_measurement_type <- {
    
    catch_file <- here::here("data/cl_catch_concepts.csv")
    effort_file <- here::here("data/cl_measurement_types_effort.csv")
    if (!file.exists(catch_file)) {
      utils::download.file("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_catch_concepts.csv", catch_file, mode = "wb")
    }
    # to be fixed here 
    if (!file.exists(effort_file)) {
      utils::download.file("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement_types_effort.csv", effort_file, mode = "wb")
    }
    dplyr::bind_rows(
      readr::read_csv(catch_file) %>% janitor::clean_names() %>% dplyr::select(code, label),
      readr::read_csv(effort_file) %>% janitor::clean_names() %>% dplyr::select(code, label)
    )
  }
  
  # Gear labels
  cl_cwp_gear_level2 <- try_db_query(
    con,
    "SELECT * FROM gear_type.isscfg_revision_1",
    here::here("data/cl_isscfg_pilot_gear.csv"),
    function(f) readr::read_csv(f) %>% dplyr::select(Code = code, Gear = label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_isscfg_pilot_gear.csv"
  )
  
  # Fishing fleet labels
  fishing_fleet_label <- try_db_query(
    con,
    "SELECT * FROM fishing_fleet.fishingfleet_firms",
    here::here("data/cl_fishingfleet_firms.csv"),
    function(f) readr::read_csv(f) %>% janitor::clean_names() %>% dplyr::select(code, label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_fleet.csv"
  )
  
  # Measurement unit labels
  measurement_unit_label <- {
    files <- c("cl_effortunit_wcpfc.csv", "cl_effortunit_ccsbt.csv", "cl_effortunit_iattc.csv", "cl_effortunit_iccat.csv", "cl_effortunit_iotc.csv", "cl_catchunit_rfmos.csv")
               
    base_url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/"
    files %>%
      lapply(function(file) {
        readr::read_csv(paste0(base_url, file), show_col_types = FALSE) %>%
          dplyr::select(code, label) %>%
          dplyr::mutate(source_authority = dplyr::case_when(
            stringr::str_detect(file, "ccsbt") ~ "CCSBT",
            stringr::str_detect(file, "iattc") ~ "IATTC",
            stringr::str_detect(file, "iccat") ~ "ICCAT",
            stringr::str_detect(file, "iotc") ~ "IOTC",
            stringr::str_detect(file, "wcpfc") ~ "WCPFC",
            TRUE ~ "ALL"
          ))
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(label = dplyr::case_when(
        label == "UNK" ~ "Unknown",
        label == "Number of Sets" ~ "Number of sets",
        TRUE ~ label
      ))
  }
  
  # CWP grid (geometry)
  cwp_grid_file <- here::here("data/cl_areal_grid.csv")
  if (!file.exists(cwp_grid_file)) {
    zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
    zip_path <- here::here("data/cwp_grid.zip")
    message("Downloading CWP Grid...")
    utils::download.file(zip_url, zip_path, mode = "wb")
    utils::unzip(zip_path, exdir = here::here("data"))
  }
  shapefile.fix <- sf::st_read(cwp_grid_file) %>%
    sf::st_as_sf(wkt = "geom_wkt", crs = 4326) %>%
    dplyr::rename(cwp_code = CWP_CODE, geom = geom_wkt)
  
  # Start enrichment
  enriched_data <- data
  # Add missing columns if needed
  if (!"gear_type_label" %in% names(enriched_data)) {
    enriched_data <- enriched_data %>%
      dplyr::left_join(cl_cwp_gear_level2, by = c("gear_type" = "Code")) %>%
      dplyr::rename(gear_type_label = Gear)
  }
  
  if (!"fishing_fleet_label" %in% names(enriched_data)) {
    enriched_data <- enriched_data %>%
      dplyr::left_join(fishing_fleet_label %>% dplyr::select(code, fishing_fleet_label = label), by = c("fishing_fleet" = "code"))
  }
  
  if (!"measurement_unit_label" %in% names(enriched_data)) {
    
    # Séparer les labels spécifiques et globaux
    measurement_unit_label_global <- measurement_unit_label %>% 
      dplyr::filter(source_authority == "ALL")
    
    measurement_unit_label_specific <- measurement_unit_label %>% 
      dplyr::filter(source_authority != "ALL")
    
    # 1. Jointure avec les labels spécifiques (effort avec source_authority)
    enriched_data <- enriched_data %>%
      dplyr::left_join(
        measurement_unit_label_specific %>%
          dplyr::select(code, source_authority, measurement_unit_label_specific = label),
        by = c("measurement_unit" = "code", "source_authority")
      )
    
    # 2. Jointure avec les labels globaux (catch)
    enriched_data <- enriched_data %>%
      dplyr::left_join(
        measurement_unit_label_global %>%
          dplyr::select(code, measurement_unit_label_global = label),
        by = c("measurement_unit" = "code")
      )
    
    # 3. Fusion des deux labels dans une seule colonne
    enriched_data <- enriched_data %>%
      dplyr::mutate(
        measurement_unit_label = dplyr::coalesce(measurement_unit_label_specific, measurement_unit_label_global)
      ) %>%
      dplyr::select(-measurement_unit_label_specific, -measurement_unit_label_global)
  }
  

  # Enrich measurement_type_label if missing
  
  if (!"measurement_type_label" %in% names(enriched_data)) {
    enriched_data <- enriched_data %>%
      dplyr::left_join(cl_measurement_type %>% dplyr::select(code, measurement_type_label = label), 
                       by = c("measurement_type" = "code"))
  }
  
  # Enrich measurement_label if missing
  if (!"measurement_label" %in% names(enriched_data)) {
    enriched_data <- enriched_data %>%
      dplyr::left_join(cl_measurement %>% dplyr::select(code, measurement_label = label), 
                       by = c("measurement" = "code"))
  }

  # Enrich measurement_processing_level_label if missing
  if (!"measurement_processing_level_label" %in% names(enriched_data)) {
    enriched_data <- enriched_data %>%
      dplyr::left_join(cl_measurement_processing_level%>% dplyr::select(code, measurement_processing_level_label = label) , 
                       by = c("measurement_processing_level" = "code"))
  }
  
  # Enrich fishing_mode_label if missing
  if (!"fishing_mode_label" %in% names(enriched_data)) {
    enriched_data <- enriched_data %>%
      dplyr::left_join(cl_fishing_mode %>% dplyr::select(code, fishing_mode_label = label), 
                       by = c("fishing_mode" = "code"))
  }
  
  reorder_with_labels <- function(df) {
    original_cols <- names(df)
    new_order <- character()
    
    for (col in original_cols) {
      # On saute les colonnes *_label ici, elles seront ajoutées à côté de leur base
      if (grepl("_label$", col)) next
      new_order <- c(new_order, col)
      label_col <- paste0(col, "_label")
      if (label_col %in% original_cols) {
        new_order <- c(new_order, label_col)
      }
    }
    
    # Ajouter les colonnes *_label orphelines non associées (s'il y en a)
    orphan_labels <- setdiff(
      grep("_label$", original_cols, value = TRUE),
      new_order
    )
    new_order <- c(new_order, orphan_labels)
    
    # Réorganiser le data.frame
    df[, new_order, drop = FALSE]
  }
  
  
  enriched_data <- reorder_with_labels(enriched_data)
  
  
  if (!"gridtype" %in% names(enriched_data)) {
    enriched_data <- enriched_data %>%
      dplyr::left_join(shapefile.fix %>% dplyr::select(geographic_identifier = cwp_code, gridtype = GRIDTYPE), by = "geographic_identifier")
  }
  
  enriched_data <- enriched_data%>%
    dplyr::select(measurement_value, species, species_label, fishing_fleet, fishing_fleet_label, time_start, time_end, everything())
  enriched_data <- sf::st_as_sf(enriched_data)
  sf::st_crs(enriched_data) <- 4326
  
  # Save if requested
  if (!is.null(save_prefix)) {
    qs::qsave(enriched_data, paste0(save_prefix, "_with_geom.qs"))
    enriched_data_no_geom <- enriched_data %>% dplyr::select(-any_of("geom"))
    fwrite(enriched_data_no_geom, paste0(save_prefix, "_without_geom.csv"))
  }
  without_geom <- enriched_data 
  without_geom$geom <- NULL 
  
  return(list(
    with_geom = enriched_data,
    without_geom = without_geom
  ))
}
