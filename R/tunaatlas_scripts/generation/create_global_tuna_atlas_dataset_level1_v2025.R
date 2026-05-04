#' Create Global Tuna Atlas Level 1 Dataset
#'
#' Create a Level 1 catch dataset from a Level 0 Tuna Atlas dataset by applying
#' unit conversion and optional filtering/aggregation steps.
#'
#' @param action Geoflow action object containing options.
#' @param entity Geoflow entity object.
#' @param config Geoflow configuration object.
#'
#' @export
create_global_tuna_atlas_level1_v2025 <- function(action, entity, config) {
  
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
    config$logger.info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  
  url_scripts_generation <- here::here("R/tunaatlas_scripts/generation")
  
  source(file.path(url_scripts_generation, "download_zenodo_csv.R"))
  source(file.path(url_scripts_generation, "double_unit_data_handling.R"))
  source(file.path(url_scripts_generation, "do_unit_conversion.R"))
  source(file.path(url_scripts_generation, "perform_unit_conversion.R"))
  source(file.path(url_scripts_generation, "dimension_filtering_function.R"))
  source(file.path(url_scripts_generation, "process_and_aggregate_dataset.R"))
  
  source(here::here("R/sardara_functions/transform_cwp_code_from_1deg_to_5deg.R"))
  
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
  
  georef_dataset <- georef_dataset %>%
    dplyr::mutate(
      measurement_value = as.numeric(measurement_value),
      time_start = as.character(time_start),
      time_end = as.character(time_end)
    ) %>% 
    dplyr::filter(species %in% c("YFT", "ALB", "SKJ", "BET", "SWO", "SBF"))
  
  if (isTRUE(opts$recap_each_step)) {
    CWP.dataset::function_recap_each_step(
      "Level0_input",
      georef_dataset,
      paste0(
        "Level 0 georeferenced dataset loaded from DOI/file. DOI: ",
        opts$doilevel0,
        ". Key: ",
        opts$keylevel0,
        "."
      ),
      "read_csv/download.file",
      list(opts$doilevel0, opts$keylevel0),
      entity
    )
  }
  
  # Remove duplicated number/tons strata ----------------------------------- plus utile car level 0 na deja plus ca
  
  # if (opts$fact == "catch") {
  #   
  #   stepLogger(1, stepnumber, "Remove duplicated catch strata reported both in tons and numbers")
  #   stepnumber <- stepnumber + 1
  #   
  #   georef_dataset <- georef_dataset %>%
  #     dplyr::group_by(
  #       dplyr::across(
  #         setdiff(colnames(.), c("measurement_value", "measurement_unit"))
  #       )
  #     ) %>%
  #     dplyr::mutate(nb_units = dplyr::n_distinct(measurement_unit)) %>%
  #     dplyr::filter(!(measurement_unit == "no" & nb_units >= 2)) %>%
  #     dplyr::select(-nb_units) %>%
  #     dplyr::ungroup()
  #   
  #   if (isTRUE(opts$recap_each_step)) {
  #     CWP.dataset::function_recap_each_step(
  #       "Removing_duplicated_units",
  #       georef_dataset,
  #       "When the same catch stratum is reported both in tons and number of fish, the number-based duplicated record is removed.",
  #       "dplyr::filter",
  #       list(),
  #       entity
  #     )
  #   }
  # }
  
  # UNIT CONVERSION ---------------------------------------------------------
  
  if (isTRUE(opts$unit_conversion_convert)) {
    stepLogger(1, stepnumber, "Harmonising catch units")
    stepnumber <- stepnumber + 1
    
    original_cols <- colnames(georef_dataset)
    
    # IOTC specific conversion ----------------------------------------------
    config$logger.info("Begin IOTC specific unit conversion")
    IOTC_conv_fact_mapped <- readr::read_csv(
      opts$unit_conversion_iotc_file %||% "data/IOTC_conv_fact_mapped.csv",
      guess_max = 0
    ) %>%
      dplyr::mutate(
        gear_type = as.character(gear_type),
        time_start = as.character(time_start),
        time_end = as.character(time_end),
        geographic_identifier = as.character(geographic_identifier),
        year = lubridate::year(time_start)
      )%>% 
      dplyr::mutate(gear_type = as.character(gear_type), time_start = as.character(time_start)) %>% 
      dplyr::mutate(year = lubridate::year(time_start)) %>% dplyr::mutate(measurement_unit = case_when(measurement_unit == "NO"~ "no", 
                                                                                                       measurement_unit == "MT"~ "t",
                                                                                                       TRUE ~ measurement_unit))%>%
      dplyr::mutate(unit_target = case_when(unit_target == "NO"~ "no", 
                                            unit_target == "MT"~ "t",
                                            TRUE ~ unit_target))
    
    iotc <- georef_dataset %>%
      dplyr::filter(source_authority == "IOTC") %>%
      dplyr::group_by(
        time_start,
        time_end,
        species,
        fishing_fleet,
        gear_type,
        source_authority,
        fishing_mode,
        geographic_identifier
      ) %>%
      dplyr::mutate(numberunit = dplyr::n_distinct(measurement_unit)) %>%
      dplyr::ungroup()
    
    iotc_only_number <- iotc %>%
      dplyr::filter(measurement_unit == "no", numberunit == 1) %>%
      dplyr::select(dplyr::all_of(original_cols))
    
    iotc_tons_or_mixed <- iotc %>%
      dplyr::filter(!(measurement_unit == "no" & numberunit == 1)) %>%
      dplyr::filter(measurement_unit != "no") %>%
      dplyr::select(dplyr::all_of(original_cols))
    iotc_only_number_converted <- iotc_only_number %>%
      dplyr::inner_join(
        IOTC_conv_fact_mapped %>%dplyr::select(-conversion_factor) %>% 
          dplyr::rename(conversion_factor = measurement_value) %>% 
          dplyr::select(-measurement_unit)  %>% 
          dplyr::mutate(year = as.character(year)) %>% dplyr::mutate(conversion_factor = as.numeric(conversion_factor)) ,
        by = setdiff(intersect(
          colnames(iotc_only_number),
          colnames(IOTC_conv_fact_mapped %>% dplyr::select(-measurement_unit))
        ), "measurement_value")
      ) %>%
      dplyr::mutate(
        measurement_value = measurement_value * conversion_factor,
        measurement_unit = "t"
      ) %>%
      dplyr::select(dplyr::all_of(original_cols))
    
    iotc_new <- dplyr::bind_rows(
      iotc_only_number_converted,
      iotc_tons_or_mixed
    )
    
    georef_dataset_not_iotc <- georef_dataset %>%
      dplyr::filter(source_authority != "IOTC")
    
    georef_dataset <- dplyr::bind_rows(
      iotc_new,
      georef_dataset_not_iotc
    ) %>%
      dplyr::group_by(dplyr::across(-measurement_value)) %>%
      dplyr::summarise(
        measurement_value = sum(measurement_value, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (isTRUE(opts$recap_each_step)) {
      CWP.dataset::function_recap_each_step(
        "IOTC_unit_conversion",
        georef_dataset,
        paste0(
          "IOTC catch records expressed only in number of fish are converted ",
          "to tons using IOTC-specific conversion factors. ",
          "When the same stratum is available both in tons and number of fish, ",
          "the number-based record is not converted and is removed."
        ),
        "IOTC_conv_fact_mapped",
        list(opts$unit_conversion_iotc_file),
        entity
      )
    }
    
    config$logger.info("End IOTC specific unit conversion")
    
    # IRD / other conversion -------------------------------------------------
    
    config$logger.info("Begin IRD upgraded unit conversion")
    
    georef_dataset <- perform_unit_conversion(
      conversion_factor_csv =
        opts$unit_conversion_ird_file %||% "data/fact_conv_ird_complete_01_mai_2026.csv",
      unit_conversion_codelist_geoidentifiers_conversion_factors =
        opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
      georef_dataset = georef_dataset,
      entity = entity,
      config = config,
      opts = opts,
      step_description = "Harmonising units on IRD upgraded data",
      addeddescription = paste0(
        "This conversion factors dataset contains conversion factors for strata ",
        "with the following details: gear_type, source_authority, species, ",
        "time_start-time_end and main geographical area. A stratum is converted ",
        "only if the conversion factor fully matches the stratum. "
      )
    )
    
    config$logger.info("End IRD upgraded unit conversion")
    
  #   # Final cleanup ---------------------------------------------------------- useless 
  #   
  #   georef_dataset <- georef_dataset %>%
  #     dplyr::ungroup() %>%
  #     dplyr::mutate(
  #       measurement_value = as.numeric(measurement_value),
  #       measurement_unit = dplyr::case_when(
  #         measurement_unit %in% c("MT", "t") ~ "t",
  #         measurement_unit %in% c("NO", "no") ~ "no",
  #         TRUE ~ as.character(measurement_unit)
  #       )
  #     ) %>%
  #     dplyr::group_by(dplyr::across(-measurement_value)) %>%
  #     dplyr::summarise(
  #       measurement_value = sum(measurement_value, na.rm = TRUE),
  #       .groups = "drop"
  #     )
  #   
  #   if (isTRUE(opts$recap_each_step)) {
  #     CWP.dataset::function_recap_each_step(
  #       "Unit_conversion_final",
  #       georef_dataset,
  #       paste0(
  #         "Final dataset after IOTC-specific conversion and IRD upgraded ",
  #         "conversion factors. Converted catch is expressed in tons when possible."
  #       ),
  #       "perform_unit_conversion",
  #       list(
  #         opts$unit_conversion_iotc_file,
  #         opts$unit_conversion_ird_file,
  #         opts$unit_conversion_codelist_geoidentifiers_conversion_factors
  #       ),
  #       entity
  #     )
  #   }
  # }
  
  

# Minimum conversion factors ----------------------------------------------

  df_min_conversion <- data.frame(
    species = c("ALB", "BET", "BFT", "BLM", "BUM", "MLS", 
                "SBF", "SFA", "SKJ", "SSP", "SWO", "YFT"),
    min_conv_factor = c(0.00781, 0.0277, 0.0556, 0.101, 0.0372, 0.0157,
                        0.06, 0.0153, 0.00423, 0.02, 0.0219, 0.0226),
    stringsAsFactors = FALSE
  )
  
  georef_dataset_number_converted <- georef_dataset %>%
    dplyr::filter(measurement_unit == "no") %>%
    dplyr::inner_join(df_min_conversion, by = "species") %>%
    dplyr::mutate(
      measurement_value = as.numeric(measurement_value) * min_conv_factor,
      measurement_unit = "t"
    ) %>%
    dplyr::select(-min_conv_factor)
  
  georef_dataset <- dplyr::bind_rows(
    georef_dataset_number_converted,
    georef_dataset %>% dplyr::filter(measurement_unit == "t")
  )
  
  if (isTRUE(opts$recap_each_step)) {
    CWP.dataset::function_recap_each_step(
      "Conversion_not_converted_with_minimal_conv_fact",
      georef_dataset,
      paste0(
        "Final conversion for remaining no"
      ),
      "inner_join",
      list(
      ),
      entity
    )
  }
  
  # Spatial aggregation -----------------------------------------------------
  
  if (isTRUE(opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) {
    
    stepLogger(1, stepnumber, "Aggregate 1-degree data to 5-degree resolution")
    stepnumber <- stepnumber + 1
    
    one_degree <- georef_dataset %>%
      dplyr::filter(substr(geographic_identifier, 1, 1) == "5")
    
    five_degree <- georef_dataset %>%
      dplyr::filter(substr(geographic_identifier, 1, 1) == "6")
    
    one_degree_aggregated <- one_degree %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        geographic_identifier =
          transform_cwp_code_from_1deg_to_5deg(geographic_identifier)
      ) %>%
      dplyr::ungroup()
    
    georef_dataset <- dplyr::bind_rows(one_degree_aggregated, five_degree)
    
    if (isTRUE(opts$recap_each_step)) {
      CWP.dataset::function_recap_each_step(
        "Aggregation_5deg",
        georef_dataset,
        "Data with spatial resolution below 5 degrees are aggregated to 5-degree CWP grid cells.",
        "transform_cwp_code_from_1deg_to_5deg",
        list(),
        entity
      )
    }
  }
  
  # Resolution filter -------------------------------------------------------
  
  if (!is.null(opts$resolution_filter)) {
    
    stepLogger(1, stepnumber, "Filter on spatial resolution")
    stepnumber <- stepnumber + 1
    
    georef_dataset <- georef_dataset %>%
      dplyr::filter(substr(geographic_identifier, 1, 1) == opts$resolution_filter)
    
    if (isTRUE(opts$recap_each_step)) {
      CWP.dataset::function_recap_each_step(
        "Filtering_on_spatial_resolution",
        georef_dataset,
        paste0("Dataset filtered on spatial resolution: ", opts$resolution_filter),
        "dplyr::filter",
        list(opts$resolution_filter),
        entity
      )
    }
  }
  
  # Other filters -----------------------------------------------------------
  
  if (!is.null(opts$filtering)) {
    
    parameter_filtering <- opts$filtering
    
    if (is.character(parameter_filtering)) {
      parameter_filtering <- eval(parse(text = toString(parameter_filtering)))
    }
    
    matchingList <- parameter_filtering %>%
      purrr::keep(~ !is.null(.))
    
    georef_dataset <- dimension_filtering_function(
      georef_dataset,
      filtering_params = matchingList
    )
    
    if (isTRUE(opts$recap_each_step)) {
      CWP.dataset::function_recap_each_step(
        "Filtering",
        georef_dataset,
        "Optional filters were applied on the dataset.",
        "dimension_filtering_function/dplyr::filter",
        list(opts$filtering, opts$gear_filter),
        entity
      )
    }
  }
  
  

  
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
  
  config$logger.info("End: Level 1 Tuna Atlas dataset has been created.")
  
  rm(georef_dataset)
  gc()
  
  invisible(TRUE)
}