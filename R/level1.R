#' Global Tuna Atlas catch, level 1 (units harmonised)
#'
#' Builds the level 1 catch dataset from the level 0 georeferenced catch: keeps
#' the main species, converts the catch in number of fish into tons (IOTC
#' conversion factors, then IRD conversion factors, then a minimal conversion
#' factor by species for what remains), then optionally aggregates to 5 degrees
#' and filters.
#'
#' @param georef_dataset The level 0 georeferenced catch: a data frame, or the
#'   path to the CSV file.
#' @param con Connection to the Tuna Atlas database, or NULL (default): not
#'   needed with the IRD conversion areas (areas_conversion_factors_numtoweight_ird),
#'   whose cells are classified from their CWP code.
#' @param unit_conversion_convert Convert the units (IOTC and IRD factors).
#' @param unit_conversion_iotc_file CSV of the IOTC conversion factors.
#' @param unit_conversion_ird_file CSV of the IRD conversion factors.
#' @param unit_conversion_codelist_geoidentifiers_conversion_factors Code list
#'   of the areas of the IRD conversion factors.
#' @param mapping_map_code_lists Whether the code lists were mapped (passed to
#'   the IRD unit conversion).
#' @param fact `"catch"`.
#' @param aggregate_on_5deg_data_with_resolution_inferior_to_5deg Aggregate the
#'   1-degree data to 5 degrees.
#' @param resolution_filter Keep only one spatial resolution (first character of
#'   the CWP code, e.g. `"5"` or `"6"`), or `NULL`.
#' @param filtering Filters on the dimensions (list, or its R code as a
#'   string), see dimension_filtering_function(), or `NULL`.
#' @param gear_filter Only reported in the summary of the filtering step.
#' @param doilevel0,keylevel0 DOI and file name of the level 0 dataset, only
#'   reported in the summary of the input step.
#' @param recap_each_step Write the summary of each step
#'   (CWP.dataset::function_recap_each_step(), in the working directory).
#' @param entity A geoflow entity whose metadata (provenance, description) the
#'   steps complete, or `NULL`.
#' @return The level 1 catch (data frame).
#' @export
gta_level1 <- function(georef_dataset,
                       con = NULL,
                       unit_conversion_convert = TRUE,
                       unit_conversion_iotc_file = "data/IOTC_conv_fact_mapped.csv",
                       unit_conversion_ird_file = "data/fact_conv_ird_complete_01_May_2026.csv",
                       unit_conversion_codelist_geoidentifiers_conversion_factors = "areas_conversion_factors_numtoweight_ird",
                       mapping_map_code_lists = TRUE,
                       fact = "catch",
                       aggregate_on_5deg_data_with_resolution_inferior_to_5deg = FALSE,
                       resolution_filter = NULL,
                       filtering = NULL,
                       gear_filter = NULL,
                       doilevel0 = NULL,
                       keylevel0 = NULL,
                       recap_each_step = FALSE,
                       entity = NULL) {
  url_scripts_generation <- here::here("R/tunaatlas_scripts/generation")

  # The step summaries (CWP.dataset::function_recap_each_step()) write the text
  # of the option expressions they are given: the options are kept in a list
  # named opts so that this text stays "opts$<option>", as in the geoflow action
  opts <- list(doilevel0 = doilevel0, keylevel0 = keylevel0,
               unit_conversion_iotc_file = unit_conversion_iotc_file,
               resolution_filter = resolution_filter, filtering = filtering, gear_filter = gear_filter)

  stepnumber <- 1
  stepLogger <- function(level, step, msg) {
    message(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }

  # Level 0 input -------------------------------------------------------------
  georef_dataset <- gta_data(georef_dataset, reader = function(path) readr::read_csv(path, guess_max = 0))
  georef_dataset <- georef_dataset %>%
    dplyr::mutate(
      measurement_value = as.numeric(measurement_value),
      time_start = as.character(time_start),
      time_end = as.character(time_end)
    ) %>%
    dplyr::filter(species %in% c("YFT", "ALB", "SKJ", "BET", "SWO", "SBF"))

  if (isTRUE(recap_each_step)) {
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

  # Unit conversion -----------------------------------------------------------
  if (isTRUE(unit_conversion_convert)) {
    stepLogger(1, stepnumber, "Harmonising catch units")
    stepnumber <- stepnumber + 1

    original_cols <- colnames(georef_dataset)

    message("Begin IOTC specific unit conversion")

    IOTC_conv_fact_mapped <- readr::read_csv(
      unit_conversion_iotc_file,
      guess_max = 0
    ) %>%
      dplyr::mutate(
        gear_type = as.character(gear_type),
        time_start = as.character(time_start),
        time_end = as.character(time_end),
        geographic_identifier = as.character(geographic_identifier),
        year = lubridate::year(time_start)
      ) %>%
      dplyr::mutate(gear_type = as.character(gear_type), time_start = as.character(time_start)) %>%
      dplyr::mutate(year = lubridate::year(time_start)) %>%
      dplyr::mutate(measurement_unit = dplyr::case_when(measurement_unit == "NO" ~ "no",
                                                        measurement_unit == "MT" ~ "t",
                                                        TRUE ~ measurement_unit)) %>%
      dplyr::mutate(unit_target = dplyr::case_when(unit_target == "NO" ~ "no",
                                                   unit_target == "MT" ~ "t",
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

    # Strata only in number of fish: converted with the IOTC factors
    iotc_only_number <- iotc %>%
      dplyr::filter(measurement_unit == "no", numberunit == 1) %>%
      dplyr::select(dplyr::all_of(original_cols))

    # Strata in tons, or in tons and number: the tons are kept
    iotc_tons_or_mixed <- iotc %>%
      dplyr::filter(!(measurement_unit == "no" & numberunit == 1)) %>%
      dplyr::filter(measurement_unit != "no") %>%
      dplyr::select(dplyr::all_of(original_cols))

    iotc_only_number_converted <- iotc_only_number %>%
      dplyr::inner_join(
        IOTC_conv_fact_mapped %>%
          dplyr::rename(conversion_factor = measurement_value) %>%
          dplyr::select(-measurement_unit) %>%
          dplyr::mutate(year = as.character(year)) %>% dplyr::mutate(conversion_factor = as.numeric(conversion_factor)),
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

    if (isTRUE(recap_each_step)) {
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

    message("End IOTC specific unit conversion")

    message("Begin IRD upgraded unit conversion")
    georef_dataset <- perform_unit_conversion(
      conversion_factor_csv = unit_conversion_ird_file,
      unit_conversion_codelist_geoidentifiers_conversion_factors =
        unit_conversion_codelist_geoidentifiers_conversion_factors,
      georef_dataset = georef_dataset,
      entity = entity,
      config = NULL,
      opts = list(fact = fact, mapping_map_code_lists = mapping_map_code_lists),
      step_description = "Harmonising units on IRD upgraded data",
      addeddescription = paste0(
        "This conversion factors dataset contains conversion factors for strata ",
        "with the following details: gear_type, source_authority, species, ",
        "time_start-time_end and main geographical area. A stratum is converted ",
        "only if the conversion factor fully matches the stratum. "
      ),
      con = con
    )
  }
  message("End IRD upgraded unit conversion")

  # Remaining number of fish: minimal conversion factor by species -------------
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

  if (isTRUE(recap_each_step)) {
    CWP.dataset::function_recap_each_step(
      "Conversion_not_converted_with_minimal_conv_fact",
      georef_dataset,
      paste0(
        "Final conversion for remaining no"
      ),
      "inner_join",
      list(),
      entity
    )
  }

  # Optional aggregation on 5 degrees -----------------------------------------
  if (isTRUE(aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) {
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

    if (isTRUE(recap_each_step)) {
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

  # Optional filter on spatial resolution -------------------------------------
  if (!is.null(resolution_filter)) {
    stepLogger(1, stepnumber, "Filter on spatial resolution")
    stepnumber <- stepnumber + 1

    georef_dataset <- georef_dataset %>%
      dplyr::filter(substr(geographic_identifier, 1, 1) == resolution_filter)

    if (isTRUE(recap_each_step)) {
      CWP.dataset::function_recap_each_step(
        "Filtering_on_spatial_resolution",
        georef_dataset,
        paste0("Dataset filtered on spatial resolution: ", resolution_filter),
        "dplyr::filter",
        list(opts$resolution_filter),
        entity
      )
    }
  }

  # Optional filters on the dimensions ----------------------------------------
  if (!is.null(filtering)) {
    parameter_filtering <- filtering

    if (is.character(parameter_filtering)) {
      parameter_filtering <- eval(parse(text = toString(parameter_filtering)))
    }

    matchingList <- parameter_filtering %>%
      purrr::keep(~ !is.null(.))

    georef_dataset <- dimension_filtering_function(
      georef_dataset,
      filtering_params = matchingList
    )

    if (isTRUE(recap_each_step)) {
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

  georef_dataset
}
