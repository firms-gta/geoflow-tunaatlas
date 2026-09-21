#' Process and aggregate georeferenced dataset
#'
#' This function aggregates a georeferenced or a nominal dataset by its dimensions, processes the temporal extent,
#' appends codelists if available, and exports the results to CSV files.
#' This function also enrich the entity adding resources
#'
#' @param georef_dataset A data frame containing the georeferenced dataset to be processed.
#' @param entity An geoflow entity object that contains metadata and identifiers.
#' @param config A geoflow config objects.
#' @param opts A list of options, including geom_table and any other required settings.
#'
#' @return No return value. The function performs operations and exports files directly.
#' @export
process_and_aggregate_dataset <- function(dataset, entity, config, opts, 
  columns_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode", "time_start", "time_end", "year", "month", "quarter", "geographic_identifier", "measurement_unit", "measurement_value", 
                      "measurement", "measurement_processing_level")) {

  # # Aggregation by dimensions
  # dataset <- georef_dataset %>%
  #   dplyr::group_by(.dots = setdiff(colnames(georef_dataset), "measurement_value")) %>%
  #   dplyr::summarise(measurement_value = sum(measurement_value)) %>%
  #   # head(1000) %>%
  #   dplyr::ungroup()
  # 
  # dataset <- as.data.frame(dataset)
  
  # Correct measurement unit if necessary
  idx <- which(dataset$measurement_unit == "TRUE")
  if (length(idx) > 0) {
    dataset$measurement_unit[idx] <- "t"
  }
  
  # Convert time columns to Date format
  dataset$time_start <- as.Date(dataset$time_start)
  dataset$time_end <- as.Date(dataset$time_end)
  
  # Update entity with temporal extent
  dataset_temporal_extent <- paste(as.character(min(dataset$time_start)), as.character(max(dataset$time_end)), sep = "/")
  entity$setTemporalExtent(dataset_temporal_extent)
  
  dataset_files <- sapply(
    entity$data$source,
    function(x) entity$getJobDataResource(config, x)
  )
  
  codelists_path <- dataset_files[
    grepl("codelist", basename(dataset_files), ignore.case = TRUE)
  ][1]
  
  if (file.exists(codelists_path)) {
    log_info(
      paste0(
        "Loading global codelists from: ",
        codelists_path
      )
    )
    
    df_codelists <- read.csv(codelists_path)
    
  } else {
    config$logger.warn(
      paste0(
        "Global codelists file not found: ",
        codelists_path,
        ". The dataset will be generated without codelist metadata. ",
        "The codelists file is required by load_dataset() to associate ",
        "each dimension with its corresponding global codelist. ",
        "The workflow will therefore fail if this dataset is later loaded into the database."
      )
    )
    
    df_codelists <- NULL
  }
  
  dataset$geographic_identifier = as.character(dataset$geographic_identifier)
  # Export dataset as CSV
  output_name_dataset <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
  readr::write_csv(dataset, output_name_dataset)
  
  # Create enriched public dataset
  output_name_dataset_public <- file.path("data", paste0(entity$identifiers[["id"]], "_public.csv"))
  dataset$year <- as.integer(format(dataset$time_end, "%Y"))
  dataset$month <- as.integer(format(dataset$time_end, "%m"))
  dataset$quarter <- as.integer(substr(quarters(dataset$time_end), 2, 2))
  
  
  columns_to_keep <- intersect(colnames(dataset), columns_to_keep)
  
  dataset <- dataset[, columns_to_keep]
  readr::write_csv(dataset, output_name_dataset_public)
  
  # Export codelists if available
  if (!is.null(df_codelists)) {
    output_name_codelists <- file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv"))
    write.csv(df_codelists, output_name_codelists, row.names = FALSE)
    entity$addResource("codelists", output_name_codelists)
  }
  
  #write to service dbi
  entity$data$features = dataset
  # if(entity$data$upload) writeWorkflowJobDataResource(entity=entity,config=config,type="dbtable",useFeatures=TRUE,useUploadSource=TRUE, createIndexes=TRUE)
  entity$addResource("fact", opts$fact)
  entity$addResource("geom_table", opts$geom_table)
  
  # Add resources to the entity
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("public", output_name_dataset_public)
  
  # Log completion
  log_info("-----------------------------------------------------------------------------------------------------")
  log_info("End: Your tuna atlas dataset has been created!")
  log_info("-----------------------------------------------------------------------------------------------------")
  
  # Clean up
  rm(dataset)
  gc()
  return(NULL)
}
