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
process_and_aggregate_dataset <- function(georef_dataset, entity, config, opts, 
  columns_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode", "time_start", "time_end", "year", "month", "quarter", "geographic_identifier", "measurement_unit", "measurement_value")) {
  
  # Aggregation by dimensions
  dataset <- georef_dataset %>%
    dplyr::group_by(.dots = setdiff(colnames(georef_dataset), "measurement_value")) %>%
    dplyr::summarise(measurement_value = sum(measurement_value)) %>%
    dplyr::ungroup()
  
  dataset <- as.data.frame(dataset)
  
  # Correct measurement unit if necessary
  if (!is.na(any(dataset$measurement_unit == "TRUE"))) {
    if (any(dataset$measurement_unit == "TRUE")) {
      dataset[dataset$measurement_unit == "TRUE", ]$measurement_unit <- "t" # Patch issue
    }
  }
  
  # Convert time columns to Date format
  dataset$time_start <- as.Date(dataset$time_start)
  dataset$time_end <- as.Date(dataset$time_end)
  
  # Update entity with temporal extent
  dataset_temporal_extent <- paste(as.character(min(dataset$time_start)), as.character(max(dataset$time_end)), sep = "/")
  entity$setTemporalExtent(dataset_temporal_extent)
  
  # Append codelists if available
  df_codelists <- NULL
  cl_relations <- entity$relations[sapply(entity$relations, function(x) x$name == "codelists")]
  
  if (length(cl_relations) > 0) {
    config$logger.info("Appending codelists to global dataset generation action output")
    googledrive_baseurl <- "https://drive.google.com/open?id="
    
    if (startsWith(cl_relations[[1]]$link, googledrive_baseurl)) {
      # Downloading file using Google Drive R interface
      config$logger.info("Downloading file using Google Drive R interface")
      drive_id <- unlist(strsplit(cl_relations[[1]]$link, "id="))[2]
      drive_id <- unlist(strsplit(drive_id, "&export"))[1] # Control in case export param is appended
      googledrive::drive_download(file = googledrive::as_id(drive_id), path = file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv")), overwrite = TRUE)
      df_codelists <- read.csv(file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv")))
    } else {
      df_codelists <- read.csv(cl_relations[[1]]$link)
    }
  }
  
  # Output structure
  dataset_list <- list(
    dataset = dataset,
    additional_metadata = NULL,
    codelists = df_codelists # In case the entity was provided with a link to codelists
  )
  
  # Export dataset as CSV
  output_name_dataset <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
  readr::write_csv(dataset_list$dataset, output_name_dataset)
  
  # Create enriched public dataset
  output_name_dataset_public <- file.path("data", paste0(entity$identifiers[["id"]], "_public.csv"))
  dataset_enriched <- dataset_list$dataset
  dataset_enriched$year <- as.integer(format(dataset_enriched$time_end, "%Y"))
  dataset_enriched$month <- as.integer(format(dataset_enriched$time_end, "%m"))
  dataset_enriched$quarter <- as.integer(substr(quarters(dataset_enriched$time_end), 2, 2))
  
  
  columns_to_keep <- intersect(colnames(dataset_enriched), columns_to_keep)
  
  dataset_enriched <- dataset_enriched[, columns_to_keep]
  readr::write_csv(dataset_enriched, output_name_dataset_public)
  
  # Export codelists if available
  if (!is.null(df_codelists)) {
    output_name_codelists <- file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv"))
    write.csv(dataset_list$codelists, output_name_codelists, row.names = FALSE)
    entity$addResource("codelists", output_name_codelists)
  }
  
  #write to service dbi
  dataset_enriched$geographic_identifier = as.character(dataset_enriched$geographic_identifier)
  entity$data$features = dataset_enriched
  if(entity$data$upload) writeWorkflowJobDataResource(entity=entity,config=config,type="dbtable",useFeatures=TRUE,useUploadSource=TRUE, createIndexes=TRUE)
  
  entity$addResource("fact", opts$fact)
  entity$addResource("geom_table", opts$geom_table)
  
  # Add resources to the entity
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("public", output_name_dataset_public)
  
  # Log completion
  config$logger.info("-----------------------------------------------------------------------------------------------------")
  config$logger.info("End: Your tuna atlas dataset has been created!")
  config$logger.info("-----------------------------------------------------------------------------------------------------")
  
  # Clean up
  rm(georef_dataset)
  gc()
  # return(list(entity, georef_dataset, entity, config, opts))
}
