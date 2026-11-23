#' Format pre-harmonized data to cwp format (geoflow action)
#'
#' geoflow action of the entity: finds its files, runs the core function gta_rawdata_cwp_format()
#' (R/core/rawdata_cwp_format.R), writes the outputs and declares the resources of
#' the entity. The processing itself is in the core function, usable without geoflow.
#'
#' @param action The action context from geoflow.
#' @param entity The entity context from geoflow.
#' @param config The configuration context from geoflow.
function(action, entity, config){
  
  require(dplyr)
# Historical name for the dataset at source  iccat_catch_all_1m_firms_level0_20230405.csv or iotc_catch_all_1m_firms_level0.csv

  filename1 <- entity$data$source[[1]]
# Historical name for the dataset at source  iccat_catch_all_1m_firms_level0_20230405.csv or iotc_catch_all_1m_firms_level0.csv
  filename2 <- entity$data$source[[2]]
# Historical name for the dataset at source  iccat_catch_code_lists.csv or iotc_catch_code_lists.csv
  path_to_raw_dataset <- entity$getJobDataResource(config, entity$data$source[[1]])
# Historical name for the dataset at source  iccat_catch_all_1m_firms_level0_20230405.csv or iotc_catch_all_1m_firms_level0.csv
  
  opts <- options()
  options(encoding = "UTF-8")

  source(here::here("R/core/gta_data.R"))
  source(here::here("R/core/rawdata_cwp_format.R"))
  catches <- gta_rawdata_cwp_format(path_to_raw_dataset)

  dataset_temporal_extent <- paste(
    paste0(format(min(catches$time_start), "%Y"), "-01-01"),
    paste0(format(max(catches$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #----------------------------------------------------------------------------------------------------------------------------
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(min(catches$time_start),max(catches$time_end),sep = "/")
  entity$setTemporalExtent(dataset_temporal_extent)
  
  base1 <- tools::file_path_sans_ext(basename(filename1))
  #@geoflow -> export as csv
  # output in same folder as path_to_raw_dataset 
  output_name_dataset   <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_harmonized.csv"))
  output_name_codelists <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_codelists.csv"))
  
  write.csv(catches, output_name_dataset, row.names = FALSE)
  
  file.rename(
    from = entity$getJobDataResource(config, filename2),
    to   = output_name_codelists
  )
  #----------------------------------------------------------------------------------------------------------------------------
  entity$addResource("source", output_name_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  
  
}  
