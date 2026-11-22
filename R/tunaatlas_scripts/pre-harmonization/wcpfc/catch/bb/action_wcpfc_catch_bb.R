#' Harmonize WCPFC Pole-and-Line Catch Datasets (geoflow action)
#'
#' geoflow action of the entity: finds its files, runs the core function gta_rawdata_wcpfc_catch_bb()
#' (R/core/rawdata_wcpfc_catch_bb.R), writes the outputs and declares the resources of
#' the entity. The processing itself is in the core function, usable without geoflow.
#'
#' @param action The action context from geoflow.
#' @param entity The entity context from geoflow.
#' @param config The configuration context from geoflow.
function(action, entity, config){

if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
}
if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
  filename1 <- entity$data$source[[1]] #data
  filename2 <- entity$data$source[[2]] #structure
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  opts <- options()
  options(encoding = "UTF-8")

source(here::here("R/core/gta_data.R"))
source(here::here("R/core/rawdata_wcpfc_catch_bb.R"))
catches <- gta_rawdata_wcpfc_catch_bb(path_to_raw_dataset)

  dataset_temporal_extent <- paste(
    paste0(format(min(catches$time_start), "%Y"), "-01-01"),
    paste0(format(max(catches$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)

  base1 <- tools::file_path_sans_ext(basename(filename1))
  #@geoflow -> export as csv
  # output in same folder as path_to_raw_dataset 
  output_name_dataset   <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_harmonized.csv"))
  output_name_codelists <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_codelists.csv"))
  
  write.csv(catches, output_name_dataset, row.names = FALSE)
  
  file.rename(  from = entity$getJobDataResource(config, filename2),  to   = output_name_codelists)
  #----------------------------------------------------------------------------------------------------------------------------
  entity$addResource("source", path_to_raw_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
}
