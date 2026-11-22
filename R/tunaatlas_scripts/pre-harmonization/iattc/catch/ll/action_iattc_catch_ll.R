#' Convert a catch dataset into CWP format (geoflow action)
#'
#' geoflow action of the entity: finds its files, runs the core function gta_rawdata_iattc_catch_ll()
#' (R/core/rawdata_iattc_catch_ll.R), writes the outputs and declares the resources of
#' the entity. The processing itself is in the core function, usable without geoflow.
#'
#' @param action The action context from geoflow.
#' @param entity The entity context from geoflow.
#' @param config The configuration context from geoflow.
function(action, entity, config){

  
  packages <- c("dplyr", "tidyr", "readr", "stringr")
  
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  filename1 <- entity$data$source[[1]] #data
  # Historical name for the dataset at source  PublicLLSharkMt.csv and PublicLLTunaBillfishMt.csv
  filename2 <- entity$data$source[[2]] #structure
  # Historical name for the dataset at source  iattc_catch_code_lists.csv
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  
  opts <- options()
  options(encoding = "UTF-8")

source(here::here("R/core/gta_data.R"))
source(here::here("R/core/rawdata_iattc_catch_ll.R"))
df <- gta_rawdata_iattc_catch_ll(path_to_raw_dataset)

  dataset_temporal_extent <- paste(
    paste0(format(min(df$time_start), "%Y"), "-01-01"),
    paste0(format(max(df$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)

  base1 <- tools::file_path_sans_ext(basename(filename1))
  #@geoflow -> export as csv
  # output in same folder as path_to_raw_dataset 
  output_name_dataset   <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_harmonized.csv"))
  output_name_codelists <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_codelists.csv"))
  
  write.csv(df, output_name_dataset, row.names = FALSE)
  
  file.rename(from = entity$getJobDataResource(config, filename2),    to   = output_name_codelists  )
  #----------------------------------------------------------------------------------------------------------------------------  
  entity$addResource("source", path_to_raw_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  
}
