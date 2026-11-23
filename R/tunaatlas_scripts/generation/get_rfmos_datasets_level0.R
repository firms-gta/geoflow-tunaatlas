#' Georeferenced catch or effort of one tRFMO, from a geoflow entity (level 0)
#'
#' Lists the data files of the entity (all its sources but the first) and runs
#' the core function gta_rfmo_dataset_level0() (R/core/rfmo_datasets_level0.R)
#' on them, read from the data/ folder of the job.
#'
#' @param rfmo `"IOTC"`, `"WCPFC"`, `"CCSBT"`, `"ICCAT"` or `"IATTC"`.
#' @param entity The geoflow entity.
#' @param config The geoflow configuration.
#' @param options The action options (see gta_rfmo_dataset_level0()).
#' @return The dataset of the tRFMO (data frame), or NULL if not included.
get_rfmos_datasets_level0 <- function(rfmo, entity, config, options){
  
  #list of dataset files (from entity data sources)
  dataset_files <- sapply(entity$data$source[1:length(entity$data$source)], function(x){ entity$getJobDataResource(config, x) })
  dataset_files <- sapply(
    entity$data$source[2:length(entity$data$source)],
    function(x) {
      path <- entity$getJobDataResource(config, x)
      path <- fs::path_norm(path)
      path <- gsub("/data/data/", "/data/", path)
      path <- gsub("/\\./", "/", path)          
      path <- gsub("//+", "/", path)            
      path
    }
  ) #for local, preventing having data/./data/ in path 
  names(dataset_files) <- entity$data$source[2:length(entity$data$source)]
  names(dataset_files) <- basename(names(dataset_files))
  
  # Processing: core function gta_rfmo_dataset_level0() (R/core/rfmo_datasets_level0.R),
  # on the files of the entity (read from the data/ folder of the job)
  source(here::here("R/core/rfmo_datasets_level0.R"))
  dataset <- gta_rfmo_dataset_level0(rfmo, names(dataset_files), options, data_dir = "data")
  return(dataset)
}
