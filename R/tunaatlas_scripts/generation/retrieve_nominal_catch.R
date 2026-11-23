#' Retrieve the nominal catch datasets of a geoflow entity
#'
#' Finds the nominal catch files among the data sources of the entity, keeps
#' those of the included tRFMOs (gta_nominal_catch_files()) and reads them
#' (gta_read_nominal_catch(), R/core/nominal_catch.R).
#'
#' @param entity The geoflow entity.
#' @param config The geoflow configuration.
#' @param options The action options: include_IOTC, include_ICCAT,
#'   include_WCPFC, include_CCSBT, include_IATTC.
#' @return The nominal catch of the included tRFMOs (data frame).
retrieve_nominal_catch <- function(entity, config, options){
  source(here::here("R/core/gta_data.R"))
  source(here::here("R/core/nominal_catch.R"))
  #list of dataset files (from entity data sources)
  dataset_files <- sapply(entity$data$source, function(x){ entity$getJobDataResource(config, x) })
  include <- list(IOTC = options$include_IOTC, ICCAT = options$include_ICCAT, WCPFC = options$include_WCPFC,
                  CCSBT = options$include_CCSBT, IATTC = options$include_IATTC)
  gta_read_nominal_catch(gta_nominal_catch_files(dataset_files, include))
}
