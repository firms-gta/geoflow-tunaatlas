function_duplicate_sbf <- function(georef_dataset,connection, rfmo_not_to_keep_sbf, options_strata_overlap_sbf =c("species"), opts_sbf) {
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  georef_dataset <-
    function_overlapped(
      dataset = georef_dataset,
      con = connection,
      rfmo_to_keep = "CCSBT",
      rfmo_not_to_keep = rfmo_not_to_keep_sbf,
      strata = options_strata_overlap_sbf, opts = opts_sbf
    )
  
  function_recap_each_step(
    paste0("overlap_", tolower("CCSBT"), "_", tolower(rfmo_not_to_keep_sbf)),
    georef_dataset,
    paste0(
      "In this step, the georeferenced data present on the overlapping zone between ", "CCSBT", " and ", rfmo_not_to_keep_sbf, " is handled.",
      "The option for the strata overlapping allows handling the maximum similarities allowed between two data to keep both.",
      "In the case the data is identical on the provided stratas, the remaining data is from ", rfmo_not_to_keep_sbf
    ),
    "function_overlapped",
    list(options_strata_overlap_sbf)
  )
  # config$logger.info(paste0("Applying apply_overlap_sbf_logic for source_authority", rfmo_not_to_keep))
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
}

