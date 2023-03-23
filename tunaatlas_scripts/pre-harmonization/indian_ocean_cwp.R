function(action, entity, config){
  
  require(dplyr)
  
  catches <- readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]))
  filename_dsd <- entity$data$source[[2]] #structure
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  #----------------------------------------------------------------------------------------------------------------------------
  
  ##Catches
  catches <- catches %>% rename(schooltype = fishing_mode, gear = gear_type, catchtype = measurement_type, schooltype = fishing_mode, value = measurement_value, unit = measurement_unit ) 
  
  catches <- catches %>% mutate( geographic_identifier= as.character(geographic_identifier))
  

  colnames(catches)<-c("fishingfleet","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
  
  #----------------------------------------------------------------------------------------------------------------------------
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(catches$time_start), "%Y"), "-01-01"),
    paste0(format(max(catches$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #@geoflow -> export as csv
  output_name_dataset <- file.path(dirname(filename_dsd), paste0(entity$identifiers[["id"]], "_harmonized.csv"))
  write.csv(catches, output_name_dataset, row.names = FALSE)
  output_name_codelists <-  file.path(dirname(filename_dsd), paste0(entity$identifiers[["id"]], "_codelists.csv"))
  file.rename(from = entity$getJobDataResource(config, filename_dsd), to = output_name_codelists)
  #----------------------------------------------------------------------------------------------------------------------------
  # entity$addResource("source", output_name_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  

}  
  
