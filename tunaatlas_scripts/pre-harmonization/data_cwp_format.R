function(action, entity, config){
  
  require(dplyr)
  
  catches <- readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]))

  filename1 <- entity$data$source[[1]]
  filename2 <- entity$data$source[[2]]
  path_to_raw_dataset <- entity$getJobDataResource(config, entity$data$source[[1]])
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  #----------------------------------------------------------------------------------------------------------------------------
  
  ##Catches
  catches <- catches %>% dplyr::rename(fishingfleet = fishing_fleet) 
  
  catches <- catches %>% dplyr::mutate( time_start = as.character(time_start), time_end = as.character(time_end),  geographic_identifier= as.character(geographic_identifier))
  
  catches <- catches %>% filter(measurement_value!= 0)
  
  catches<-catches %>% dplyr::select("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value", "source_authority")
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
  catches$time_start <- as.Date(catches$time_start)
  catches$time_end <- as.Date(catches$time_end)
  #we enrich the entity with temporal coverage
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
  
  #@geoflow -> export as csv
  output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
  write.csv(catches, output_name_dataset, row.names = FALSE)
  
  output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
  file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)  #----------------------------------------------------------------------------------------------------------------------------
  # entity$addResource("source", output_name_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  

}  
  
