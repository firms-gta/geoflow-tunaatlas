tidying_iotc_conversion_factor <- function(action, entity, config){
  

  
    iotc_data_initial <- as.data.frame(readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]), guess_max = 0, 
                                                       col_types = cols(AVG_WEIGHT = col_double())))
    
    iotc_data_final <- iotc_data_initial%>%
      mutate(geographic_identifier = FISHING_GROUND_CODE,
             unit = "no", unit_target = "t", species = SPECIES_CODE, gear = GEAR_CODE,source_authority = "IOTC",
             conversion_factor = AVG_WEIGHT/1000, 
             time_start = lubridate::as_date(paste0(YEAR,"-",MONTH_START, "-01 "))) %>%
      mutate(time_end =lubridate::ceiling_date(time_start, "month") - 1 ) %>%
      select(gear	,source_authority,	species	,geographic_identifier,	time_start,	time_end,	unit	,unit_target,	conversion_factor) %>% mutate(value = conversion_factor) %>% 
      select(-conversion_factor)


  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
    iotc_data_final$time_start <- as.Date(iotc_data_final$time_start)
    iotc_data_final$time_end <- as.Date(iotc_data_final$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(iotc_data_final$time_start), "%Y"), "-01-01"),
    paste0(format(max(iotc_data_final$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #----------------------------------------------------------------------------------------------------------------------------
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(min(iotc_data_final$time_start),max(iotc_data_final$time_end),sep = "/")
  entity$setTemporalExtent(dataset_temporal_extent)
  
  iotc_data_final$time_start <- as.character(iotc_data_final$time_start)
  iotc_data_final$time_end <- as.character(iotc_data_final$time_end)
  iotc_data_final$geographic_identifier <- as.character(iotc_data_final$geographic_identifier)
  
  
  filename1 <- entity$data$source[[1]] #data
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  
  output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
  write.csv(iotc_data_final, output_name_dataset, row.names = FALSE)
  #----------------------------------------------------------------------------------------------------------------------------
  # entity$addResource("source", output_name_dataset)
  entity$addResource("harmonized", output_name_dataset)

  
}  


