tidying_GTA_data_for_comparison = function(dataframe, shape = NULL, 
species_group_dataframe = NULL, cl_cwp_gear_level2_dataframe = NULL ){
  if(is_string(dataframe)){
    dataframe <- readRDS(dataframe)
  }
  
  if("geographic_identifier"%in%colnames(dataframe) & !is.null(shape)){
    dataframe <- dataframe%>%  dplyr::left_join(shape%>% 
                                                  dplyr::select(GRIDTYPE, cwp_code), by = c("geographic_identifier"="cwp_code")) 
    print_map <- TRUE
  } else {print_map <- FALSE}
  
  if("GRIDTYPE"%in%colnames(dataframe)){
    dataframe <- dataframe%>%dplyr::mutate(GRIDTYPE = as.character(GRIDTYPE))
  }
  if(!is.null(species_group_dataframe) && ("species" %in% colnames(dataframe))){
    dataframe <- dataframe %>% dplyr::left_join(species_group_dataframe%>% dplyr::distinct(), by = c("species"))
  }
  if("gear_type" %in%colnames(dataframe) & !is.null(cl_cwp_gear_level2_dataframe) ){
    dataframe <- dataframe %>% dplyr::left_join(cl_cwp_gear_level2_dataframe, by = c("gear_type" = "Code"))
  }
  
  dataframe <- dataframe%>%dplyr::mutate(measurement_unit = dplyr::case_when(measurement_unit %in% c("MT","t","MTNO", "Tons")~ "Tons", 
  measurement_unit %in% c("NO", "NOMT","no", "Number of fish")~"Number of fish", TRUE ~ measurement_unit)) 
  
  # dataframe <- dataframe %>% dplyr::mutate(measurement_value = sum(measurement_value))
  
  return(dataframe = dataframe)
}
