function_raise_data<-function(fact,source_authority_filter,dataset_to_raise,dataset_to_compute_rf,nominal_dataset_df,x_raising_dimensions,
                              decrease_when_rf_inferior_to_one = FALSE, remove_corresponding_number = TRUE){
  
  dataset_to_raise<-dataset_to_raise[which(dataset_to_raise$source_authority %in% source_authority_filter),]
  
  dataset_to_compute_rf<-dataset_to_compute_rf[which(dataset_to_compute_rf$source_authority %in% source_authority_filter),]
  
  nominal_dataset_df<-nominal_dataset_df[which(nominal_dataset_df$source_authority %in% source_authority_filter),]
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/raise_get_rf.R")
  
  df_rf <- raise_get_rf(df_input_incomplete = dataset_to_compute_rf,
                        df_input_total = nominal_dataset_df,
                        x_raising_dimensions = c(x_raising_dimensions,"measurement_unit")
  )
  saveRDS(df_rf, paste0("data/",gsub(Sys.time(),pattern = " ", replacement = "_"),"raisingfactordataset.rds"))
  
  if (fact=="catch"){
    raising_dimensions=c(x_raising_dimensions,"measurement_unit")
  } else if (fact=="effort"){
    raising_dimensions=x_raising_dimensions
    df_rf$measurement_unit=NULL
  }
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/raise_incomplete_dataset_to_total_dataset.R")
  
  data_raised<-raise_incomplete_dataset_to_total_dataset(df_input_incomplete = dataset_to_raise,
                                                         df_input_total = nominal_dataset_df,
                                                         df_rf = df_rf,
                                                         x_raising_dimensions = raising_dimensions,
                                                         decrease_when_rf_inferior_to_one = decrease_when_rf_inferior_to_one,
                                                         threshold_rf = NULL)
  
  if(remove_corresponding_number){
  data_raised <- data_raised$df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across(-measurement_value)) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), .groups = 'drop')
  
  if(decrease_when_rf_inferior_to_one){
    df_rf <- df_rf
  } else { # si on decrease pas, on enlève pas les nombres qui correspondent à des tonnes qui n'ont pas été augmentées
    df_rf <- df_rf %>% dplyr::filter(round(rf,6) > 1)
  }
  
  corresponding_number_to_raised_data <- data_raised %>% 
    dplyr::filter(measurement_unit == "no") %>% 
    dplyr::mutate(year = lubridate::year(time_start))%>% 
    dplyr::select(x_raising_dimensions) %>% dplyr::distinct() %>% 
    dplyr::inner_join(df_rf %>% 
                        dplyr::select(x_raising_dimensions)%>% dplyr::distinct(),
                      by = x_raising_dimensions) %>% 
    dplyr::mutate(measurement_unit = "no") %>%
    dplyr::distinct()
  
  # inner_join_removed <- data_raised%>% 
  #   dplyr::mutate(year = lubridate::year(time_start)) %>% dplyr::inner_join(corresponding_number_to_raised_data,by = c("source_authority", "species", "gear_type", "fishing_fleet", "measurement_unit", "year"))
  
  data_raised <- data_raised %>% 
    dplyr::mutate(year = lubridate::year(time_start)) %>% 
    anti_join(corresponding_number_to_raised_data, by = c(x_raising_dimensions,"measurement_unit")) %>% 
    dplyr::select(-year)
  }
  
  return(list(data_raised = data_raised, df_rf = df_rf))
  
}