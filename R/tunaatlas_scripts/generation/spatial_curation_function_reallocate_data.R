spatial_curation_function_reallocate_data = function (df_input, dimension_reallocation, vector_to_reallocate, 
          reallocation_dimensions) 
{
  nb_strata_combinations_input <- df_input %>% select(reallocation_dimensions) %>% 
    distinct()
  total_before <- sum(df_input$measurement_value)
  index.dataToReallocate <- which(df_input[[dimension_reallocation]] %in% vector_to_reallocate)
  
  cat(paste0(length(index.dataToReallocate), " lines have to be removed and their catch measurement_value has to be reallocated in related lines (same strata) \n "))
  if (length(index.dataToReallocate) > 0) {
    dataset_to_reallocate <- df_input[index.dataToReallocate, 
    ]
    dataset_to_reallocate <- dataset_to_reallocate %>% group_by_(.dots = reallocation_dimensions) %>% 
      summarise(measurement_value = sum(measurement_value))
    data_reallocated <- semi_join(dataset_to_reallocate, 
                                  df_input, by = reallocation_dimensions)
    perc_data_reallocated <- sum(data_reallocated$measurement_value)/sum(dataset_to_reallocate$measurement_value) * 
      100
    sum_fact_to_reallocate <- data_reallocated %>% group_by(measurement_unit) %>% 
      summarise(measurement_value_reallocate = sum(measurement_value))
    sum_whole_dataset <- df_input %>% group_by(measurement_unit) %>% 
      summarise(measurement_value = sum(measurement_value))
    stats_reallocated_data <- left_join(sum_whole_dataset, 
                                        sum_fact_to_reallocate)
    df_input <- df_input[-index.dataToReallocate, ]
    nb_strata_combinations_remaining_lines <- df_input %>% 
      select(reallocation_dimensions) %>% distinct
    nb_strata_combinations_removed_lines <- df_input[index.dataToReallocate, 
    ] %>% select(reallocation_dimensions) %>% distinct
    inner_join
    inner_join(nb_strata_combinations_removed_lines, nb_strata_combinations_remaining_lines)
    wxc <- left_join(df_input, dataset_to_reallocate, by = reallocation_dimensions)
    wxc2 <- wxc %>% filter(!is.na(measurement_value.y)) %>% group_by_(.dots = c(reallocation_dimensions, 
                                                                    "measurement_value.y")) %>% summarise(number = n())
    wxc2$measurement_value_realloc <- wxc2$measurement_value.y/wxc2$number
    wxc2$measurement_value.y <- NULL
    wxc2$number <- NULL
    wxc2 <- data.frame(wxc2)
    head(wxc2)
    df_input <- left_join(df_input, wxc2, by = reallocation_dimensions)
    index.dataToReallocate <- which(!is.na(df_input$measurement_value_realloc))
    df_input$measurement_value[index.dataToReallocate] <- df_input$measurement_value[index.dataToReallocate] + 
      df_input$measurement_value_realloc[index.dataToReallocate]
    df_input$measurement_value_realloc <- NULL
  }
  else {
    dataset_to_reallocate <- NULL
    stats_reallocated_data <- NULL
    perc_data_reallocated <- NULL
  }
  total_after <- sum(df_input$measurement_value)
  difference <- total_before - total_after
  difference
  sum_fact_to_reallocate$measurement_value_reallocate - difference
  return(list(df = df_input, stats = stats_reallocated_data))
}
