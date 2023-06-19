# filtering on minimum time_start -----------------------------------------
time_extent_harmonisation = function(df_input){
  config$logger.info("filtering on minimum time_start ")
  
  
  # extract the maximum year of declaration for each source_authority
  max_years <- df_input %>%
    group_by(source_authority) %>%
    summarise(max_time_start = max(time_start))
  
  # check if not all the source_authority columns have the same maximum year of declaration
  if (length(unique(max_years$max_time_start)) > 1) {
    config$logger.info("Careful, not all the source_authority has the same maximum year of declaration")
    
    
    # get the minimum time_start of all the maximum time_start of each source_authority
    min_time_start <- min(max_years$max_time_start)
    
    # filter the df_input based on the minimum time_start of all the maximum time_start of each source_authority
    df_input <- df_input %>%
      filter(time_start <= min_time_start)
  }
  
}
  
  

