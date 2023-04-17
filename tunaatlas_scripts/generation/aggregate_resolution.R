aggregate_resolution =function (con, df_input, resolution) 
{
  columns_df_input <- colnames(df_input)
  
  
  cwp_grid_data_with_resolution_as_required <- subset(df_input, grepl(paste0("^", resolution), geographic_identifier))
  cwp_grid_data_with_resolution_not_as_required <- setdiff(df_input, cwp_grid_data_with_resolution_as_required)
  
  df_input_distinct_area <- unique(cwp_grid_data_with_resolution_not_as_required$geographic_identifier)
  df_input_distinct_area <- paste(unique(df_input_distinct_area), 
                                  collapse = "','")
  
  
  areas_to_project_data_to_aggregate <- dbGetQuery(con, paste0("SELECT\n      left(a2.code,7) as input_geographic_identifier,\n      left(a1.code,7) as geographic_identifier_project\n      from\n      area.cwp_grid a1,\n      area.cwp_grid a2\n      where\n      a2.code IN ('", 
                                                               df_input_distinct_area, "') and\n      LEFT(CAST(a1.code AS TEXT), 1) = '6' and LEFT(CAST(a2.code AS TEXT), 1) = '5' and \n      ST_Within(a2.geom, a1.geom)\n      UNION\n      SELECT\n      a2.code as input_geographic_identifier,\n      left(a1.code,7) as geographic_identifier_project\n      from\n      area.cwp_grid a1,\n      area.irregular_areas_task2_iotc a2\n      where\n      a2.code IN ('", 
                                                               df_input_distinct_area, "') and\n      LEFT(CAST(a1.code AS TEXT), 1)='6' and \n      ST_Within(a2.geom, a1.geom)")) %>% distinct()
  
  

  if (nrow(areas_to_project_data_to_aggregate) > 0) {
    
    df_input_to_aggregate <- inner_join(cwp_grid_data_with_resolution_not_as_required, areas_to_project_data_to_aggregate, 
                                        by = c(geographic_identifier = "input_geographic_identifier")) 
    
    df_input_not_aggregated <- df_input_to_aggregate %>% filter(is.na(geographic_identifier_project))
    
    df_input_to_aggregate <- df_input_to_aggregate %>% filter(!is.na(geographic_identifier_project))
    
    
    df_input_to_aggregate <- df_input_to_aggregate %>% group_by_(.dots = setdiff(c(columns_df_input, 
                                                                                   "geographic_identifier_project"), c("geographic_identifier", 
                                                                                                                       "value"))) %>% summarise(value = sum(value))
    df_input_to_aggregate$geographic_identifier <- df_input_to_aggregate$geographic_identifier_project
    df_input_to_aggregate$geographic_identifier_project <- NULL
    
  }
  else {
    df_input_to_aggregate = NULL
  }
  df_input_final_aggregated_on_resolution_to_aggregate <- rbind(data.frame(cwp_grid_data_with_resolution_as_required),
                                                                data.frame(df_input_to_aggregate)#, data.frame(df_input_areas_sup_to_resolution_to_aggregate)
                                                                )
  if (!is.null(df_input_final_aggregated_on_resolution_to_aggregate)) {
    sum_fact_to_reallocate <- df_input_final_aggregated_on_resolution_to_aggregate %>% 
      group_by(unit) %>% summarise(value_reallocate = sum(value))
    sum_whole_df_input <- df_input %>% group_by(unit) %>% 
      summarise(value = sum(value))
    stats_reallocated_data <- left_join(sum_whole_df_input, 
                                        sum_fact_to_reallocate)
    stats_reallocated_data$percentage_reallocated <- stats_reallocated_data$value_reallocate/stats_reallocated_data$value * 
      100
  }
  else {
    stats_reallocated_data = NULL
  }
  return(list(df = df_input_final_aggregated_on_resolution_to_aggregate, df_input_not_aggregated = df_input_not_aggregated,
              stats = stats_reallocated_data))
}
