spatial_curation_upgrade_resolution =function (con, df_input, resolution) 
{
  columns_df_input <- colnames(df_input)
  df_input_distinct_area <- unique(df_input$geographic_identifier)
  df_input_distinct_area <- paste(unique(df_input_distinct_area), 
                                  collapse = "','")
  cwp_grid_data_with_resolution_to_upgrade <- dbGetQuery(con, 
                                                         paste0("SELECT codesource_area as geographic_identifier,left(cwp_grid.code,7) as code FROM area.area_labels \n                                                                  JOIN area.cwp_grid\n                                                                  USING (geom)\n                                                                  WHERE codesource_area IN ('", 
                                                                df_input_distinct_area, "')\n                                                                  AND tablesource_area='areas_tuna_rfmos_task2'\n                                                                  and spatial_resolution='", 
                                                                resolution, "'"))
  if (nrow(cwp_grid_data_with_resolution_to_upgrade) != 0) {
    df_input_to_leave_as_so <- inner_join(df_input, cwp_grid_data_with_resolution_to_upgrade, 
                                          by = "geographic_identifier")
    df_input_to_leave_as_so$geographic_identifier <- df_input_to_leave_as_so$code
    df_input_to_leave_as_so$code <- NULL   
    df_input_not_to_leave_as_so <- anti_join(df_input, cwp_grid_data_with_resolution_to_upgrade,
                                             by = "geographic_identifier")
    # df_input_not_to_leave_as_so$geographic_identifier <- df_input_not_to_leave_as_so$code
    # df_input_not_to_leave_as_so$code <- NULL
  }
  else {
    df_input_to_leave_as_so <- NULL
  }
  area_changeresolution <- setdiff(unique(df_input$geographic_identifier), 
                                   cwp_grid_data_with_resolution_to_upgrade$geographic_identifier)
  area_changeresolution <- paste(unique(area_changeresolution), 
                                 collapse = "','")
  areas_to_project_data_to_disaggregate <- dbGetQuery(con, 
                                                      paste0("SELECT\n    left(a2.code,7) as input_geographic_identifier,\n    left(a1.code,7) as geographic_identifier_project\n    from\n    area.cwp_grid a1,\n    area.cwp_grid a2\n    where\n    a2.code IN ( '", 
                                                             area_changeresolution, "') and\n    a1.size_grid='6' and a2.size_grid IN ('1','2','7','8','9') and \n    ST_Within(a1.geom, a2.geom)\n    UNION\n    SELECT\n    a2.code as input_geographic_identifier,\n    left(a1.code,7) as geographic_identifier_project\n    from\n    area.cwp_grid a1,\n    area.irregular_areas_task2_iotc a2\n    where\n    a2.code IN ( '", 
                                                             area_changeresolution, "') and\n    a1.size_grid='6' and \n    ST_Within(a1.geom, a2.geom)")) %>% distinct()
  if (nrow(areas_to_project_data_to_disaggregate) > 0) {
    areas_sup_to_resolution_to_aggregate <- unique(areas_to_project_data_to_disaggregate$input_geographic_identifier)
    df_input_areas_sup_to_resolution_to_aggregate <- df_input %>% 
      filter(geographic_identifier %in% areas_sup_to_resolution_to_aggregate)
  }
  else {
    df_input_areas_sup_to_resolution_to_aggregate = NULL
  }
  # con %>% tbl(in_schema("area", "cwp_grid"))%>% filter(code == "6130045") %>% collect()
  # con %>% tbl(in_schema("area", "irregular_areas_task2_iotc"))%>%  collect()
  # 
  areas_to_project_data_to_aggregate <- dbGetQuery(con, paste0("SELECT\n      left(a2.code,7) as input_geographic_identifier,\n      left(a1.code,7) as geographic_identifier_project\n      from\n      area.cwp_grid a1,\n      area.cwp_grid a2\n      where\n      a2.code IN ('", 
                                                               area_changeresolution, "') and\n      a1.size_grid = '6' and a2.size_grid = '5' and \n      ST_Within(a2.geom, a1.geom)\n      UNION\n      SELECT\n      a2.code as input_geographic_identifier,\n      left(a1.code,7) as geographic_identifier_project\n      from\n      area.cwp_grid a1,\n      area.irregular_areas_task2_iotc a2\n      where\n      a2.code IN ('", 
                                                               area_changeresolution, "') and\n      a1.size_grid='6' and \n      ST_Within(a2.geom, a1.geom)"))
  areas_to_project_data_to_aggregate_without_duplicate_from_disaggregate <- anti_join(areas_to_project_data_to_aggregate,areas_to_project_data_to_disaggregate)
  
  if (nrow(areas_to_project_data_to_aggregate) > 0) {
    
    df_input_to_aggregate <- inner_join(df_input_not_to_leave_as_so, areas_to_project_data_to_aggregate_without_duplicate_from_disaggregate, 
                                        by = c(geographic_identifier = "input_geographic_identifier")) # %>%  #left join beacause there is an error for the 6130045 cwp grid "6130045\n"
    # mutate(geographic_identifier_project = ifelse(!is.na(geographic_identifier_project), geographic_identifier_project,geographic_identifier))
    
    df_input_to_aggregate <- df_input_to_aggregate %>% group_by_(.dots = setdiff(c(columns_df_input, 
                                                                                   "geographic_identifier_project"), c("geographic_identifier", 
                                                                                                                       "value"))) %>% summarise(value = sum(value))
    df_input_to_aggregate$geographic_identifier <- df_input_to_aggregate$geographic_identifier_project
    df_input_to_aggregate$geographic_identifier_project <- NULL
    
  }
  else {
    df_input_to_aggregate = NULL
  }
  
  df_input_final_aggregated_on_resolution_to_aggregate <- rbind(data.frame(df_input_to_leave_as_so), 
                                                                data.frame(df_input_to_aggregate), data.frame(df_input_areas_sup_to_resolution_to_aggregate))
  if (!is.null(df_input_to_aggregate)) {
    sum_fact_to_reallocate <- df_input_to_aggregate %>% 
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
  return(list(df = df_input_final_aggregated_on_resolution_to_aggregate, 
              stats = stats_reallocated_data))
}
