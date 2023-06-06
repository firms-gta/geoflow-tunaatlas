spatial_curation_downgrade_resolution <- function (con, df_input, resolution, remove = FALSE) 
{
  georef_dataset <- df_input
  columns_dataset_input <- colnames(georef_dataset)
  dataset_distinct_area <- unique(georef_dataset$geographic_identifier)
  dataset_distinct_area <- paste(unique(dataset_distinct_area), 
                                 collapse = "','")
  cwp_grid_data_with_resolution_to_downgrade <- dbGetQuery(con, 
                                                           paste0("SELECT codesource_area as geographic_identifier,left(cwp_grid.code,7) as code FROM area.area_labels \n                                                                    JOIN area.cwp_grid\n                                                                    USING (geom)\n                                                                    WHERE codesource_area IN ('", 
                                                                  dataset_distinct_area, "')\n                                                                    AND tablesource_area='areas_tuna_rfmos_task2'\n                                                                    and left(code,1)='", 
                                                                  resolution, "'"))
  if (nrow(cwp_grid_data_with_resolution_to_downgrade) > 0) {
    dataset_not_to_leave_as_so <- anti_join(georef_dataset, cwp_grid_data_with_resolution_to_downgrade, 
                                            by = "geographic_identifier")
    dataset_to_leave_as_so <- inner_join(georef_dataset, cwp_grid_data_with_resolution_to_downgrade, 
                                         by = "geographic_identifier")
    dataset_to_leave_as_so$geographic_identifier <- dataset_to_leave_as_so$code
    dataset_to_leave_as_so$code <- NULL
    
  }  else {
    dataset_to_leave_as_so <- NULL
    dataset_not_to_leave_as_so <- NULL
  }
  # area_changeresolution <- setdiff(unique(georef_dataset$geographic_identifier),
  #                                  cwp_grid_data_with_resolution_to_downgrade$geographic_identifier)
  area_changeresolution <- unique(dataset_not_to_leave_as_so$geographic_identifier)
  area_changeresolution <- paste(unique(area_changeresolution), 
                                 collapse = "','")
  gc()
  if (resolution == 1) {
    a1.size_grid = "'5'"
    a2.size_grid = "('1','2','6','7','8','9')"
  }   else if (resolution == 5) {
    a1.size_grid = "'6'"
    a2.size_grid = "('1','2','7','8','9')"
  }
  areas_to_project_data_to_disaggregate <- dbGetQuery(con, 
                                                      paste0("SELECT\n    left(a2.code,7) as input_geographic_identifier,\n    left(a1.code,7) as geographic_identifier_project\n    from\n    area.cwp_grid a1,\n    area.cwp_grid a2\n    where\n    a2.code IN ( '", 
                                                             area_changeresolution, "') and\n    a1.size_grid=", 
                                                             a1.size_grid, " and a2.size_grid IN ", a2.size_grid, 
                                                             " and \n    ST_Within(a1.geom, a2.geom)\n    UNION\n    SELECT\n    a2.code as input_geographic_identifier,\n    left(a1.code,7) as geographic_identifier_project\n    from\n    area.cwp_grid a1,\n    area.irregular_areas_task2_iotc a2\n    where\n    a2.code IN ( '", 
                                                             area_changeresolution, "') and\n    a1.size_grid=", 
                                                             a1.size_grid, " and \n    ST_Within(a1.geom, a2.geom)"))
  
  
  
  if (nrow(areas_to_project_data_to_disaggregate) > 0) {
    
    query <- "SELECT  code,st_area(geom), geom from area.cwp_grid"
    world_sf <- dbGetQuery(con, query)
    world_sf <- st_make_valid(st_read(con, query = query))%>% filter(!st_is_empty(.))
    world_sf <- world_sf[sf::st_is_valid(world_sf),]
    query <- "SELECT  code,st_area(geom), geom from area.gshhs_world_coastlines"
    continent <- st_read(con, query = query)%>% filter(!st_is_empty(.))
    world_sf$continent <- st_within(world_sf, continent) %>% lengths > 0 
    
    st_geometry(world_sf) <- NULL
    
    areas_to_project_data_to_disaggregate <- inner_join(areas_to_project_data_to_disaggregate %>% 
                                                          mutate(geographic_identifier_project = as.character(geographic_identifier_project)),world_sf,  
                                                        by = c("geographic_identifier_project"="code"))%>% 
      select(geographic_identifier_project, input_geographic_identifier, continent) %>%       filter(continent == FALSE)%>%
      group_by(input_geographic_identifier) %>% mutate(number = n()) 
    rm(world_sf)
    gc()
    dataset_to_disaggregate <- inner_join(dataset_not_to_leave_as_so, areas_to_project_data_to_disaggregate, 
                                          by = c(geographic_identifier = "input_geographic_identifier"))
    dataset_to_aggregate <- anti_join(dataset_not_to_leave_as_so, areas_to_project_data_to_disaggregate, 
                                      by = c(geographic_identifier = "input_geographic_identifier"))
    rm(dataset_not_to_leave_as_so)
    # rm(areas_to_project_data_to_disaggregate)
    gc()
    dataset_to_disaggregate$value <- dataset_to_disaggregate$value/dataset_to_disaggregate$number
    dataset_to_disaggregate <- dataset_to_disaggregate %>% select(-number, -geographic_identifier, -continent)
    dataset_to_disaggregate <- rename(dataset_to_disaggregate, geographic_identifier = geographic_identifier_project)
    dataset_disaggregated <- rbind(dataset_to_aggregate, dataset_to_disaggregate)
    gc()
  } else {
    config$logger.info("No areas to project data to disaggregate so returning initial input")
    return(df_input)}
  if (!is.null(dataset_to_disaggregate)) {
    sum_fact_to_reallocate <- dataset_to_disaggregate %>% 
      group_by(unit) %>% summarise(value_reallocate = sum(value))
    sum_whole_dataset <- georef_dataset %>% group_by(unit) %>% 
      summarise(value = sum(value))
    stats_reallocated_data <- left_join(sum_whole_dataset, 
                                        sum_fact_to_reallocate)
    stats_reallocated_data$percentage_reallocated <- stats_reallocated_data$value_reallocate/stats_reallocated_data$value * 
      100
  }  else {
    stats_reallocated_data = NULL
  }
  rm(georef_dataset)
  rm(dataset_to_disaggregate)
  gc()
  georef_dataset <- list(df = dataset_final_disaggregated_on_resolution_to_disaggregate, 
                          stats = stats_reallocated_data)
  
  
  # georef_dataset<-function_spatial_curation_upgrade_Bastien(con,georef_dataset = georef_dataset,resolution,remove)
  gc()
  config$logger.info("END rtunaatlas::spatial_curation_downgrade_resolution() function")
  georef_dataset<-georef_dataset$df
  
  config$logger.info(sprintf("Disaggregating / Removing data that are defined on quadrants or areas superior to [%s]° quadrant resolution OK", resolution))
  
  return(list(dataset=georef_dataset,lineage=lineage,description=description, stats_reallocated_data = stats_reallocated_data))
  
}
