spatial_curation =function (con, df_input, remove_reallocate){

cwp_grid <- dbGetQuery(con, "SELECT cwp_code from area.cwp_grid") %>% rename(geographic_identifier = cwp_code)

df_input_cwp_grid <- df_input %>% inner_join(cwp_grid)
  
not_cwp_grid <- df_input %>% anti_join(cwp_grid)

if(remove_reallocate == "remove"){
  
  if (!is.null(df_input_cwp_grid)) {
    sum_fact_to_reallocate <- df_input_cwp_grid %>% 
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
  return(list(df = df_input_cwp_grid, not_cwp_grid = not_cwp_grid,stats = stats_reallocated_data))
} else {

area_changeresolution <- setdiff(unique(df_input$geographic_identifier), 
                                 unique(cwp_grid_data_with_resolution_as_required$geographic_identifier))
area_changeresolution <- paste(unique(area_changeresolution), 
                               collapse = "','")
areas_to_project_data_not_cwp <- dbGetQuery(con, 
                                                    paste0("SELECT\n    left(a2.code,7) as input_geographic_identifier,\n    left(a1.code,7) as geographic_identifier_project\n    from\n    area.cwp_grid a1,\n    area.cwp_grid a2\n    where\n    a2.code IN ( '", 
                                                           area_changeresolution, "') and\n   LEFT(CAST(a1.code AS TEXT), 1) ='6' and LEFT(CAST(a2.code AS TEXT), 1) IN ('1','2','7','8','9') and \n    ST_Within(a1.geom, a2.geom)\n    UNION\n    SELECT\n    a2.code as input_geographic_identifier,\n    left(a1.code,7) as geographic_identifier_project\n    from\n    area.cwp_grid a1,\n    area.irregular_areas_task2_iotc a2\n    where\n    a2.code IN ( '", 
                                                           area_changeresolution, "') and\n    LEFT(CAST(a1.code AS TEXT), 1)='6' and \n    ST_Within(a1.geom, a2.geom)")) %>% distinct()

if (nrow(areas_to_project_data_not_cwp) > 0) {
  areas_to_curate <- unique(areas_to_project_data_not_cwp$input_geographic_identifier)
  df_input_areas_to_curate <- not_cwp_grid %>%
    filter(geographic_identifier %in% areas_to_curate)
  areas_to_project_data_not_cwp <- areas_to_project_data_not_cwp %>% group_by(input_geographic_identifier) %>% slice(1)
  `%notin%` <- Negate(`%in%`)
  df_input_areas_not_curated <- not_cwp_grid %>% 
    filter(geographic_identifier %notin% areas_to_curate)
}

df_input_curated <- df_input_areas_to_curate %>% inner_join(areas_to_project_data_not_cwp, by = c("geographic_identifier" = "input_geographic_identifier"))
df_input_not_curated <- df_input_areas_to_curate %>% left_join(areas_to_project_data_not_cwp, by = c("geographic_identifier" = "input_geographic_identifier")) 

df_input_curated <- df_input_curated %>% group_by_(.dots = setdiff(c(columns_df_input, 
                                                                               "geographic_identifier_project"), c("geographic_identifier", 
                                                                                                                   "value"))) %>% summarise(value = sum(value))
df_input_curated$geographic_identifier <- df_input_curated$geographic_identifier_project
df_input_curated$geographic_identifier_project <- NULL


df_input_final_curated <- rbind(data.frame(df_input_cwp_grid),
                                                              data.frame(df_input_curated)#, data.frame(df_input_areas_sup_to_resolution_to_curate)
)
if (!is.null(df_input_final_curated)) {
  sum_fact_to_reallocate <- df_input_final_curated %>% 
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
return(list(df = df_input_final_curated_on_resolution_to_curate, df_input_areas_not_curated = df_input_areas_not_curated,
            stats = stats_reallocated_data))

  
   }

}














