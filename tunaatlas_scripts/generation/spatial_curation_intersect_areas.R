spatial_curation_intersect_areas<-function (con, df_input, df_spatial_code_list_name, intersection_spatial_code_list_name) 
{
  cat(paste0("Please ignore here-under warning messages 'unrecognized PostgreSQL field type unknown'"))
  inputAreas_forQuery <- paste(unique(df_input$geographic_identifier), 
                               collapse = "','")
  db_table_name_inputAreas <- dbGetQuery(con, paste0("SELECT identifier from metadata.metadata where identifier='", 
                                                     df_spatial_code_list_name, "'"))$table_name
  db_table_name_intersectionArea <- dbGetQuery(con, paste0("SELECT identifier from metadata.metadata where identifier='", 
                                                           intersection_spatial_code_list_name, "'"))$table_name
  query_data_inland <- paste("WITH \n                           source_layer AS (\n                           SELECT code, label, geom FROM area.", 
                             df_spatial_code_list_name, " WHERE code IN ('", inputAreas_forQuery, 
                             "')\n                           ),intersection_layer\n                           AS (\n                           SELECT code, label, geom FROM public.", 
                             intersection_spatial_code_list_name, "\n                           )\n                           SELECT \n                           source_layer.code as geographic_identifier_source_layer,\n                           intersection_layer.code as geographic_identifier_intersection_layer,\n                           '", 
                             df_spatial_code_list_name, "' as codelist_source_layer,\n                           '", 
                             intersection_spatial_code_list_name, "' as codelist_intersection_layer,\n                           ST_Area(ST_Intersection(source_layer.geom, intersection_layer.geom))/ST_Area(source_layer.geom) as proportion_source_area_intersection\n                           FROM \n                           source_layer,intersection_layer\n                           WHERE\n                           ST_Intersects(source_layer.geom, intersection_layer.geom)", 
                             sep = "")
  areas_intersected <- dbGetQuery(con, query_data_inland)
  areas_intersected$geographic_identifier_source_layer <- gsub(" ", 
                                                               "", areas_intersected$geographic_identifier_source_layer, 
                                                               fixed = TRUE)
  df_input <- left_join(df_input, areas_intersected, by = c(geographic_identifier = "geographic_identifier_source_layer"))
  df_input$codelist_source_layer <- NULL
  df_input$codelist_intersection_layer <- NULL
  df_input$proportion_source_area_intersection[which(is.na(df_input$proportion_source_area_intersection))] <- 0
  areas_not_intersected <- setdiff(df_input$geographic_identifier, 
                                   unique(areas_intersected$geographic_identifier_source_layer))
  if (!identical(areas_not_intersected, character(0))) {
    areas_not_intersected <- data.frame(areas_not_intersected)
    colnames(areas_not_intersected) <- "geographic_identifier_source_layer"
    areas_not_intersected$geographic_identifier_source_layer <- as.character(areas_not_intersected$geographic_identifier_source_layer)
    areas_not_intersected$geographic_identifier_intersection_layer <- NA
    areas_not_intersected$codelist_intersection_layer <- intersection_spatial_code_list_name
    areas_not_intersected$proportion_source_area_intersection <- 0
    areas_not_intersected$codelist_source_layer <- df_spatial_code_list_name
  }
  else {
    areas_not_intersected = NULL
  }
  df_input_areas_intersect_intersection_layer <- rbind(areas_intersected, 
                                                       areas_not_intersected)
  df_input_areas_intersect_intersection_layer$geographic_identifier_intersection_layer[which(df_input_areas_intersect_intersection_layer$proportion_intersection == 
                                                                                               0)] <- NA
  return(list(df = df_input, df_input_areas_intersect_intersection_layer = df_input_areas_intersect_intersection_layer))
}
