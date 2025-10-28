spatial_curation =function (con, df_input, remove_reallocate){

  tryCatch({
    # Votre requête DBI::dbGetQuery
    cwp_grid <- DBI::dbGetQuery(con, "SELECT ON_LAND_P,cwp_code from area.cwp_grid") %>%
      dplyr::rename(geographic_identifier = cwp_code)
  }, error = function(e) {
    message("no connexion to DB trying to unzip file")
    csv_file <- here::here("data/cl_areal_grid.csv")
    if(!file.exists(csv_file)){
      message("no connexion to DB nor zip, downloading cwp_grid")
      
      # Téléchargement du fichier ZIP depuis l'URL
      zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
      local_file <- here::here("data", "cwp_grid.zip")
      download.file(zip_url, local_file, mode = "wb")
      
      # Extraction du contenu du fichier ZIP
      unzip(local_file, exdir = here::here("data"))
      
      # Lecture du fichier CSV extrait
      
    } 
    cwp_grid <- read.csv(csv_file)
    
    # Renommage de colonnes
    cwp_grid <- cwp_grid %>% dplyr::select(ON_LAND_P, CWP_CODE) %>% 
      dplyr::rename(geographic_identifier = CWP_CODE)
  })

df_input_cwp_grid <- df_input %>% dplyr::inner_join(cwp_grid)
  
not_cwp_grid <- df_input %>% dplyr::anti_join(cwp_grid)

if(remove_reallocate == "remove"){
  
  if (!is.null(df_input_cwp_grid)) {
    sum_fact_to_reallocate <- df_input_cwp_grid %>% 
      dplyr::group_by(measurement_unit) %>% dplyr::summarise(value_reallocate = sum(measurement_value))
    sum_whole_df_input <- df_input %>% dplyr::group_by(measurement_unit) %>% 
      dplyr::summarise(measurement_value = sum(measurement_value))
    stats_reallocated_data <- dplyr::left_join(sum_whole_df_input, 
                                        sum_fact_to_reallocate)
    stats_reallocated_data$percentage_reallocated <- stats_reallocated_data$value_reallocate/stats_reallocated_data$measurement_value * 
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
    dplyr::filter(geographic_identifier %in% areas_to_curate)
  areas_to_project_data_not_cwp <- areas_to_project_data_not_cwp %>% dplyr::group_by(input_geographic_identifier) %>% dplyr::slice(1)
  `%notin%` <- Negate(`%in%`)
  df_input_areas_not_curated <- not_cwp_grid %>% 
    dplyr::filter(geographic_identifier %notin% areas_to_curate)
}

df_input_curated <- df_input_areas_to_curate %>% dplyr::inner_join(areas_to_project_data_not_cwp, by = c("geographic_identifier" = "input_geographic_identifier"))
df_input_not_curated <- df_input_areas_to_curate %>% dplyr::left_join(areas_to_project_data_not_cwp, by = c("geographic_identifier" = "input_geographic_identifier")) 

df_input_curated <- df_input_curated %>% dplyr::group_by_(.dots = setdiff(c(columns_df_input, 
                                                                               "geographic_identifier_project"), c("geographic_identifier", 
                                                                                                                   "measurement_value"))) %>% summarise(measurement_value = sum(measurement_value))
df_input_curated$geographic_identifier <- df_input_curated$geographic_identifier_project
df_input_curated$geographic_identifier_project <- NULL


df_input_final_curated <- rbind(data.frame(df_input_cwp_grid),
                                                              data.frame(df_input_curated)#, data.frame(df_input_areas_sup_to_resolution_to_curate)
)
if (!is.null(df_input_final_curated)) {
  sum_fact_to_reallocate <- df_input_final_curated %>% 
    dplyr::group_by(measurement_unit) %>% dplyr::summarise(value_reallocate = sum(measurement_value))
  sum_whole_df_input <- df_input %>% dplyr::group_by(measurement_unit) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value))
  stats_reallocated_data <- dplyr::left_join(sum_whole_df_input, 
                                      sum_fact_to_reallocate)
  stats_reallocated_data$percentage_reallocated <- stats_reallocated_data$value_reallocate/stats_reallocated_data$measurement_value * 
    100
}
else {
  stats_reallocated_data = NULL
}
return(list(df = df_input_final_curated_on_resolution_to_curate, df_input_areas_not_curated = df_input_areas_not_curated,
            stats = stats_reallocated_data))

  
   }

}














