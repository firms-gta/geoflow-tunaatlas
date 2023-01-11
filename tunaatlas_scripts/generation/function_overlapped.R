function_overlapped =function(dataset, con, rfmo_to_keep, rfmo_not_to_keep, 
                              strata =c("geographic_identifier",    "species", "time_start", "time_end",
                                        "unit", opts = opts)){
  variable <- opts$fact
  columns_to_keep <- NULL
  if (variable == "catch"){
    columns_to_keep<-c("source_authority","species","gear","fishingfleet","schooltype","time_start","time_end","geographic_identifier","catchtype","unit","value")
  } else if (variable=="effort"){
    columns_to_keep<-c("source_authority","gear","fishingfleet","schooltype","time_start","time_end","geographic_identifier","unit","value")
  }
  strata <- intersect(strata, columns_to_keep)
  #   overlapping_zone_request <- paste0("SELECT codesource_area,area,wkt from
  # (WITH rfmo_to_keep_area_of_competence AS (
  #   SELECT rfmos_convention_areas_fao.geom
  #   FROM area.rfmos_convention_areas_fao
  #   WHERE code::text = '",rfmo_to_keep,"'::text
  # ), rfmo_not_to_keep_area_of_competence AS (
  #   SELECT rfmos_convention_areas_fao.geom
  #   FROM area.rfmos_convention_areas_fao
  #   WHERE code::text = '",rfmo_not_to_keep, "'::text
  # ), geom_rfmo_to_keep_rfmo_not_to_keep_intersection AS (
  #   SELECT st_collectionextract(st_intersection(rfmo_to_keep_area_of_competence.geom, rfmo_not_to_keep_area_of_competence.geom), 3) AS geom
  #   FROM rfmo_to_keep_area_of_competence,
  #   rfmo_not_to_keep_area_of_competence
  # )
  #   SELECT area_labels.id_area,
  #   area_labels.codesource_area,
  #   area_labels.geom,
  #   st_astext(area_labels.geom) as wkt,
  #   st_area(area_labels.geom) as area
  #   FROM area.area_labels,
  #   geom_rfmo_to_keep_rfmo_not_to_keep_intersection
  #   WHERE area_labels.tablesource_area = 'cwp_grid'::text AND st_within(area_labels.geom, geom_rfmo_to_keep_rfmo_not_to_keep_intersection.geom))tab 
  # order by area desc")
  # 
  # 
  # overlapping_zone <- dbGetQuery(con, overlapping_zone_request)
  
  
  # assign("overlapping_ancient_method", 
  #        dataset[ which(!(dataset$geographic_identifier %in% overlapping_zone$codesource_area & 
  #                           dataset$source_authority == rfmo_not_to_keep)), ], envir = .GlobalEnv)
  # assign("reverse_overlapping", 
  #        dataset[ which(!(dataset$geographic_identifier %in% overlapping_zone$codesource_area & dataset$source_authority == rfmo_to_keep)), ],
  #        envir = .GlobalEnv)
  
  rfmo_to_keep_DT <- dataset %>% filter(source_authority == rfmo_to_keep)
  rfmo_not_to_keep_DT <- dataset %>% filter(source_authority == rfmo_not_to_keep)
  rfmo_restant <- dataset %>% 
    filter(source_authority != rfmo_not_to_keep & source_authority!= rfmo_to_keep)
  
  rfmo_not_to_keep_without_equivalent <- dplyr::anti_join(rfmo_not_to_keep_DT, rfmo_to_keep_DT, 
                                                          by = strata)
  georef_dataset <- rbind(rfmo_restant, rfmo_not_to_keep_without_equivalent, rfmo_to_keep_DT)
  rm(rfmo_to_keep_DT, rfmo_not_to_keep_DT, rfmo_restant, rfmo_not_to_keep_without_equivalent)
  gc()
  
  # test <- dataset %>%
  #   ungroup() %>%
  #   group_by(source_authority, gear, fishingfleet,geographic_identifier,species, time_start, time_end, unit) %>%
  #   summarise(value = sum(value, na.rm = TRUE)) %>% ungroup()
  
  # test2 <- test %>%  select( geographic_identifier,species, time_start, time_end, unit,gear, fishingfleet) %>%
  #   arrange(desc(value)) %>% slice(1)
  
  
  # if (nrow(test)!= nrow(dataset)){
  #   georef_dataset <- dataset %>%
  #     ungroup() %>% 
  #     group_by( geographic_identifier,species, time_start, time_end, unit,gear, fishingfleet) %>%
  #     arrange(desc(value)) %>% slice(1)
  #     } else { georef_dataset <- dataset %>% ungroup()}
  
  # georef_dataset <- dataset %>%
  #   group_by(across(c(-source_authority,-value))) %>%
  #   arrange(desc(value)) %>%
  #   filter(row_number() ==1)
  
  
  georef_dataset %>% ungroup()
}