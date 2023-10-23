function_outside_juridiction = function(georef_dataset, con){

colnames_final <- colnames(georef_dataset)
query <- "SELECT * FROM area.cwp_grid_by_tRFMOs"
cwp_trfmo <- dbGetQuery(con, query)
# Convert data.frames to data.tables

outside_juridiction <- georef_dataset %>% dplyr::anti_join(cwp_trfmo,
    by = c("geographic_identifier" = "code", "source_authority" = "RFB")) %>% 
  filter(source_authority != "CCSBT") # for now as CCSBT does not have a area

# georef_dataset <- georef_dataset %>% dplyr::inner_join(cwp_trfmo,
#   by = c("geographic_identifier" = "code", "source_authority" = "RFB")) %>% 
#   select(-row.names)

# merged_data <- merge(georef_dataset, cwp_trfmo, by.x = "geographic_identifier", by.y = "code", all.x = TRUE)
# merged_data$match_found <- with(merged_data, ifelse(!is.na(RFB) & source_authority %in% RFB, 1, 0))
# 
# georef_dataset <- merged_data %>% 
#   dplyr::filter(match_found ==1) %>% 
#   dplyr::select(colnames_final)
# outside_juridiction <- merged_data %>% 
#   dplyr::filter(match_found ==0) %>% 
#   dplyr::select(colnames_final)

if(nrow(outside_juridiction) ==0){
  return(list(georef_dataset = georef_dataset,
  outside_juridiction = NULL))
}

return(list(georef_dataset = georef_dataset,
       outside_juridiction = outside_juridiction))
}

