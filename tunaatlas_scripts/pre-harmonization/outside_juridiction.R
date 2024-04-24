function_outside_juridiction = function(georef_dataset, con){

colnames_final <- colnames(georef_dataset)
query <- "SELECT * FROM area.cwp_grid_by_tRFMOs"
cwp_trfmo <- dbGetQuery(con, query)
# Convert data.frames to data.tables

outside_juridiction <- georef_dataset %>% dplyr::anti_join(cwp_trfmo,
    by = c("geographic_identifier" = "code", "source_authority" = "RFB")) %>% 
  filter(source_authority != "CCSBT") # for now as CCSBT does not have a area

if(nrow(outside_juridiction) ==0){
  return(list(georef_dataset = georef_dataset,
  outside_juridiction = NULL))
}

return(list(georef_dataset = georef_dataset,
       outside_juridiction = outside_juridiction))
}

