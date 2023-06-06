identification_data_on_land_cwp = function(con, df_input){
  
  cwp_grid <- DBI::dbGetQuery(con, "SELECT ON_LAND_P,cwp_code from area.cwp_grid") %>% dplyr::rename(geographic_identifier = cwp_code)

  # Merge 5x5 catch data with grid
  CA_WITH_GRIDS = merge(df_input, cwp_grid, by = "geographic_identifier")
  CA_WITH_GRIDS$on_land_p <- as.numeric(CA_WITH_GRIDS$on_land_p)
  # Identify catch data in grids on land
  CA_ON_LAND = CA_WITH_GRIDS %>% dplyr::filter(on_land_p ==100)

  return(areas_in_land =unique(CA_ON_LAND$geographic_identifier))
}
