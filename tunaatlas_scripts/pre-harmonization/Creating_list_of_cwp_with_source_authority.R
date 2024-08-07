url= "https://www.fao.org/fishery/geoserver/wfs" 
serviceVersion = "1.0.0" 
logger = "INFO"
# SOURCE: OGC ####
WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/wfs", serviceVersion = "1.0.0", logger = "INFO")



# Load the files
CCSBT_SF = WFS$getFeatures("rfb:RFB_CCSBT") 
ICCAT_SF = WFS$getFeatures("rfb:RFB_ICCAT")
IATTC_SF = WFS$getFeatures("rfb:RFB_IATTC")
IOTC_SF  = WFS$getFeatures("rfb:RFB_IOTC")
WCPFC_SF = WFS$getFeatures("rfb:RFB_WCPFC")

shapefile.fix <- rbindlist(list(ICCAT_SF, IATTC_SF, IOTC_SF, WCPFC_SF))

# Ensure both datasets are simple features
query <- "SELECT code, geom from area.cwp_grid"
cwp_grid <- st_read(con, query = query)

shapefile.fix <- st_as_sf(shapefile.fix)

# Identify which geometries from dataset1 are within shapefile.fix
within_idx <- st_within(cwp_grid, shapefile.fix)
intersects_idx <- st_intersects(cwp_grid, shapefile.fix)
combined_idx <- lapply(1:length(cwp_grid$geom), function(i) {
  unique(c(unlist(intersects_idx[i]), unlist(within_idx[i])))
})

results <- lapply(1:length(combined_idx), function(i) {
  idx <- unlist(combined_idx[i])
  
  cwp_val <- ifelse(length(cwp_grid$code) >= i, cwp_grid$code[i], NA)
  
  # If idx is empty, return a data frame with NA for RFB
  if(length(idx) == 0) {
    return(data.frame(code = cwp_val, RFB = NA))
  } else {
    if(is.na(cwp_val) || cwp_val == "") {
      cwp_val <- NA
    }
    return(data.frame(code = rep(cwp_val, length(idx)), RFB = shapefile.fix$RFB[idx]))
  }
})
final_result <- do.call(rbind, results)

dbExecute(con, "DROP TABLE IF EXISTS temp_table_for_view CASCADE")

# First, write the final_result as a temporary table
dbWriteTable(con, "temp_table_for_view", final_result, overwrite = TRUE, temporary = TRUE)

# Now, let's create a materialized view based on this temporary table
sql <- "
CREATE MATERIALIZED VIEW area.cwp_grid_by_tRFMOs AS
SELECT * FROM temp_table_for_view;
"

dbExecute(con, sql)
