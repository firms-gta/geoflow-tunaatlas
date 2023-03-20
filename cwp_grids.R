require(ows4R)
require(readr)

WFS_FAO_NFI <- WFSClient$new(
  url = "https://www.fao.org/fishery/geoserver/cwp/wfs",
  serviceVersion = "1.0.0", logger = "INFO"
)


layers <- c("cwp:cwp-grid-map-1deg_x_1deg", "cwp:cwp-grid-map-5deg_x_5deg", "cwp:cwp-grid-map-10deg_x_10deg", "cwp:cwp-grid-map-20deg_x_20deg") 
erased_layers <- paste0(layers, "_erased")

#CWP Grids not erased by continent
cwp_grid <- do.call(rbind, lapply(layers, WFS_FAO_NFI$getFeatures))
readr::write_csv(cwp_grid, "cwp_grid.csv")

#CWP Grids erased by continent
cwp_grid_erased <- do.call(rbind, lapply(erased_layers, WFS_FAO_NFI$getFeatures))
readr::write_csv(cwp_grid, "cwp_grid_erased.csv")

#logic to upload on drive
folder_codelists_id <- drive_get("~/geoflow_tunaatlas/data/inputs/codelists")
drive_upload("fao_cwp_grid.csv", as_id(folder_codelists_id), overwrite = TRUE)
drive_upload("fao_cwp_grid_erased.csv", as_id(folder_codelists_id), overwrite = TRUE)



