shapefile.fix <- st_read("data/world_sf.csv") %>% dplyr::select(CWP_CODE, GRIDTYPE, SURFACE) %>% dplyr::rename(geom = geometry)


shape_without_geom  <- shapefile.fix %>% as_tibble() %>%dplyr::select(-geom)

continent <- st_read("data/continent.csv") %>% dplyr::rename(geom = geometry)
