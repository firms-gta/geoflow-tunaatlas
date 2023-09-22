
species_group <-  read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
cl_cwp_gear_level2 <- read_csv(file.path("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_isscfg_pilot_gear.csv")) %>% select(Code = code, Gear = label)
