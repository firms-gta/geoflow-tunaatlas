strata_in_georef_but_not_in_nominal_report_launching = function(main.dir, connectionDB ){
  ancient_wd <- getwd()
  setwd(main.dir)
  path = getwd()
  if(file.exists(file.path(main.dir, "entities/global_nominal_catch_firms/data/global_nominal_catch_firms_harmonized.csv"))){
#strata_in_georef_but_not_in_nominal_report

nominal <- as.data.frame(read_csv(file.path(main.dir,"entities/global_nominal_catch_firms/data/global_nominal_catch_firms_harmonized.csv")) %>% 
                           mutate(measurement_unit ="Tons")) %>% 
  dplyr::mutate(gear_type = as.numeric(gear_type)) %>% 
  dplyr::mutate(gear_type = as.character(gear_type))
try(georef_mapped <- readRDS(file.path(main.dir,"entities/global_catch_firms_level0/Markdown/rawdata/rds.rds")))
try(georef_mapped <- readRDS(file.path(main.dir,"entities/global_catch_firms_level0_/Markdown/rawdata/rds.rds")))

georef_mapped <- georef_mapped %>% dplyr::mutate(year =lubridate::year(time_start)) %>% 
  dplyr::select("fishing_fleet"  ,"geographic_identifier",   "species"    ,      "measurement_unit"          ,  "gear_type", "source_authority",
  "year", "measurement_value") %>%
  dplyr::filter(measurement_unit %in% c("t", "Tons"))%>%
  dplyr::mutate(year = as.character(year))%>%
  dplyr::mutate(year = paste0(year, "-01-01")) 

row.names(georef_mapped) <- NULL
nominal <- (nominal)%>% mutate(year = lubridate::year(time_start)) %>% 
  dplyr::select("fishing_fleet"  ,   "species"    ,      "measurement_unit"          ,  "gear_type",  "source_authority",
  "year","measurement_value")%>% 
  mutate(year = as.character(year))%>% 
  mutate(year = paste0(year, "-01-01"))

species_group <-  st_read(connectionDB,query = "SELECT taxa_order, code from species.species_asfis") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
cl_cwp_gear_level2 <- st_read(connectionDB, query = "SELECT * FROM gear_type.isscfg_revision_1")%>% select(Code = code, Gear = label)

shapefile.fix <- st_read(connectionDB,query = "SELECT * from area.cwp_grid") %>% 
  dplyr::rename(GRIDTYPE = gridtype)

shape_without_geom  <- shapefile.fix %>% as_tibble() %>%dplyr::select(-geom)

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/tidying_GTA_data_for_comparison.R")


# we only keep georef data for which we have an equivalent year in nominal

nominal_year <- unique(nominal$year)
georef_mapped <- georef_mapped %>% filter(year %in% nominal_year) %>% 
  dplyr::group_by(across(setdiff(everything(), "measurement_value"))) %>% 
  mutate(measurement_value = sum(measurement_value))
list_strata <- list(c("species", "year", "source_authority"), 
  c("species", "year", "source_authority", "gear_type"))

for (strata in list_strata){
name <- paste0(toString(strata))


georef_no_nominal <- anti_join(georef_mapped, nominal, by = strata)
georef_no_nominal <- georef_no_nominal %>% mutate(year = as.character(year))

georef_no_nominal_groupped <- georef_no_nominal %>% 
  dplyr::group_by(across(setdiff(everything(), "measurement_value"))) %>% 
  mutate(measurement_value = sum(measurement_value))

georef_mapped_groupped <- georef_mapped %>% group_by_at(strata) %>%
  summarise(measurement_value = sum(measurement_value)) 

nominal_groupped <- nominal %>% group_by_at(strata) %>%
  summarise(measurement_value = sum(measurement_value))


georef_sup_nominal <- inner_join(nominal_groupped, georef_mapped_groupped, by = strata) %>% 
  dplyr::rename(nominal_data = measurement_value.x, georefed_data = measurement_value.y) %>% 
  mutate(nominal_data = round(nominal_data, 3)) %>% 
  mutate(georefed_data = round(georefed_data, 3)) %>% 
  filter(georefed_data > nominal_data)%>% 
  mutate(Difference = georefed_data - nominal_data) %>% filter(Difference >= 1) %>% 
  dplyr::mutate(measurement_unit = "Tons") %>% 
  dplyr::mutate(measurement_value = Difference)

# georef_sup_nominal <- georef_sup_nominal%>% group_by_at(strata) %>% 
#   summarise(measurement_value = sum(measurement_value))

georef_sup_to_nom_all <- tidying_GTA_data_for_comparison(dataframe = georef_sup_nominal,
                                             shape = shape_without_geom, 
                                             species_group_dataframe = species_group,
                                             cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2)
georef_no_nominal_groupped <- georef_no_nominal_groupped %>% dplyr::mutate(geographic_identifier = as.character(geographic_identifier))
georef_no_nominal_groupped_all <- tidying_GTA_data_for_comparison(dataframe = georef_no_nominal_groupped,
                                                      shape = shape_without_geom, 
                                                      species_group_dataframe = species_group,
                                                      cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2)


concerned_trfmos <- unique(c(unique(georef_no_nominal$source_authority),unique(georef_sup_nominal$source_authority)))
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/copy_project_files.R", local = TRUE)
# 
copy_project_files(original_repo_path = here::here("Analysis_markdown/"), new_repo_path = getwd())

tryCatch({
  for (i in concerned_trfmos){
    
    dir.create(file.path(getwd(),"georef_not_nominal_markdown/",i, "/", name, "/figures/georef_sup_nominal"),recursive = TRUE )
    georef_no_nominal_groupped <- georef_no_nominal_groupped_all %>% dplyr::filter(source_authority == i)
    georef_sup_to_nom <- georef_sup_to_nom_all %>% dplyr::filter(source_authority == i)
    nominal_groupped <- nominal_groupped %>% dplyr::filter(source_authority == i)
    if(nrow(georef_sup_to_nom) != 0 | nrow(georef_no_nominal_groupped)!= 0){
    parameters_child_global <- list(strata = strata, 
                                    fig.path = paste0("georef_not_nominal_markdown/",i, "/figures/"), 
                                    parameter_init = georef_no_nominal_groupped, 
                                    nominal_groupped = nominal_groupped,
                                    parameter_colnames_to_keep = c("fishing_fleet", "gear_type", "geographic_identifier",
                                                                   "fishing_mode", "species", "measurement_unit", "measurement_value", 
                                                                   "Gear", "species_group", "GRIDTYPE"),
                                    georef_sup_to_nom = georef_sup_to_nom, 
                                    shapefile.fix = shapefile.fix, 
                                    parameter_geographical_dimension = "geographic_identifier", 
                                    parameter_geographical_dimension_groupping = "GRIDTYPE")
    
    
    child_env_global = new.env()
    
    list2env(parameters_child_global, env = child_env_global)
    rmarkdown::render("strata_in_georef_but_no_nominal.Rmd",
                      envir = child_env_global,
                      output_file = "strata_in_georef_but_no_nominal.html", 
                      output_dir = file.path("georef_not_nominal_markdown/",i, "/", name))    
    
    }
  }
}, error = function(e) {
  # Handle the error, e.g., print an error message
  message("An error occurred: ", conditionMessage(e))
})


}

georef_no_nominal_all <- anti_join(georef_mapped, nominal, by = c("species", "year", "source_authority")) %>% 
  filter(measurement_value > 0)
georef_no_nominal_all <- georef_no_nominal_all %>%
  dplyr::mutate(year = as.character(year)) %>% 
  ungroup() %>% 
  dplyr::select(-geographic_identifier)

# Define directories
source_directory <- file.path(getwd(),"georef_not_nominal_markdown")
target_directory <- source_directory

# List all HTML files
files <- list.files(source_directory, recursive = TRUE, full.names = TRUE, pattern = "\\.html$")

# Function to create a new name based on the directory path
create_new_name <- function(file_path) {
  parts <- unlist(strsplit(file_path, "/"))
  new_name <- paste(paste(parts[length(parts)-2],paste(parts[length(parts)-1], parts[length(parts)], sep = "_")))
  return(new_name)
}

# Copy and rename files
for (file in files) {
  new_name <- create_new_name(file)
  file.copy(file, file.path(target_directory, new_name))
}

upgradded_nominal <- rbind(nominal,georef_no_nominal_all)
setwd(ancient_wd)
return(upgradded_nominal)
  }
  
  
}
