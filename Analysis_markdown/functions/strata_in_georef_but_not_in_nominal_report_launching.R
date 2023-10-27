if(file.exists(file.path(file.path, "entities/global_nominal_catch_firms/data/global_nominal_catch_firms_level0.csv"))){
#strata_in_georef_but_not_in_nominal_report
copyrmd("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/strata_in_georef_but_no_nominal.Rmd")

nominal <- as.data.frame(read_csv(file.path(file.path,"entities/global_nominal_catch_firms/data/global_nominal_catch_firms_level0.csv")) %>% mutate(measurement_unit ="t"))
try(georef_mapped <- readRDS(file.path(file.path,"entities/global_catch_firms_level0/Markdown/mapping_codelist/rds.rds")))
try(georef_mapped <- readRDS(file.path(file.path,"entities/global_catch_firms_level0_/Markdown/mapping_codelist/rds.rds")))


georef_mapped <- georef_mapped %>% dplyr::mutate(year =lubridate::year(time_start)) %>% select("fishing_fleet"  ,   "species"    ,      "measurement_unit"          ,  "gear_type", "source_authority",
                                                                                                                   "year", "measurement_value") %>% dplyr::filter(measurement_unit == "t")%>% mutate(year = as.character(year))%>% mutate(year = paste0(year, "-01-01")) 
row.names(georef_mapped) <- NULL
nominal <- (nominal)%>% mutate(year = lubridate::year(time_start)) %>% dplyr::select("fishing_fleet"  ,   "species"    ,      "measurement_unit"          ,  "gear_type",  "source_authority",
                                                                                                       "year","measurement_value")%>% mutate(year = as.character(year))%>% mutate(year = paste0(year, "-01-01"))


# we only keep georef data for which we have an equivalent year in nominal

nominal_year <- unique(nominal$year)
georef_mapped <- georef_mapped %>% filter(year %in% nominal_year)

list_strata <- list(c("species", "year", "source_authority"), 
  c("species", "year", "source_authority", "gear_type"))

for (strata in list_strata){

georef_no_nominal <- anti_join(georef_mapped, nominal, by = strata)
georef_no_nominal <- georef_no_nominal %>% mutate(year = as.character(year))%>% mutate(year = paste0(year, "-01-01"))

georef_no_nominal_groupped <- georef_no_nominal %>% group_by(strata) %>% 
  summarise(measurement_value = sum(measurement_value))

georef_sup_nominal <- full_join(georef_mapped, nominal, by = strata) %>% 
  mutate(measurement_value.x = round(measurement_value.x, 3)) %>% 
  mutate(measurement_value.y = round(measurement_value.y, 3)) %>% 
  filter(measurement_value.x > measurement_value.y)%>% 
  mutate(Difference = measurement_value_georef - measurement_value_nominal) %>% filter(Difference >= 1)

georef_sup_nominal <- georef_sup_nominal%>% group_by(strata) %>% 
  summarise(measurement_value = sum(measurement_value))

concerned_trfmos <- unique(georef_no_nominal$source_authority)

tryCatch({
  for (i in concerned_trfmos){
    
    dir.create(paste0("georef_not_nominal_markdown/",i, "/figures"), recursive = TRUE, showWarnings = FALSE)
    dir.create(paste0("georef_not_nominal_markdown_gear/",i, "/figures"), recursive = TRUE, showWarnings = FALSE)
    
    parameters_child_global <- list(action = action,
                                    entity = entity, config = config, debugging = TRUE,
                                    strata = c("species", "year", "source_authority"), 
                                    fig.path = paste0("georef_not_nominal_markdown/",i, "/figures/"), 
                                    parameter_init = "Markdown_nominal/georef_not_nominal", 
                                    parameter_colnames_to_keep = c("fishing_fleet", "gear_type", "geographic_identifier",
                                                                   "fishing_mode", "species", "measurement_unit", "measurement_value", 
                                                                   "Gear", "species_group", "GRIDTYPE"),
                                    georef = "Markdown_nominal/georef_for_nominal/rds.rds",
                                    nominal = "Markdown_nominal/nominal_catches/rds.rds")
    child_env_global = new.env()
    list2env(parameters_child_global, env = child_env_global)
    rmarkdown::render("strata_in_georef_but_no_nominal.Rmd",
                      envir = child_env_global,
                      output_file = "strata_in_georef_but_no_nominal.html", 
                      output_dir = paste0("georef_not_nominal_markdown/",i))
    
    
    
    parameters_child_global <- list(action = action,
                                    entity = entity, config = config, debugging = TRUE,
                                    strata = c("species", "year", "source_authority", "gear_type"), 
                                    fig.path = paste0("georef_not_nominal_markdown_gear/",i, "/figures/"), 
                                    parameter_init = "Markdown_nominal/georef_not_nominal_gear", 
                                    georef = "Markdown_nominal/georef_for_nominal/rds.rds",
                                    nominal = "Markdown_nominal/nominal_catches/rds.rds")
    child_env_global = new.env()
    list2env(parameters_child_global, env = child_env_global)
    rmarkdown::render("strata_in_georef_but_no_nominal.Rmd",
                      envir = child_env_global,
                      output_file = "strata_in_georef_but_no_nominal_gear_type.html", 
                      output_dir = paste0("georef_not_nominal_markdown_gear/",i))
    
    dir.create(paste0("Comp_georef_to_nominal/",i, "/figures"), recursive = TRUE, showWarnings = FALSE)
    rmarkdown::render("Comparison_georef_to_nominal.Rmd",
                      envir = child_env_global,
                      output_file = "Comp_georef_to_nominal", 
                      output_dir = paste0("Comp_georef_to_nominal/",i))
    
    
    
  }
}, error = function(e) {
  # Handle the error, e.g., print an error message
  message("An error occurred: ", conditionMessage(e))
})

dir.create(paste0("georef_not_nominal_markdown/all/figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0("georef_not_nominal_markdown_gear/all/figures"), recursive = TRUE, showWarnings = FALSE)

entity$data$actions[[1]]$options$filtering <- NULL
parameters_child_global <- list(action = action,
                                entity = entity, config = config, debugging = TRUE,
                                strata = c("species", "year", "source_authority", "gear_type"), 
                                fig.path = paste0("georef_not_nominal_markdown_gear/all/figures/"), 
                                parameter_init = "Markdown_nominal/georef_not_nominal_gear/rds.rds", 
                                georef = "Markdown_nominal/georef_for_nominal/rds.rds",
                                nominal = "Markdown_nominal/nominal_catches/rds.rds")
child_env_global = new.env()
list2env(parameters_child_global, env = child_env_global)
rmarkdown::render("strata_in_georef_but_no_nominal.Rmd",
                  envir = child_env_global,
                  output_file = "strata_in_georef_but_no_nominal_gear_type.html", 
                  output_dir = paste0("georef_not_nominal_markdown_gear/all/"))

entity$data$actions[[1]]$options$filtering <- NULL
parameters_child_global <- list(action = action,
                                entity = entity, config = config, debugging = TRUE,
                                strata = c("species", "year", "source_authority"), 
                                fig.path = paste0("georef_not_nominal_markdown/all/figures/"), 
                                parameter_init = "Markdown_nominal/georef_not_nominal", 
                                georef = "Markdown_nominal/georef_for_nominal",
                                nominal = "Markdown_nominal/nominal_catches")
child_env_global = new.env()
list2env(parameters_child_global, env = child_env_global)
rmarkdown::render("strata_in_georef_but_no_nominal.Rmd",
                  envir = child_env_global,
                  output_file = "strata_in_georef_but_no_nominal_gear_type.html", 
                  output_dir = paste0("georef_not_nominal_markdown/all"))

}
}