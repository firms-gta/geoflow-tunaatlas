#strata_in_georef_but_not_in_nominal_report
copyrmd("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/strata_in_georef_but_no_nominal.Rmd")

nominal <- as.data.frame(read_csv("data/global_nominal_catch_firms_level0.csv") %>% mutate(measurement_unit ="t"))
georef_mapped <- readRDS("Markdown/mapping_codelist/rds.rds")


georef_mapped <- (georef_mapped) %>% dplyr::mutate(year =lubridate::year(time_start)) %>% select("fishing_fleet"  ,   "species"    ,      "measurement_unit"          ,  "gear_type", "source_authority",
                                                                                                                   "year", "measurement_value") %>% mutate(measurement_unit = ifelse(measurement_unit %in% c("MTNO", "MT"),"t",measurement_unit)) %>% filter(measurement_unit == "t")%>% mutate(year = as.character(year))%>% mutate(year = paste0(year, "-01-01")) #%>% group_by(year) %>% summarise(measurement_value = sum(measurement_value))
row.names(georef_mapped) <- NULL
nominal <- (nominal)%>% mutate(year = lubridate::year(time_start)) %>% dplyr::select("fishing_fleet"  ,   "species"    ,      "measurement_unit"          ,  "gear_type",  "source_authority",
                                                                                                       "year","measurement_value")%>% mutate(year = as.character(year))%>% mutate(year = paste0(year, "-01-01"))


# we only keep georef data for which we have an equivalent year in nominal

nominal_year <- unique(nominal$year)
georef_mapped <- georef_mapped %>% filter(year %in% nominal_year)

strata = c("species", "year", "source_authority")

georef_no_nominal <- anti_join(georef_mapped, nominal, by = strata)
georef_no_nominal <- georef_no_nominal %>% mutate(year = as.character(year))%>% mutate(year = paste0(year, "-01-01"))

dir.create("Markdown_nominal/georef_not_nominal")
saveRDS(georef_no_nominal, "Markdown_nominal/georef_not_nominal/rds.rds")

strata = c("species", "year", "source_authority", "gear_type")
georef_no_nominal <- anti_join(georef_mapped, nominal, by = strata)
georef_no_nominal <- georef_no_nominal %>% mutate(year = as.character(year))%>% mutate(year = paste0(year, "-01-01"))

dir.create("Markdown_nominal/georef_not_nominal_gear")
saveRDS(georef_no_nominal, "Markdown_nominal/georef_not_nominal_gear/rds.rds")

dir.create("Markdown_nominal/nominal_catches", recursive = TRUE)
saveRDS(nominal, "Markdown_nominal/nominal_catches/rds.rds")
dir.create("Markdown_nominal/georef_for_nominal", recursive = TRUE)
saveRDS(georef_mapped, "Markdown_nominal/georef_for_nominal/rds.rds")


tryCatch({
  for (i in c( 'IATTC','IOTC', 'ICCAT', 'WCPFC', 'CCSBT')){
    
    entity$data$actions[[1]]$options$filtering <- list(source_authority = c(paste(i)))
    dir.create(paste0("georef_not_nominal_markdown/",i, "/figures"), recursive = TRUE, showWarnings = FALSE)
    dir.create(paste0("georef_not_nominal_markdown_gear/",i, "/figures"), recursive = TRUE, showWarnings = FALSE)
    
    parameters_child_global <- list(action = action,
                                    entity = entity, config = config, debugging = TRUE,
                                    strata = c("species", "year", "source_authority"), 
                                    fig.path = paste0("georef_not_nominal_markdown/",i, "/figures/"), 
                                    parameter_init = "Markdown_nominal/georef_not_nominal", 
                                    georef = "Markdown_nominal/georef_for_nominal",
                                    nominal = "Markdown_nominal/nominal_catches")
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
                                    georef = "Markdown_nominal/georef_for_nominal",
                                    nominal = "Markdown_nominal/nominal_catches")
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
                                parameter_init = "Markdown_nominal/georef_not_nominal_gear", 
                                georef = "Markdown_nominal/georef_for_nominal",
                                nominal = "Markdown_nominal/nominal_catches")
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

