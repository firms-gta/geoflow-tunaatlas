parameter_colnames_to_keep_fact = c("source_authority", "fishing_mode_label", "geographic_identifier","fishing_fleet_label","gear_type_label",
                                    "measurement_unit", "measurement_value", "gridtype","species_group", "species_label", "measurement_type")

cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
if (!file.exists(cwp_grid_file)) {
  stop("cl_areal_grid.csv not found in inst/extdata - run data-raw/download_codelists.R")
}
shp_raw <- sf::st_read(cwp_grid_file, show_col_types = FALSE)
shapefile.fix <- sf::st_as_sf(shp_raw, wkt = "geom_wkt", crs = 4326)
shapefile.fix <- dplyr::rename(shapefile.fix,
                               cwp_code = CWP_CODE,
                               geom     = geom_wkt)
continent <- tryCatch({
  
  st_read(connectionDB, query = "SELECT * FROM public.continent")
}, error = function(e) {
  
  futile.logger::flog.error("An error occurred while reading continent data: %s", e$message)
  
  NULL
  
})

if (is.null(continent)) {
  
  futile.logger::flog.warn("Continent data not found in the database. Fetching from WFS service.")
  
  url <- "https://www.fao.org/fishery/geoserver/wfs"
  
  serviceVersion <- "1.0.0"
  
  logger <- "INFO"
  
  WFS <- WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
  
  continent <- WFS$getFeatures("fifao:UN_CONTINENT2")
  
  futile.logger::flog.info("Fetched continent data from WFS service")
  
}
child_env_first_to_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
  parameter_init ="~/firms-gta/geoflow-tunaatlas/jobs/20260313045040/entities/global_catch_ird_level2_1950_2024/Markdown/Conv_NO_nominal2/data.qs",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20260313045040/entities/global_catch_ird_level2_1950_2024/Markdown/RF_pass_2/data.qs",
  parameter_fact = "catch",
  parameter_colnames_to_keep = c(parameter_colnames_to_keep_fact, "species", "gear_type", "fishing_fleet"),
  shapefile_fix = shapefile.fix,
  continent = continent,
  coverage = TRUE,
  parameter_titre_dataset_1 =  "Final",
  parameter_titre_dataset_2 = "Conv",
  parameter_geographical_dimension_groupping = "gridtype",
  unique_analyse = FALSE,
  topnumber = 50
)



child_env_first_to_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
  parameter_init ="~/firms-gta/geoflow-tunaatlas/jobs/20260311191047level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/Conv_NO_nominal12/ancient.qs",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20260311191047level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/RF_pass_12/ancient.qs",
  parameter_fact = "catch",
  parameter_colnames_to_keep = c(parameter_colnames_to_keep_fact, "species", "gear_type", "fishing_fleet"),
  shapefile_fix = shapefile.fix,
  continent = continent,
  coverage = TRUE,
  parameter_titre_dataset_1 =  "Final",
  parameter_titre_dataset_2 = "Conv",
  parameter_geographical_dimension_groupping = "gridtype",
  unique_analyse = FALSE,
  topnumber = 50
)
# surtout d'atures espèces que thons majeurs
View(child_env_first_to_last_result$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay)

child_env_first_to_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
  parameter_init ="~/firms-gta/geoflow-tunaatlas/jobs/20260311191047level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/Conv_NO_nominal13/ancient.qs",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20260311191047level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/RF_pass_13/ancient.qs",
  parameter_fact = "catch",
  parameter_colnames_to_keep = c(parameter_colnames_to_keep_fact, "species", "gear_type", "fishing_fleet"),
  shapefile_fix = shapefile.fix,
  continent = continent,
  coverage = TRUE,
  parameter_titre_dataset_1 =  "Final",
  parameter_titre_dataset_2 = "Conv",
  parameter_geographical_dimension_groupping = "gridtype",
  unique_analyse = FALSE,
  topnumber = 50
)
# tres grosse augmentation pour argentine, portugalpas mal aussi, usa un peu tout en fait. (USA beaucou en nombre, MAR ausi), augmentation 10.9 bcp, 99.9 pas mal aussi 
# pas agumentation #SKJ, YFT
# augmentatoin 60% ALB et 40 % BET,  50%SWO
View(child_env_first_to_last_result$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay)

child_env_first_to_last_result_iotc <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
  parameter_init ="~/firms-gta/geoflow-tunaatlas/jobs/20260309124848level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/Conv_NO_nominal13/ancient.qs",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20260309124848level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/RF_pass_13/ancient.qs",
  parameter_fact = "catch",
  parameter_colnames_to_keep = parameter_colnames_to_keep_fact,
  shapefile_fix = shapefile.fix,
  continent = continent,
  parameter_filtering = list("source_authority" = "IOTC"),
    coverage = TRUE,
  parameter_titre_dataset_1 =  "Final",
  parameter_titre_dataset_2 = "Conv",
  parameter_geographical_dimension_groupping = "gridtype",
  unique_analyse = FALSE
)


child_env_first_to_last_majortunas <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
  parameter_init ="~/firms-gta/geoflow-tunaatlas/jobs/20260309124848level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/Conv_NO_nominal13/ancient.qs",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20260309124848level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/RF_pass_13/ancient.qs",
  parameter_fact = "catch",
  parameter_colnames_to_keep = parameter_colnames_to_keep_fact,
  shapefile_fix = shapefile.fix,
  continent = continent,
  parameter_filtering = list(species = c("YFT", "SKJ", "BET", "ALB", "SBF", "SWO")),  
  parameter_titre_dataset_1 =  "Final",
  parameter_titre_dataset_2 = "Conv",
  parameter_geographical_dimension_groupping = "gridtype",
  unique_analyse = FALSE
)


child_env_first_to_last_alb <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
  parameter_init ="~/firms-gta/geoflow-tunaatlas/jobs/20260309124848level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/Conv_NO_nominal13/ancient.qs",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20260309124848level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/RF_pass_13/ancient.qs",
  parameter_fact = "catch",
  parameter_colnames_to_keep = parameter_colnames_to_keep_fact,
  shapefile_fix = shapefile.fix,
  continent = continent,
  parameter_filtering = list(species = "ALB"),  
  parameter_titre_dataset_1 =  "Final",
  parameter_titre_dataset_2 = "Conv",
  parameter_geographical_dimension_groupping = "gridtype",
  unique_analyse = FALSE
)
