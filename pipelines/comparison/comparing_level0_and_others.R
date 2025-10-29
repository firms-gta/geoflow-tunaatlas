source("launching_jsons_creating_GTA.R")
a <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs_test/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level0_Firms/data.qs")
a <- a %>% slice_sample(n = 1000)
# qs::qsave(x = a, file = "~/firms-gta/geoflow-tunaatlas/jobs_test/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level0_Firms/data.qs")



b <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs_test/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level2_RF1_basic/data.qs")
b <- b %>% slice_sample(n = 1000)
# qs::qsave(x = a, file = "~/firms-gta/geoflow-tunaatlas/jobs_test/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level2_RF1_basic/data.qs")



c <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs_test/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level2_RF1_full/data.qs")
c <- c %>% slice_sample(n = 1000)
# qs::qsave(x = a, file = "~/firms-gta/geoflow-tunaatlas/jobs_test/20241112172709_LVL2_RECAP/entities/global_catch_ird_level2_rf1/Markdown/Level2_RF1_full/data.qs")


path_test <- "~/firms-gta/geoflow-tunaatlas/jobs_test/20241112172709_LVL2_RECAP"
setwd("~/firms-gta/geoflow-tunaatlas")
require(CWP.dataset)
CWP.dataset::summarising_step(main_dir = path_test, con = con, config  = config, sizepdf = "middle",savestep = FALSE, usesave = FALSE, 
                 source_authoritylist = c("all"))

# file_path_url <- "~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions"
# source(file.path(file_path_url,"copy_project_files.R"), local = TRUE)
# source(file.path(file_path_url,"tidying_GTA_data_for_comparison.R"))
# source(file.path(file_path_url,"Functions_markdown.R"), local = TRUE)
# source(file.path(file_path_url,"compare_temporal_differences_dygraphs.R"), local = TRUE)
# source(file.path(file_path_url,"other_dimension_analysis_dygraphs.R"), local = TRUE)
# source(file.path(file_path_url,"Groupping_differences.R"), local = TRUE)
# source(file.path(file_path_url,"compare_strata_differences.R"), local = TRUE)
# source(file.path(file_path_url,"compare_dimension_differences.R"), local = TRUE)
# source(file.path(file_path_url,"compare_temporal_differences.R"), local = TRUE)
# source(file.path(file_path_url,"geographic_diff.R"), local = TRUE)
# source(file.path(file_path_url,"time_coverage_analysis.R"), local = TRUE)
# source(file.path(file_path_url,"spatial_coverage_analysis.R"), local = TRUE)
# source(file.path(file_path_url,"other_dimension_analysis.R"), local = TRUE)
# source(file.path(file_path_url,"comprehensive_cwp_dataframe_analysis.R"), local = TRUE)
# source(file.path(file_path_url,"process_fisheries_data.R"), local = TRUE)

species_group <- st_read(con, query = "SELECT taxa_order, code FROM species.species_asfis") %>%
  janitor::clean_names() %>%
  dplyr::select(species_group = taxa_order, species = code)
flog.info("Loaded species_group data")

if(!file.exists("data/cl_fishing_mode.csv")){
  url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/31756d4c0baf44c6d7d851e93c14c1e6917f7276/global/firms/gta/cl_fishing_mode.csv"
  destination <- "data/cl_fishing_mode.csv"
  
  utils::download.file(url, destination, method = "curl")
}
setwd(ancient_wd)
cl_fishing_mode <- readr::read_csv("data/cl_fishing_mode.csv")

species_label <- st_read(con, query = "SELECT * FROM species.species_asfis") %>%
  janitor::clean_names()
fishing_fleet_label <- st_read(con, query = "SELECT * FROM fishing_fleet.fishingfleet_firms") %>%
  janitor::clean_names()

cl_cwp_gear_level2 <- st_read(con, query = "SELECT * FROM gear_type.isscfg_revision_1") %>%
  dplyr::select(Code = code, Gear = label)

flog.info("Loaded cl_cwp_gear_level2 data")

shapefile.fix <- st_read(con, query = "SELECT * FROM area.cwp_grid") %>%
  dplyr::rename(GRIDTYPE = gridtype)
flog.info("Loaded shapefile.fix data")

continent <- tryCatch({
  st_read(con, query = "SELECT * FROM public.continent")
}, error = function(e) {
  flog.error("An error occurred while reading continent data: %s", e$message)
  NULL
})

if (is.null(continent)) {
  flog.warn("Continent data not found in the database. Fetching from WFS service.")
  url <- "https://www.fao.org/fishery/geoserver/wfs"
  serviceVersion <- "1.0.0"
  logger <- "INFO"
  WFS <- WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
  continent <- WFS$getFeatures("fifao:UN_CONTINENT2")
  flog.info("Fetched continent data from WFS service")
}

shape_without_geom <- shapefile.fix %>%
  as_tibble() %>%
  dplyr::select(-geom)
flog.info("Processed shapefile.fix data")

child_env_first_to_last_result <- comprehensive_cwp_dataframe_analysis(
  parameter_init = b,
  parameter_final = c,
  fig.path = NULL,
  parameter_fact = "catch",
  parameter_colnames_to_keep = c("source_authority", "species","geographic_identifier",
                                 "measurement_unit", "measurement_value", "GRIDTYPE"),
  shapefile_fix = shapefile.fix,
  continent = continent,
  coverage = TRUE,
  parameter_resolution_filter = NULL,
  parameter_titre_dataset_1 = "FirmsLevel0",
  parameter_titre_dataset_2 = entity$identifiers[["id"]],
  unique_analyse = FALSE
)

child_env_first_to_last_result <- comprehensive_cwp_dataframe_analysis(
  parameter_init = b,
  parameter_final = c,
  fig.path = parameters_child_global$fig.path,
  parameter_fact = "catch",
  parameter_colnames_to_keep = c("source_authority", "species","geographic_identifier",
                                 "measurement_unit", "measurement_value", "GRIDTYPE"),
  shapefile_fix = shapefile.fix,
  continent = continent,
  coverage = FALSE,
  parameter_resolution_filter = parameters_child_global$parameter_resolution_filter,
  parameter_filtering = parameters_child_global$parameter_filtering,
  parameter_titre_dataset_1 = "FirmsLevel0",
  parameter_titre_dataset_2 = entity$identifiers[["id"]],
  unique_analyse = FALSE
)
