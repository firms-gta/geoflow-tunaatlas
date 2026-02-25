

level0_2026 <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260224104016level_0_catch_2026/entities/global_catch_ird_level2_1950_2023/Markdown/rawdata/data.qs")
level0_2025 <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20250513162429level_2_catch_2025_raise_only_unk_passe4/entities/global_catch_ird_lvl2_rawdata_1950_2023_upgrade_nominal_passes4_weak_raise_only_on_unk/Markdown/rawdata/data.qs") %>%
  dplyr::filter(source_authority != "IOTC" & source_authority != "ICCAT")

if(!file.exists("data/cl_fishing_mode.csv")){
  url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/31756d4c0baf44c6d7d851e93c14c1e6917f7276/global/firms/gta/cl_fishing_mode.csv"
  destination <- "data/cl_fishing_mode.csv"
  
  utils::download.file(url, destination, method = "curl")
}
cl_fishing_mode <- readr::read_csv("data/cl_fishing_mode.csv")


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
cwp_grid_file <- here::here("data/cl_areal_grid.csv")
if (!file.exists(cwp_grid_file)) {
  zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
  zip_path <- here::here("data/cwp_grid.zip")
  message("Downloading CWP Grid...")
  utils::download.file(zip_url, zip_path, mode = "wb")
  utils::unzip(zip_path, exdir = here::here("data"))
}
shapefile.fix <- sf::st_read(cwp_grid_file) %>%
  sf::st_as_sf(wkt = "geom_wkt", crs = 4326) %>%
  dplyr::rename(cwp_code = CWP_CODE, geom = geom_wkt)

level0_2025 <- level0_2025 %>% 
  dplyr::mutate(time_start = as.Date(time_start))%>% 
  dplyr::mutate(time_end = as.Date(time_end))%>% dplyr::mutate(measurement_unit = case_when(measurement_unit == "Tons" ~ "t", 
                                                                                            measurement_unit == "Number of fish" ~ "no",
                                                                                            TRUE ~ measurement_unit))

level0_2026 <- level0_2026 %>% 
  dplyr::mutate(time_start = as.Date(time_start))%>% 
  dplyr::mutate(time_end = as.Date(time_end)) %>% dplyr::mutate(measurement_unit = case_when(measurement_unit == "Tons" ~ "t", 
                                                                                             measurement_unit == "Number of fish" ~ "no",
                                                                                             TRUE ~ measurement_unit)) %>% dplyr::filter(time_start < "2024-01-01")

result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(parameter_init = level0_2025 , 
                                                            parameter_final = level0_2026 ,
                                                            shapefile_fix = shapefile.fix,
                                                            continent = continent,
                                                            plotting_type = "plot", 
                                                            parameter_geographical_dimension_groupping = "gridtype",
                                                            parameter_titre_dataset_1 = "V2025", 
                                                            parameter_titre_dataset_2 = "V2026", 
                                                            parameter_short = FALSE)

unlink("comp2025_2026.qs")
qs::qsave(result,  "comp2025_2026.qs")
result$time_coverage_analysis_list
result$summary_of_differences
result$disapandap
result$strates_perdues_first_10
rm(result)

source_authoritylist <- c("CCSBT", "IATTC", "WCPFC")

for (i in 1:length(source_authoritylist)){
  
  
  result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(level0_2025 , level0_2026, 
                                                              shapefile_fix = shapefile.fix,
                                                              continent = continent,
                                                              parameter_filtering = list(source_authority = source_authoritylist[i]),
                                                              parameter_titre_dataset_1 = "V2025", 
                                                              parameter_titre_dataset_2 = "V2026", coverage = TRUE)
  result$time_coverage_analysis_list
  result$summary_of_differences
  result$disapandap
  result$strates_perdues_first_10
  qs::qsave(result, paste0(source_authoritylist[i], "compV2025_2026.qs"))
  rm(result)
  gc()
}
# 
# qflextable(wcpfc$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay)qflextable(wcpfc$summary_of_differences)
# iattc$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay}
# iattc$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay
# iattc$summary_of_differences
# iotc$summary_of_differences
# ccsbt$summary_of_differences
# iccat$summary_of_differences
# wcpfc <- qs::qread("WCPFCresult.qs")
# wcpfc
# qflextable(compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay)
# qflextable(wcpfc$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay)
# qflextable(wcpfc$summary_of_differences)
# iotc <- qs::qread("IOTCresult.qs")
# ccsbt <- qs::qread("CCSBTresult.qs")
# iccat <- qs::qread("ICCATresult.qs")
# iattc <- qs::qread("IATTCresult.qs")



# Load necessary library
library(rmarkdown)

library(qs)
library(rmarkdown)

# Function to render R Markdown based on the loaded QS file
render_analysis <- function(object, name = "name", file_path =system.file("rmd/comparison.Rmd", package = "CWP.dataset")) {
  env_V2025_en <- new.env()
  list2env(object, env_V2025_en)  # Ensure 'object' is a list of data from QS file
  options(knitr.duplicate.label = "allow")
  env_V2025_en$step_title_t_f <- F
  env_V2025_en$child_header = "-#"
  env_V2025_en$child = TRUE
  env_V2025_en$parameter_short = FALSE
  # Render the R Markdown document
  rmarkdown::render(
    input = file_path,
    output_format = "pdf_document",
    output_file = paste0(name, "_Data_Analysis.pdf"),
    output_dir = getwd(),
    envir = env_V2025_en
  )
  
  # Cleanup
  rm(env_V2025_en)
}

# Example of loading QS files and rendering Rmd for each
# qs_files <- list.files(pattern = "\\.qs$")
# qs_files <- qs_files[-13]
# qs_files <- qs_files[-13]
# qs_files <- qs_files[-10]
# qs_files <- qs_files[-3]
# qs_files <- qs_files[-10]
# qs_files <- qs_files[-1]

specific_qs_files <- c(
  "WCPFCmajortunascompV2025_2026.qs", "IOTCmajortunascompV2025_2026.qs", "IOTCcompV2025_2026.qs",
  "ICCATmajortunascompV2025_2026.qs", "ICCATcompV2025_2026.qs", "IATTCmajortunascompV2025_2026.qs",
  "IATTCcompV2025_2026.qs", "CCSBTcompV2025_2026.qs","WCPFCcompV2025_2026.qs",
  "majortunascompV2025_2026.qs",
  "comp2025_2026.qs"
)

# names_of_datasets <- sub(".qs", "", specific_qs_files)  # Remove file extension for naming

for (file_name in specific_qs_files) {
  set_flextable_defaults(fonts_ignore=TRUE)
  object_qs <- qs::qread(file_name)  # Load the QS file
  names_of_datasets <- sub(".qs", "", file_name)  # Remove file extension for naming
  
  render_analysis(object = object_qs, name = names_of_datasets)
}


# analysis of the numbers