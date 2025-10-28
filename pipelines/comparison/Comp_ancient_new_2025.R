run_analysis <- function(init, final, source_authority) {
  # Extraire le nom du fichier à partir du chemin complet
  output_file <- tools::file_path_sans_ext(basename(final)) # Supprime l'extension .csv
  
  # Exécuter l'analyse
  analysis_result <- comprehensive_cwp_dataframe_analysis(
    parameter_init = init,
    parameter_final = final,
    fig.path = "",
    parameter_fact = "catch",
    parameter_colnames_to_keep = c("gear_type", "fishing_fleet", "species", "measurement_unit", "measurement_value"),
    coverage = TRUE,
    shapefile_fix = NULL,
    parameter_time_dimension = c("time_start"),
    continent = NULL,
    parameter_titre_dataset_1 = "Dataset2024",
    parameter_titre_dataset_2 = "Dataset2025",
    unique_analyse = FALSE,
    print_map = FALSE
  )
  
  # Modifier les attributs de l'objet
  analysis_result$step_title_t_f <- FALSE
  analysis_result$parameter_short <- FALSE
  analysis_result$explenation <- FALSE
  analysis_result$child_header <- ""
  analysis_result$title_markdown <- paste("Comparison between 2024 and 2025 GTA", source_authority, "georef datasets")
  analysis_result$fig.path <- "Figures"
  analysis_result$Add_lines <- "Add_lines.Rmd"
  
  # Préparer l'environnement pour le rendu du RMarkdown
  render_env <- new.env()
  list2env(analysis_result, render_env)
  
  # Définition du répertoire de sortie
  output_dir <- here::here(paste0("docs/", source_authority, "_comp"))
  
  # Rendre le document
  setwd(here::here("Analysis_markdown")) 
  rmarkdown::render(
    input = "comparison.Rmd",
    envir = render_env,
    output_format = "bookdown::html_document2",
    output_dir = output_dir,
    output_file = output_file # Utilise le nom du fichier CSV comme nom de sortie
  )
  
  # Revenir au répertoire de base
  setwd(here::here())
}

# Nominal ------------------------------------------------------------

run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20250117134936_raw_nominal_catch/entities/nominal_catch_ccsbt_level0__bygear/Markdown/mapping_codelist/data.qs",
  final = "~/firms-gta/geoflow-tunaatlas/jobs/20250203133245_raw_nominal_catch_2024/entities/nominal_catch_ccsbt_level0__bygear/Markdown/mapping_codelist/data.qs",
  source_authority = "CCSBT")
  
run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20250117134936_raw_nominal_catch/entities/nominal_catch_wcpfc_level0//Markdown/mapping_codelist/data.qs",
  final = "~/firms-gta/geoflow-tunaatlas/jobs/20250203133245_raw_nominal_catch_2024/entities/nominal_catch_wcpfc_level0//Markdown/mapping_codelist/data.qs",
  source_authority = "WCPFC"
)

# wcpfc_georef ------------------------------------------------------------
# init <- readRDS("~/firms-gta/geoflow-tunaatlas/jobs/20231026074436_global_catch/entities/global_catch_firms_level0_/Markdown/rawdata/rds.rds") %>% dplyr::filter(source_authority == "WCPFC")
# final <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20250206054639/entities/global_catch_firms_new_level0/Markdown/rawdata/data.qs") %>% dplyr::filter(source_authority == "WCPFC")
# wcpfc_georef <- comprehensive_cwp_dataframe_analysis(
#   parameter_init = "~/firms-gta/geoflow-tunaatlas/jobs/20231026074436_global_catch/entities/global_catch_firms_level0_/Markdown/rawdata/rds.rds",
#   # parameter_init = "~/firms-gta/geoflow-tunaatlas/jobs/20250130091002new_level_1_2_01_2025/entities/global_catch_ird_level2_rf1_convert_decrease/Markdown/Level0_Firms/ancient.qs",
#   parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20250206054639/entities/global_catch_firms_new_level0/Markdown/rawdata/data.qs",
#   fig.path = "",
#   parameter_fact = "catch",
#   parameter_colnames_to_keep = c("gear_type", "fishing_fleet", "species", "measurement_unit", "measurement_value", "source_authority"),
#   coverage = TRUE,
#   shapefile_fix = NULL,
#   parameter_filtering = list(source_authority = c("WCPFC")),
#   parameter_time_dimension = c("time_start"),
#   continent = NULL,
#   parameter_titre_dataset_1 = "Dataset2024",parameter_titre_dataset_2 = "Dataset2025",
#   unique_analyse = FALSE, print_map = FALSE
# )
# wcpfc_georef$step_title_t_f <- FALSE
# wcpfc_georef$parameter_short <- FALSE
# wcpfc_georef$explenation <- FALSE
# wcpfc_georef$child_header <- ""
# wcpfc_georef$title_markdown <- "Comparison between 2024 and 2025 GTA WCPFC georef datasets"
# wcpfc_georef$fig.path <- "Figures"
# wcpfc_georef$Add_lines <- "Add_lines.Rmd"
# 
# render_env <- new.env()
# list2env(wcpfc_georef, render_env)
# setwd(here::here("Analysis_markdown")) 
# 
# rmarkdown::render(input = "comparison.Rmd",
#                   envir = render_env,
#                   output_format = "bookdown::html_document2",
#                   output_dir = here::here("docs/WCPFC_comp"), output_file = "Georef_comp"
# )
# 
# setwd(here::here())
# > wcpfc_georef$summary_of_differences
# Key: <measurement_unit>
#   measurement_unit Dataset2024 Dataset2025 Difference Difference (in %)
# <char>       <num>       <num>      <num>             <num>
#   1:               no   472733953    26401287 -446332666        -94.415191
# 2:                t    73639673    76958313    3318639          4.506592
# pas normal erreur avec NO mais en gros vient sûrement du drive ou du nom jsp ça a l'air de pas prendre un des jeux de donneés
# ccsbt_georef ------------------------------------------------------------

ccsbt_georef <- comprehensive_cwp_dataframe_analysis(
  parameter_init = "~/firms-gta/geoflow-tunaatlas/jobs/20231026074436_global_catch/entities/global_catch_firms_level0_/Markdown/rawdata/rds.rds",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20250206054639/entities/global_catch_firms_new_level0/Markdown/rawdata/data.qs",
  fig.path = "",
  parameter_fact = "catch",
  parameter_colnames_to_keep = c("gear_type", "fishing_fleet", "species", "measurement_unit", "measurement_value", "source_authority"),
  coverage = TRUE,
  shapefile_fix = NULL,
  parameter_filtering = list(source_authority = c("CCSBT")),
  parameter_time_dimension = c("time_start"),
  continent = NULL,
  parameter_titre_dataset_1 = "Dataset2024",parameter_titre_dataset_2 = "Dataset2025",
  unique_analyse = FALSE, print_map = FALSE
)
ccsbt_georef$step_title_t_f <- FALSE
ccsbt_georef$parameter_short <- FALSE
ccsbt_georef$explenation <- FALSE
ccsbt_georef$child_header <- ""
ccsbt_georef$title_markdown <- "Comparison between 2024 and 2025 GTA ccsbt georef datasets"
ccsbt_georef$fig.path <- "Figures"
ccsbt_georef$Add_lines <- "Add_lines.Rmd"

render_env <- new.env()
list2env(ccsbt_georef, render_env)
setwd(here::here("Analysis_markdown")) 

rmarkdown::render(input = "comparison.Rmd",
                  envir = render_env,
                  output_format = "bookdown::html_document2",
                  output_dir = here::here("docs/CCSBT_comp"), output_file = "Georef_comp"
)

setwd(here::here())



# testwcpfc ---------------------------------------------------------------


wcpfc_raw <- comprehensive_cwp_dataframe_analysis(
  parameter_init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_5deg_1m_ll_wcpfc_level0/data/WCPFC_L_PUBLIC_BY_FLAG_MON_harmonized.csv",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20250203133532_raw_data_georef_2024/entities/catch_5deg_1m_ll_wcpfc_level0/data/WCPFC_L_PUBLIC_BY_FLAG_MON_harmonized.csv",
  fig.path = "",
  parameter_fact = "catch",
  parameter_colnames_to_keep = c("gear_type", "fishing_fleet", "species", "measurement_unit", "measurement_value"),
  coverage = TRUE,
  shapefile_fix = NULL,
  parameter_time_dimension = c("time_start"),
  continent = NULL,
  parameter_titre_dataset_1 = "Dataset2024",parameter_titre_dataset_2 = "Dataset2025",
  unique_analyse = FALSE, print_map = FALSE
)
wcpfc_raw$step_title_t_f <- FALSE
wcpfc_raw$parameter_short <- FALSE
wcpfc_raw$explenation <- FALSE
wcpfc_raw$child_header <- ""
wcpfc_raw$title_markdown <- "Comparison between 2024 and 2025 GTA WCPFC georef datasets"
wcpfc_raw$fig.path <- "Figures"
wcpfc_raw$Add_lines <- "Add_lines.Rmd"

render_env <- new.env()
list2env(wcpfc_raw, render_env)
setwd(here::here("Analysis_markdown")) 

rmarkdown::render(input = "comparison.Rmd",
                  envir = render_env,
                  output_format = "bookdown::html_document2",
                  output_dir = here::here("docs/WCPFC_comp"), output_file = "WCPFC_L_PUBLIC_BY_FLAG_MON_comp"
)

setwd(here::here())
# pa grosse différnce mais ça doit quand mêem être là 

# > sum(WCPFC_L_PUBLIC_BY_FLAG_MON$alb_c)
# [1] 2799531
# > sum(WCPFC_L_PUBLIC_BY_FLAG_MON$alb_n)
# [1] 191326010

wcpfc_raw <- comprehensive_cwp_dataframe_analysis(
  parameter_init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_5deg_1m_bb_wcpfc_level0//data/WCPFC_P_PUBLIC_BY_YR_MON.csv_harmonized.csv",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20250203133532_raw_data_georef_2024/entities/catch_5deg_1m_bb_wcpfc_level0/data/WCPFC_P_PUBLIC_BY_YR_MON.csv_harmonized.csv",
  fig.path = "",
  parameter_fact = "catch",
  parameter_colnames_to_keep = c("gear_type", "fishing_fleet", "species", "measurement_unit", "measurement_value"),
  coverage = TRUE,
  shapefile_fix = NULL,
  parameter_time_dimension = c("time_start"),
  continent = NULL,
  parameter_titre_dataset_1 = "Dataset2024",parameter_titre_dataset_2 = "Dataset2025",
  unique_analyse = FALSE, print_map = FALSE
)
wcpfc_raw$step_title_t_f <- FALSE
wcpfc_raw$parameter_short <- FALSE
wcpfc_raw$explenation <- FALSE
wcpfc_raw$child_header <- ""
wcpfc_raw$title_markdown <- "Comparison between 2024 and 2025 GTA WCPFC georef datasets"
wcpfc_raw$fig.path <- "Figures"
wcpfc_raw$Add_lines <- "Add_lines.Rmd"

render_env <- new.env()
list2env(wcpfc_raw, render_env)
setwd(here::here("Analysis_markdown")) 

rmarkdown::render(input = "comparison.Rmd",
                  envir = render_env,
                  output_format = "bookdown::html_document2",
                  output_dir = here::here("docs/WCPFC_comp"), output_file = "WCPFC_P_PUBLIC_BY_YR_MON"
)

setwd(here::here())
# pa grosse différnce


wcpfc_raw <- comprehensive_cwp_dataframe_analysis(
  parameter_init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_5deg_1m_driftnet_wcpfc_level0///data/WCPFC_G_PUBLIC_BY_YR_MON.csv_harmonized.csv",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20250203133532_raw_data_georef_2024/entities/catch_5deg_1m_driftnet_wcpfc_level0/data/WCPFC_G_PUBLIC_BY_YR_MON.csv_harmonized.csv",
  fig.path = "",
  parameter_fact = "catch",
  parameter_colnames_to_keep = c("gear_type", "fishing_fleet", "species", "measurement_unit", "measurement_value"),
  coverage = TRUE,
  shapefile_fix = NULL,
  parameter_time_dimension = c("time_start"),
  continent = NULL,
  parameter_titre_dataset_1 = "Dataset2024",parameter_titre_dataset_2 = "Dataset2025",
  unique_analyse = FALSE, print_map = FALSE
)
wcpfc_raw$step_title_t_f <- FALSE
wcpfc_raw$parameter_short <- FALSE
wcpfc_raw$explenation <- FALSE
wcpfc_raw$child_header <- ""
wcpfc_raw$title_markdown <- "Comparison between 2024 and 2025 GTA WCPFC georef datasets"
wcpfc_raw$fig.path <- "Figures"
wcpfc_raw$Add_lines <- "Add_lines.Rmd"

render_env <- new.env()
list2env(wcpfc_raw, render_env)
setwd(here::here("Analysis_markdown")) 

rmarkdown::render(input = "comparison.Rmd",
                  envir = render_env,
                  output_format = "bookdown::html_document2",
                  output_dir = here::here("docs/WCPFC_comp"), output_file = "WCPFC_G_PUBLIC_BY_YR_MON"
)

setwd(here::here())
# aucune différence c'est pas normal en fait ça a l'air normal vu qu'ils le changent pas depuis les années 90

wcpfc_raw <- comprehensive_cwp_dataframe_analysis(
  parameter_init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_5deg_1m_ps_wcpfc_level0////data/WCPFC_S_PUBLIC_BY_YR_MON.csv_harmonized.csv",
  parameter_final = "~/firms-gta/geoflow-tunaatlas/jobs/20250203133532_raw_data_georef_2024/entities/catch_5deg_1m_ps_wcpfc_level0/data/WCPFC_S_PUBLIC_BY_YR_MON.csv_harmonized.csv",
  fig.path = "",
  parameter_fact = "catch",
  parameter_colnames_to_keep = c("gear_type", "fishing_fleet", "species", "measurement_unit", "measurement_value"),
  coverage = TRUE,
  shapefile_fix = NULL,
  parameter_time_dimension = c("time_start"),
  continent = NULL,
  parameter_titre_dataset_1 = "Dataset2024",parameter_titre_dataset_2 = "Dataset2025",
  unique_analyse = FALSE, print_map = FALSE
)

wcpfc_raw$step_title_t_f <- FALSE
wcpfc_raw$parameter_short <- FALSE
wcpfc_raw$explenation <- FALSE
wcpfc_raw$child_header <- ""
wcpfc_raw$title_markdown <- "Comparison between 2024 and 2025 GTA WCPFC georef datasets"
wcpfc_raw$fig.path <- "Figures"
wcpfc_raw$Add_lines <- "Add_lines.Rmd"

render_env <- new.env()
list2env(wcpfc_raw, render_env)
setwd(here::here("Analysis_markdown")) 

rmarkdown::render(input = "comparison.Rmd",
                  envir = render_env,
                  output_format = "bookdown::html_document2",
                  output_dir = here::here("docs/WCPFC_comp"), output_file = "WCPFC_S_PUBLIC_BY_YR_MON"
)

setwd(here::here())
# dexu erreurs potentielles donc déjà il faut vérfier qu'il y a bien la dernière année et en plus regarder comme les NOMT MTNO fonctionnnetn



# Exemple d'utilisation
run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_5deg_1m_ps_wcpfc_level0////data/WCPFC_S_PUBLIC_BY_YR_MON.csv_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/jobs/20250203133532_raw_data_georef_2024/entities/catch_5deg_1m_ps_wcpfc_level0/data/WCPFC_S_PUBLIC_BY_YR_MON.csv_harmonized.csv",
  source_authority = "WCPFC"
)


run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_1deg_1m_bb_iattc_level0__tuna_byflag////data/PublicLPTunaFlag_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/data/jobs/20250207092412_raw_data_georef/entities/catch_1deg_1m_bb_iattc_level0__tuna_byflag/data/PublicLPTunaFlag_harmonized.csv",
  source_authority = "IATTC"
)


run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__billfish_byflag////data/PublicPSBillfishFlag_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/data/jobs/20250207092412_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__billfish_byflag/data/PublicPSBillfishFlag_harmonized.csv",
  source_authority = "IATTC"
)


run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__billfish_byschool////data/PublicPSBillfishSetType_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/data/jobs/20250207092412_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__billfish_byschool/data/PublicPSBillfishSetType_harmonized.csv",
  source_authority = "IATTC"
)

run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__shark_byflag////data/PublicPSSharkFlag_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/data/jobs/20250207092412_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__shark_byflag/data/PublicPSSharkFlag_harmonized.csv",
  source_authority = "IATTC"
)

run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__shark_byschool////data/PublicPSSharkSetType_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/data/jobs/20250207092412_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__shark_byschool/data/PublicPSSharkSetType_harmonized.csv",
  source_authority = "IATTC"
)

run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__tuna_byflag////data/PublicPSTunaFlag_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/data/jobs/20250207092412_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__tuna_byflag/data/PublicPSTunaFlag_harmonized.csv",
  source_authority = "IATTC"
)

run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20240524101747_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__tuna_byschool////data/PublicPSTunaSetType_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/data/jobs/20250207092412_raw_data_georef/entities/catch_1deg_1m_ps_iattc_level0__tuna_byschool/data/PublicPSTunaSetType_harmonized.csv",
  source_authority = "IATTC"
)



run_analysis(
  init = "~/firms-gta/geoflow-tunaatlas/jobs/20250117134936_raw_nominal_catch/entities/nominal_catch_iattc_level0/////data/CatchFlagGear_harmonized.csv",
  final = "~/firms-gta/geoflow-tunaatlas/data/jobs/20250207110545/entities/nominal_catch_iattc_level0//data/CatchByFlagGear1918-2023_harmonized.csv",
  source_authority = "IATTC"
)



