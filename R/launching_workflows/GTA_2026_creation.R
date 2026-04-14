# =============================================================================
# 0. ENVIRONNEMENT / PACKAGES
# =============================================================================
require(here)
# source(here::here('R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R'))
library(renv)
renv::restore()

required_packages <- c(
  "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor", 
  "dotenv", "data.table", "here", "xfun", "RPostgreSQL", "RPostgres", "DBI", 
  "rpostgis", "terra", "sf", "RSQLite", "webshot", "usethis", "ows4R", "sp", 
  "flextable", "dplyr", "stringr", "tibble", "bookdown", "knitr", 
  "purrr", "readxl", "odbc", "rlang", "kableExtra", "tidyr", "ggplot2", "fs" ,
  "stats", "RColorBrewer", "cowplot", "tmap", "curl", "officer", 
  "gdata", "R3port", "reshape2", "tools", "plogr", "futile.logger", "lubridate", "data.table", "geoflow"
)

install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    library(package, character.only = TRUE)
  }
}

invisible(lapply(required_packages, install_and_load))

# presence_absence_flagging <- readr::read_csv(here::here("Species_Presence___Absence.csv")) # ca c'est pour specifier a la fin la donnee qui est presente dans un seul ocean

# =============================================================================
# 1. VARIABLES D'ENV / PATCH / SCRIPTS UTILES
# =============================================================================

default_file <- ".env"
if (file.exists(here::here("geoserver_sdi_lab.env"))) { # Julien si tu veux tester sur une BD il faut juste un .env  
  default_file <- "geoserver_sdi_lab.env"
}
load_dot_env(file = here::here(default_file))

source(here::here("R/running_time_of_workflow.R"))
source(here::here("R/executeAndRename.R"))

patch_geoflow_getJobDataResource <- function() {
  gen <- geoflow::geoflow_entity
  gen$set(
    "public", "getJobDataResource",
    overwrite = TRUE,
    value = function(config, path) {
      path <- gsub("\\\\", "/", path)
      file.path(getwd(), "data", basename(path))
    }
  )
  invisible(TRUE)
}
patch_geoflow_getJobDataResource()

# =============================================================================
# 2. CHECK DES DONNÉES REQUISES À PARTIR DES 3 JSON DE PRÉ-HARMONISATION
# =============================================================================

extract_sources_from_csv <- function(files) {
  unique(unlist(lapply(files, function(f) {
    txt <- readLines(f, warn = FALSE)
    src <- grep("source:", txt, value = TRUE)
    unlist(strsplit(sub(".*source:", "", src), ","))
  }))) |>
    trimws() |>
    sub("^\\./data/", "", x = _) |>
    sub("_$", "", x = _)
}

get_entity_csv_from_json <- function(json_file) {
  config <- initWorkflow(here::here(json_file), handleMetadata = FALSE)
  unlink(config$job, recursive = TRUE)
  unique(vapply(
    Filter(function(x) identical(x$handler, "csv"), config$metadata$entities),
    function(x) x$source,
    character(1)
  ))
}

check_files_from_json <- function(json_files, data_dir = "data", max_age_days = 365) {
  csv_files <- unique(unlist(lapply(json_files, get_entity_csv_from_json)))
  files_detected <- extract_sources_from_csv(csv_files)
  paths <- file.path(data_dir, files_detected)
  
  exists <- file.exists(paths)
  missing <- files_detected[!exists]
  
  if (length(missing)) {
    message("Missing files:")
    print(missing)
  } else {
    message("No missing files, workflow can be run")
  }
  
  existing_paths <- paths[exists]
  if (length(existing_paths)) {
    info <- file.info(existing_paths)
    ages <- as.numeric(Sys.Date() - as.Date(info$mtime))
    old_idx <- ages > max_age_days
    
    if (any(old_idx)) {
      warning(
        "Files older than ", max_age_days, " days:\n",
        paste0(basename(existing_paths[old_idx]), " (", as.Date(info$mtime[old_idx]), ")",
               collapse = "\n")
      )
    }
  }
  
  invisible(list(
    csv_files = csv_files,
    files_detected = files_detected,
    missing = missing
  ))
}

preharmo_json_files <- c(
  "config/Nominal_catch_2026.json",
  "config/All_raw_data_georef.json",
  "config/All_raw_data_georef_effort.json"
)

preharmo_check <- check_files_from_json(preharmo_json_files)

# =============================================================================
# 3. UTILITAIRES DB / WORKFLOW
# =============================================================================

should_upload_to_db <- function(config) { #
  drv  <- Sys.getenv("DB_DRV")
  host <- Sys.getenv("DB_HOST")
  port <- Sys.getenv("DB_PORT")
  db   <- Sys.getenv("DB_NAME")
  user <- Sys.getenv("DB_USER")
  
  ctx_ok <- identical(drv, "PostgreSQL") &&
    identical(port, "5432") &&
    identical(host, "db-tunaatlas.d4science.org") &&
    db %in% c("tunaatlas_sandbox", "tunaatlas") &&
    identical(user, "tunaatlas_u")
  
  if (!ctx_ok) return(FALSE)
  
  con <- config$software$output$dbi
  if (is.null(con)) return(FALSE)
  
  tryCatch(
    DBI::dbIsValid(con) && DBI::dbGetQuery(con, "SELECT 1 AS ok")$ok[1] == 1,
    error = function(e) FALSE
  )
}

force_upload_to_db <- function(config, action_id = "load_dataset") {
  for (ei in seq_along(config$entities)) {
    
    acts <- config$entities[[ei]]$actions
    ids  <- vapply(acts, `[[`, character(1), "id")
    
    # --- load_dataset ---
    sel <- which(ids == action_id)
    for (j in sel) {
      config$entities[[ei]]$actions[[j]]$options$upload_to_db <- TRUE
      config$entities[[ei]]$actions[[j]]$options$upload_to_db_public <- TRUE
      config$entities[[ei]]$actions[[j]]$options$create_materialized_view <- TRUE
      config$entities[[ei]]$actions[[j]]$options$add_sql_comments <- TRUE
      config$entities[[ei]]$actions[[j]]$options$upload_to_googledrive <- FALSE
    }
    
    # --- activer les actions liées DB ---
    ids_to_enable <- c(
      "enrich_metadata",
      "enrich_for_db_services",
      "load_metadata",
      "geometa-create-iso-19115",
      "geometa-create-iso-19110"
    )
    
    sel2 <- which(ids %in% ids_to_enable)
    for (j in sel2) {
      config$entities[[ei]]$actions[[j]]$run <- TRUE
    }
  }
  
  config
}

execute_workflow_maybe_upload <- function(file, dir = ".", rename_suffix = NULL) {
  out <- executeWorkflow(
    file = file,
    dir  = dir,
    on_initWorkflow = function(config, queue) {
      if (should_upload_to_db(config)) {
        config$logger.info("DB reachable + context OK -> upload_to_db=TRUE for load_dataset")
        force_upload_to_db(config, "load_dataset")
      } else {
        config$logger.info("DB not reachable or context not OK -> upload_to_db unchanged")
      }
    }
  )
  
  if (!is.null(rename_suffix)) {
    out <- executeAndRename(out, rename_suffix)
  }
  
  out
}

get_workflow_con <- function(config_file) {
  config <- initWorkflow(here::here(config_file), handleMetadata = FALSE)
  unlink(config$job, recursive = TRUE)
  config$software$output$dbi
}

summarise_invalid <- function(path, con) {
  CWP.dataset::summarising_invalid_data(
    path,
    connectionDB = con,
    upload_DB = FALSE,
    upload_drive = FALSE
  )
}

# =============================================================================
# 4. PRÉ-HARMONISATION
# =============================================================================
# 4.1 Nominal
# 4.2 Georeferenced catch
# 4.3 Georeferenced effort
# 4.4 Génération des Rmd

con_preharmo <- get_workflow_con("config/All_raw_data_georef_effort.json")

# ---- 4.1 NOMINAL ------------------------------------------------------------

raw_nominal_catch <- execute_workflow_maybe_upload(
  file = here::here("config/Nominal_catch_2026.json"),
  rename_suffix = "_raw_nominal_catch_2026_final"
)

running_time_of_workflow(raw_nominal_catch)
summarise_invalid(raw_nominal_catch, con_preharmo) # find report in the correspoding job

# ---- 4.2 GEOREFERENCED CATCH ------------------------------------------------

raw_data_georef <- execute_workflow_maybe_upload(
  file = here::here("config/All_raw_data_georef.json"),
  rename_suffix = "_raw_data_georef_final"
)

running_time_of_workflow(raw_data_georef)
summarise_invalid(raw_data_georef, con_preharmo)

# ---- 4.3 GEOREFERENCED EFFORT -----------------------------------------------

raw_data_georef_effort <- execute_workflow_maybe_upload(
  file = here::here("config/All_raw_data_georef_effort.json"),
  rename_suffix = "_raw_data_georef_effort_final"
)

running_time_of_workflow(raw_data_georef)
summarise_invalid(raw_data_georef_effort, con_preharmo)

# ---- 4.4 DOC QA PRE-HARMO ---------------------------------------------------

source(here::here("R/tunaatlas_scripts/pre-harmonization/rewrite_functions_as_rmd.R"))
rewrite_functions_as_rmd(raw_nominal_catch)
rewrite_functions_as_rmd(raw_data_georef)
rewrite_functions_as_rmd(raw_data_georef_effort)

# =============================================================================
# 5. CRÉATION DES DATASETS HARMONISÉS
# =============================================================================
# 5.1 Effort
# 5.2 Nominal
# 5.3 Catch level 0
# 5.4 Catch level 2

setwd(here::here())

# ---- 5.1 EFFORT -------------------------------------------------------------

tunaatlas_effort_path <- execute_workflow_maybe_upload(
  file = here::here("config/create_effort_dataset_2026.json"),
  rename_suffix = "new_efforts_final"
)

# ---- 5.2 NOMINAL ------------------------------------------------------------

tunaatlas_nominal_path <- execute_workflow_maybe_upload(
  file = here::here("config/create_nominal_dataset_2026.json"),
  rename_suffix = "nominal_final"
)

# ---- 5.3 CATCH LEVEL 0 ------------------------------------------------------

tunaatlas_level0_catch_path <- execute_workflow_maybe_upload(
  file = here::here("config/catch_ird_level0_local.json"),
  rename_suffix = "level_0_catch_2026"
)

config_level0 <- initWorkflow(here::here("config/catch_ird_level0_local.json"))
unlink(config_level0$job, recursive = TRUE)
con_level0 <- config_level0$software$output$dbi

CWP.dataset::summarising_step(
  main_dir = tunaatlas_level0_catch_path,
  connectionDB = con_level0,
  config = config_level0,
  sizepdf = "short",
  savestep = FALSE,
  usesave = FALSE,
  source_authoritylist = c("all")
)


# Intercaler le process mapping georef to nominal -------------------------

res_map <- propose_georef_to_nominal_mappings_clean(
  georef = georef_dataset,
  nominal = nominal_catch,
  id_cols = c("source_authority", "group_species_iattc_sharks", "year"),
  candidate_cols_order = c("fishing_mode_wcpfc_issue_unk_solved", "gear_type", "geographic_identifier_nom", "fishing_fleet")
)

# Level 1 to be added -----------------------------------------------------------------

tunaatlas_level1_catch_path <- execute_workflow_maybe_upload(
  file = here::here("config/catch_ird_level1_local.json"),
  rename_suffix = "level_2_catch_2026"
)

gc()

config_level1 <- initWorkflow(here::here("config/catch_ird_level1_local.json"))
unlink(config_level1$job, recursive = TRUE)
con_level1 <- config_level1$software$output$dbi

# ---- 5.4 CATCH LEVEL 2 ------------------------------------------------------

tunaatlas_level2_catch_path <- execute_workflow_maybe_upload(
  file = here::here("config/catch_ird_level2_local.json"),
  rename_suffix = "level_2_catch_2026"
)

gc()

config_level2 <- initWorkflow(here::here("config/catch_ird_level2_local.json"))
unlink(config_level2$job, recursive = TRUE)
con_level2 <- config_level2$software$output$dbi


# =============================================================================
# 6. RAPPORTS / SUMMARISING
# =============================================================================

# colnames_to_keep_report <- c(
#   "source_authority", "fishing_fleet_label", "fishing_mode_label",
#   "geographic_identifier", "measurement_unit", "measurement_value",
#   "gridtype", "species_label", "gear_type_label",
#   "measurement_processing_level"
# )
# 

for (sa in c("all")) {
  CWP.dataset::summarising_step(
    main_dir = tunaatlas_level2_catch_path,
    connectionDB = con_level2,
    config = config_level2,
    sizepdf = "short",
    savestep = FALSE,
    usesave = FALSE,
    source_authoritylist = sa
    # ,
    # parameter_colnames_to_keep_fact = colnames_to_keep_report
  )
}

source("~/firms-gta/geoflow-tunaatlas/R/ongoing_projects/plot_diffs_from_nominal_files.R")

comparisons <- c(
  "sums_species.csv" = "~/firms-gta/geoflow-tunaatlas/data/nominal_recap_species.csv",
  "sums_source_auth.csv" = "~/firms-gta/geoflow-tunaatlas/data/nominal_recap_source_authority.csv",
  "sums.csv" = "~/firms-gta/geoflow-tunaatlas/data/nominal_sums.csv"
)

res <- plot_diffs_from_nominal_files(
  main_dir = paste0(tunaatlas_level2_catch_path,"/entities/global_catch_ird_level2_1950_2024/Markdown"), #bof mais pas miexu pour le moment
  comparison_files = comparisons,
  value_col = "sum_t",
  filter_list = list(
    sums_species = c("YFT", "SKJ", "BET", "SWO", "ALB", "SBF"),
    sums_source_auth = NULL,
    sums = NULL
  )
)


ggplot2::ggsave(
  filename = file.path(
    "data",
    "plot_georef_vs_nominal_evolution.png"
  ),
  plot = res$plot,
  width = 16,
  height = 12,
  dpi = 300
)

source("~/firms-gta/geoflow-tunaatlas/R/ongoing_projects/check_georef_vs_nominal_entity.R")

file_path <- list(paste0(tunaatlas_level2_catch_path,"/entities/global_catch_ird_level2_1950_2024")) #bof mais pas miexu pour le moment, 

results <- lapply(file_path, run_analysis)


# config_level2$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <-
#   list(species_label = c(
#     "Skipjack tuna"
#   ), fishing_fleet_label = "Maldives")


for (sa in c( "ICCAT","IATTC", "IOTC", "WCPFC", "CCSBT")) {
  CWP.dataset::summarising_step(
    main_dir = tunaatlas_level2_catch_path,
    connectionDB = con_level2,
    config = config_level2,
    sizepdf = "short",
    savestep = FALSE,
    usesave = FALSE,
    source_authoritylist = sa
    # ,
    # parameter_colnames_to_keep_fact = colnames_to_keep_report
  )
}

# # =============================================================================
# # 7. RAPPORTS SPÉCIFIQUES
# # =============================================================================
# 
config_level2$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <-
  list(species_label = c(
    "Swordfish"), source_authority = c("WCPFC")
  )

CWP.dataset::summarising_step(
  main_dir = tunaatlas_level2_catch_path,
  connectionDB = con_level2,
  config = config_level2,
  sizepdf = "short",
  savestep = FALSE,
  usesave = FALSE,
  source_authoritylist = "all",
  nameoutput = "swo_wcpfc"
  # parameter_colnames_to_keep_fact = colnames_to_keep_report
)

no_sbf <- setdiff(
  unique(qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260311191047level_2_catch_2026/entities/global_catch_ird_level2_1950_2024/Markdown/rawdata/ancient.qs")$species),
  "SBF"
)

config_level2$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <-
  list(species = no_sbf)

CWP.dataset::summarising_step(
  main_dir = tunaatlas_level2_catch_path,
  connectionDB = con_level2,
  config = config_level2,
  sizepdf = "short",
  savestep = FALSE,
  usesave = FALSE,
  source_authoritylist = "all",
  nameoutput = "noSBF",
  parameter_colnames_to_keep_fact = colnames_to_keep_report
)
# 
# config_level2$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <-
#   list(species_label = c(
#     "Yellowfin tuna", "Skipjack tuna", "Bigeye tuna", "Albacore",
#     "Southern bluefin tuna", "Swordfish", "Tunas nei", "True tunas nei"
#   ))
# 
# CWP.dataset::summarising_step(
#   main_dir = tunaatlas_level2_catch_path,
#   connectionDB = con_level2,
#   config = config_level2,
#   sizepdf = "short",
#   savestep = FALSE,
#   usesave = FALSE,
#   source_authoritylist = "all",
#   nameoutput = "majortunasandTUN",
#   parameter_colnames_to_keep_fact = colnames_to_keep_report
# )
# 
# for (sa in c("all","IOTC", "ICCAT", "CCSBT", "WCPFC", "IATTC")) {
#   CWP.dataset::summarising_step(
#     main_dir = tunaatlas_level2_catch_path,
#     connectionDB = con_level2,
#     config = config_level2,
#     sizepdf = "middle",
#     savestep = identical(sa, "all"),
#     usesave = identical(sa, "all"),
#     source_authoritylist = sa
#   )
# }
# 
# 
# # Ongoing, process fisheries data by species for checks -------------------
# 
# for (entity_dir in entity_dirs) {
#   entity_name <- basename(entity_dir)
#   setwd(here::here(entity_dir))
#   sub_list_dir_2 <- list.files("Markdown", recursive = TRUE, pattern = "data.qs", full.names = TRUE)
#   details <- file.info(sub_list_dir_2)
#   details <- details[with(details, order(as.POSIXct(mtime))), ]
#   sub_list_dir_2 <- rownames(details)
#   flog.info("Processed sub_list_dir_2")
#   sub_list_dir_3 <- gsub("/data.qs", "", sub_list_dir_2)
#   a <- CWP.dataset::process_fisheries_data_by_species(sub_list_dir_3, "catch", specieslist)
#   combined_df <- create_combined_dataframe(a)
#   qflextable(combined_df)
#   # View(combined_df %>% dplyr::select(c(Conversion_factors_kg, Species, Step, Percentage_of_nominal, Step_number)))
#   qs::qsave(x = list(combined_df, a), file = paste0(entity_name,"tablespecies_recap.qs"))
# }
# 
# source("~/firms-gta/geoflow-tunaatlas/R/ongoing_projects/check_georef_vs_nominal_entity.R")
# res <- check_georef_vs_nominal_entity(
#   "~/firms-gta/geoflow-tunaatlas/jobs/20260318080723level_2_catch_2026/entities/global_catch_ird_level2_1950_2024",
#   steps_to_run = 1:33
# )
# 
# georef_sup_nom_analysis_folder <- file.path(tunaatlas_level2_catch_path, "georef_sup_nom_analysis/")
# dir.create(georef_sup_nom_analysis_folder)
# 
# qs::qsave(res, file.path(georef_sup_nom_analysis_folder, "globaldataframesrecap.qs"))
# 
# p <- plot_georef_vs_nominal_evolution(res)
# 
# ggplot2::ggsave(
#   filename = file.path(georef_sup_nom_analysis_folder, "plot_georef_vs_nominal_evolution.png"),
#   plot = p,
#   width = 16,
#   height = 12,
#   dpi = 300
# )


config_level2$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <-
  list(species_label = c(
    "Silky shark"
  ), source_authority = c("IATTC"))

CWP.dataset::summarising_step(
  main_dir = tunaatlas_level2_catch_path,
  connectionDB = con_level2,
  config = config_level2,
  sizepdf = "short",
  savestep = FALSE,
  usesave = FALSE,
  source_authoritylist = "all",
  nameoutput = "SilkyIATTConlynoSKH"
)
