#!/usr/bin/env Rscript

# ============================================================
# CCP-compliant entrypoint
# - --help / -h
# - exit codes: 0 ok, 1 usage/config error, 2 runtime error
# - robust CLI parsing, validation, clean logging
# ============================================================

EXIT_OK      <- 0L
EXIT_USAGE   <- 1L
EXIT_RUNTIME <- 2L
args <- commandArgs(trailingOnly = TRUE)

# main <- function(args = commandArgs(trailingOnly = TRUE)) {
show_help <- function() {
  cat(
    "NetCDF export (CCP compliant)

USAGE:
  Rscript /app/scripts/run_export.R [options]

REQUIRED:
  --path_to_dataset <file.qs>     Path to input dataset (.qs)

OPTIONAL:
  --path <dir>                   Output directory (default: data)
  --name <string>                Base name used in output identifiers (default: dataset)

  --dimensions <spec>            'all' | 'no' | comma-separated list
                                 Example: --dimensions gear_type_label,fishing_fleet_label
                                 Default: no

  --parameter_filtering <spec>   Semicolon-separated filters:
                                 col=a,b;col2=c
                                 Use NULL to skip: species=NULL
                                 Default: species=NULL;fishing_fleet=NULL

  --one_by_species <bool>         If true, loop and export one file per species
                                 Accepts: true/false/1/0/yes/no (default: false)

  --estimate_time <bool>          Estimate runtime before writing (default: false)
  --estimate_n <int>              Number of layers sampled for estimate (default: 100)
  --confirm <bool>                Ask user confirmation after estimate (default: false)

  --compression <int>             NetCDF compression level (default: 9)
  --log_file <path>               Append logs to this file (default: <path>/export.log)
  --verbose <bool>                More logs (default: true)

  --dry_run <bool>                Validate, filter, print plan; do not export (default: false)

HELP:
  -h, --help                     Show this help and exit

EXIT CODES:
  0  success
  1  usage/config error
  2  runtime error
",
    sep = ""
  )
}

source(here::here("R/netcdf_creation/mapping_cwp_to_raster.R"))

cwp_grid_file <- here::here("data/cl_areal_grid.csv")
zip_url  <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
zip_path <- here::here("data/cl_areal_grid.zip")

# S'assurer que le dossier data existe
dir.create(dirname(cwp_grid_file), recursive = TRUE, showWarnings = FALSE)

if (file.exists(cwp_grid_file)) {
  message("✔ cl_areal_grid.csv existing")
  
} else if (file.exists(zip_path)) {
  message("📦 cl_areal_grid.zip existing")
  unzip(zip_path, exdir = dirname(cwp_grid_file))
  
} else {
  message("⬇ cl_areal_grid.csv notexisting, downloading")
  download.file(zip_url, zip_path, mode = "wb", quiet = TRUE)
  unzip(zip_path, exdir = dirname(cwp_grid_file))
}

cwp_grid <- sf::st_read(cwp_grid_file) 

default_dims <- c("no")

get_bool <- function(flag, default = FALSE) {
  if (!has_arg(flag)) return(default)
  val <- get_arg(flag, default = NA_character_)
  if (is.na(val) || val == "") return(TRUE)  # si juste "--flag" => TRUE
  val <- tolower(trimws(val))
  val %in% c("1","true","t","yes","y")
}

one_by_species <- get_bool("--one_by_species", default = FALSE)

dimensions_raw <- get_arg("--dimensions", NA)

path_to_dataset <- get_arg("--path_to_dataset", "data/global_catch_tunaatlasird_level2_1950_2023.qs")
path       <- get_arg("--path", "outputs")
name     <- get_arg("--name", "")

parameter_filtering_raw <- get_arg(
  "--parameter_filtering",
  "species=NULL;fishing_fleet=NULL"
)

if (!has_arg("--path_to_dataset") && (is.na(name) || name == "")) {
  name <- "Globalcatchlevel219502023"
}

# (optional) also handle the case where user explicitly sets path_to_dataset to empty
if (has_arg("--path_to_dataset") && (is.na(path_to_dataset) || path_to_dataset == "")) {
  stop("--path_to_dataset was provided but is empty.")
}

parse_dimensions <- function(x, default_dims) {
  if (is.na(x) || x == "") {
    return(default_dims)
  }
  if (x == "all") {
    return("all")
  }
  if (x == "no") {
    return(character(0))
  }
  strsplit(x, ",")[[1]]
}

dims <- parse_dimensions(dimensions_raw, default_dims)
require(dplyr)
require(stringr)
parameter_filtering <- parse_parameter_filtering(parameter_filtering_raw)
safe_filter <- function(df, filters) {
  if (!length(filters)) return(df)
  
  # stop si colonnes dupliquées
  dup <- names(df)[duplicated(names(df))]
  if (length(dup)) {
    stop("Duplicated columns in dataset: ", paste(unique(dup), collapse = ", "))
  }
  
  for (nm in names(filters)) {
    if (!nm %in% names(df)) {
      stop("Unknown filter column: ", nm)
    }
    df <- df[df[[nm]] %in% filters[[nm]], , drop = FALSE]
  }
  
  df
}
if (isTRUE(one_by_species)) dims <- setdiff(dims, "species_label")
message("parameter_filtering = ",
        paste(names(parameter_filtering), collapse = ", "))
print(parameter_filtering)

dataset <- qs::qread(path_to_dataset)
dataset$geom <- NULL #removign geom column sinon trop de problemes
dataset$geom_wkt <- NULL#removign geom column sinon trop de problemes


dataset_filtered <- safe_filter(dataset, parameter_filtering)
message("n before = ", nrow(dataset),
        " | n after = ", nrow(dataset_filtered))
# to do, create .nc with two variables, catch_t and catch_no in the same .nc, not done yet

# dataset_filtered <- as.data.frame(CWP.dataset::filtering_function(dataset, parameter_filtering = parameter_filtering)) #a etster ça peut marche si on charge pas plyr
message("dims = ", paste(dims, collapse = ", "))
message("filtering = ", paste(parameter_filtering, collapse = ", "))
message("cols dataset_filtered = ", paste(head(names(dataset_filtered), 50), collapse = ", "))
# if (is.character(dims) && !identical(dims, "all")) {
#   missing <- setdiff(dims, names(dataset_filtered))
#   if (length(missing) > 0) {
#     stop(
#       "Some dims are not columns in dataset_filtered: ",
#       paste(missing, collapse = ", "),
#       "\nAvailable columns include: ",
#       paste(head(names(dataset_filtered), 40), collapse = ", "),
#       if (length(names(dataset_filtered)) > 40) " ..."
#     )
#   }
# }
out <- export_netcdf_by_unit_and_resolution(
  dataset_filtered = dataset_filtered,
  name = name,
  dims = dims,
  path = path,
  compression = 9,
  estimate_time = FALSE,
  confirm = FALSE,
  log_file = file.path(path, "export.log"),
  verbose = TRUE,
  cwp_grid = cwp_grid,                     
  cwp_code_col = "geographic_identifier", 
  one_by_species = one_by_species
)



# }

# if (sys.nframe() == 0) {
#   main()
# }

# args <- c(
#   "--path_to_dataset", "global_catch_tunaatlasird_level2_1950_2023.qs",
#   "--path", "data",
#   "--name", "test",
#   "--dimensions", "species_label",
#   "--parameter_filtering", "species=SBF"
# )
# 
# main(args)

