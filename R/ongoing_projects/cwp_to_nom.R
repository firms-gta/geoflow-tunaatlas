library(sf)
library(futile.logger)

# Load 'renv' for project-specific environments
# if (!require("renv")) install.packages("renv")
library(renv)
# Activate the project environment (if using project-specific libraries)
# renv::activate()
# Restore the project library (if using renv)
renv::restore()

# Define all required packages (excluding 'base' and 'utils' as they are always available)
required_packages <- c(
  "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor", 
  "dotenv", "data.table", "here", "xfun", "RPostgreSQL", "RPostgres", "DBI", 
  "rpostgis", "terra", "sf", "RSQLite", "webshot", "usethis", "ows4R", "sp", 
  "flextable", "dplyr", "stringr", "tibble", "bookdown", "knitr", 
  "purrr", "readxl", "odbc", "rlang", "kableExtra", "tidyr", "ggplot2", 
  "stats", "RColorBrewer", "cowplot", "tmap", "curl", "officer", 
  "gdata", "R3port", "reshape2", "tools", "plogr", "futile.logger", "lubridate", "data.table"
)

# Function to check, install (if necessary), and load a package
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    # install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Apply the function to each required package
sapply(required_packages, install_and_load)

require(geoflow)


executeAndRename <- function(executed_file, suffix) {
  
  # Derive folder and file names
  folder_file <- file.path("jobs", basename(executed_file))
  
  # Rename the file with the given suffix
  file.rename(folder_file, paste0("jobs/", basename(executed_file), suffix))
  return(paste0("jobs/", basename(executed_file), suffix))
}

# Note: This script assumes that the internet connection is available and
# the CRAN/GitHub repositories are accessible for package installation.

# Choose your .env in which you have stored you password for googledrive (if wanted) and database (mandatory)

default_file <- ".env"

if(file.exists(here::here("geoserver_sdi_lab.env"))){
  default_file <- "geoserver_sdi_lab.env"
} # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

# if(file.exists(here("geoserver_cines.env"))){
#   default_file <- here("geoserver_cines.env")
# } # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

load_dot_env(file = here::here(default_file)) # to be replaced by the one used
# load_dot_env(file = "~/Documents/Tunaatlas_level1/catch_local.env")# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/cwp_grids.R")

running_time_of_workflow <- function(folder){
  # Get the last modified times of the files
  json_time <- file.info(file.path(folder, "job.json"))$mtime
  txt_time <- file.info(file.path(folder, "job-logs.txt"))$mtime
  
  # Calculate the difference
  time_difference <- txt_time - json_time
  
  return(time_difference)
}
lvl0 <- qs::qread("~/firms-gta/geoflow-tunaatlas/data/firms_level0_dataset.qs")
nominal <- readr::read_csv("data/global_nominal_catch_firms_level0_harmonized.csv")
nominal_strata <- nominal %>% dplyr::select(c(source_authority, geographic_identifier)) %>% dplyr::distinct() %>% 
  dplyr::mutate(geographic_identifier = ifelse(source_authority == "CCSBT" & geographic_identifier == "WCPO","WCPFC",geographic_identifier ))

cl_areal_grid <- read_csv("data/cl_areal_grid.csv")

global_nominal_catch_firms_level0_20241022092211 <- read_csv("data/global_nominal_catch_firms_level0_20241022092211.csv")
global_nominal_catch_firms_level0_20241022092211 <- st_as_sf(global_nominal_catch_firms_level0_20241022092211, wkt = "geometry")
# Define the function with logging and geometry validation

inner_join_data <- inner_join(cl_areal_grid %>%dplyr::select(code, geom_wkt) %>%  dplyr::mutate(code = as.character(code)),
                         lvl0 %>% dplyr::select(geographic_identifier, source_authority), 
     by = c("code" = "geographic_identifier")) %>% dplyr::distinct() %>% dplyr::inner_join(nominal_strata %>% 
    dplyr::rename(geographic_identifiernominal = geographic_identifier))


# Define the function using st_intersects, st_contains, and calculating percentage of inclusion
check_geometries_intersection_and_inclusion <- function(df1, inner_join, log_level = "INFO") {
  # Set log level
  flog.threshold(log_level)
  
  # Start overall timer
  overall_start <- Sys.time()
  flog.info("Starting function check_geometries_intersection_and_inclusion")
  
  # Convert df1 and inner_join to sf objects, assuming geometry columns are named 'geometry' and 'geom_wkt'
  flog.info("Converting data frames to sf objects")
  df1_sf <- st_as_sf(df1, wkt = "geometry")
  inner_join_sf <- st_as_sf(inner_join, wkt = "geom_wkt")
  
  # Check and fix invalid geometries in df1
  flog.info("Checking for invalid geometries in df1")
  df1_invalid <- !st_is_valid(df1_sf)
  if (any(df1_invalid)) {
    flog.warn("Found %d invalid geometries in df1. Attempting to make them valid.", sum(df1_invalid))
    df1_sf[df1_invalid, ] <- st_make_valid(df1_sf[df1_invalid, ])
  }
  
  # Check and fix invalid geometries in inner_join
  flog.info("Checking for invalid geometries in inner_join")
  inner_join_invalid <- !st_is_valid(inner_join_sf)
  if (any(inner_join_invalid)) {
    flog.warn("Found %d invalid geometries in inner_join. Attempting to make them valid.", sum(inner_join_invalid))
    inner_join_sf[inner_join_invalid, ] <- st_make_valid(inner_join_sf[inner_join_invalid, ])
  }
  
  flog.info("Finished converting and validating data frames")
  
  # Initialize vectors for TRUE/FALSE results and percentage inclusion
  intersection_results <- logical(nrow(inner_join_sf))
  inclusion_percentage <- numeric(nrow(inner_join_sf))
  
  # Loop through each row in inner_join and check intersection or containment with df1
  flog.info("Starting intersection/inclusion check for each geometry")
  for (i in seq_len(nrow(inner_join_sf))) {
    # Start timer for each iteration
    iteration_start <- Sys.time()
    
    # Filter df1 based on the geographic_identifiernominal in the current row
    geographic_identifier <- inner_join_sf$geographic_identifiernominal[i]
    df1_filtered <- df1_sf[df1_sf$geographic_identifier == geographic_identifier, ]
    
    # Check for intersection or containment
    intersects <- st_intersects(df1_filtered, inner_join_sf[i, ], sparse = FALSE)[, 1]
    contains <- st_contains(df1_filtered, inner_join_sf[i, ], sparse = FALSE)[, 1]
    
    # Assign TRUE if either intersects or contains is TRUE
    intersection_results[i] <- any(intersects) | any(contains)
    
    # Calculate the percentage of inclusion if there's an intersection or containment
    if (any(intersects)) {
      # Calculate area of the geometry in inner_join
      inner_area <- st_area(inner_join_sf[i, ])
      
      # Calculate the area of the intersection
      intersection_geom <- st_intersection(df1_filtered[intersects, ], inner_join_sf[i, ])
      intersection_area <- st_area(intersection_geom)
      
      # Calculate percentage inclusion
      inclusion_percentage[i] <- as.numeric(intersection_area) / as.numeric(inner_area) * 100
    } else {
      inclusion_percentage[i] <- 0
    }
    
    # Log time for this iteration
    iteration_end <- Sys.time()
    flog.info("Processed row %d in %.2f seconds", i, as.numeric(difftime(iteration_end, iteration_start, units = "secs")))
  }
  
  # Add the results as new columns in the original inner_join
  inner_join_sf$geometry_match <- intersection_results
  inner_join_sf$inclusion_percentage <- inclusion_percentage
  
  # End overall timer
  overall_end <- Sys.time()
  flog.info("Function completed in %.2f seconds", as.numeric(difftime(overall_end, overall_start, units = "secs")))
  
  return(inner_join_sf)
}

result_df <- check_geometries_intersection_and_inclusion(df1 = global_nominal_catch_firms_level0_20241022092211, inner_join = inner_join_data)
qs::qsave(result_df, here::here("data/result_df.qs"))

result_df_included <- result_df %>% dplyr::filter(geometry_match) %>% dplyr::group_by(code, source_authority) %>% dplyr::mutate(ndist = n_distinct(inclusion_percentage)) %>% 
  dplyr::filter(ndist | inclusion_percentage != 0) %>%
  dplyr::group_by(code, source_authority) %>% 
  dplyr::mutate(ndistinct = n_distinct(geographic_identifiernominal))

qs::qsave(result_df_included, "data/result_df_included.qs")

ok <- result_df_included %>% dplyr::filter(ndistinct ==1)

bizar <- result_df_included%>% dplyr::filter(ndistinct !=1)

#dans les bizzares pour IOTC etc.. c'est quand même obvious lequel est dans lequel, pour ICCAT pas sûr

prioritize_wcp0 <- function(df) {
  # Check if WCP0 and WCPFC are both present with the same inclusion percentage
  tied_wcp <- df %>%
    filter(geographic_identifiernominal %in% c("WCP0", "WCPFC")) %>%
    arrange(desc(inclusion_percentage))

  # If there's a tie between WCP0 and WCPFC, prioritize WCP0
  if (nrow(tied_wcp) == 2 && tied_wcp$inclusion_percentage[1] == tied_wcp$inclusion_percentage[2]) {
    # Prioritize WCP0 by setting it as the first
    if (tied_wcp$geographic_identifiernominal[1] == "WCPFC" && tied_wcp$geographic_identifiernominal[2] == "WCP0") {
      df <- df %>%
        mutate(geographic_identifiernominal = ifelse(geographic_identifiernominal == "WCPFC", "WCP0", geographic_identifiernominal))
    }
  }

  return(df)
}

# Process the data to find the top and second top inclusion percentages for each group
processed_data <- bizar %>%
  group_by(code, source_authority) %>%
  arrange(desc(inclusion_percentage)) %>%
  mutate(
    top_geographic_identifier = first(geographic_identifiernominal),
    top_inclusion_percentage = first(inclusion_percentage),
    second_geographic_identifier = nth(geographic_identifiernominal, 2, default = NA),
    second_inclusion_percentage = nth(inclusion_percentage, 2, default = NA)

  ) %>%
  # Apply WCP0 prioritization for ties
  do(prioritize_wcp0(.)) %>%
  ungroup()

# Check the results
processed_data <- processed_data %>%
  select(code, source_authority, top_geographic_identifier, second_geographic_identifier) %>% dplyr::distinct()

ok <- ok %>% dplyr::select(code, source_authority,top_geographic_identifier= geographic_identifiernominal)
ok$geom_wkt <- NULL
ok$second_geographic_identifier <- NA

final <- rbind(ok, processed_data) %>% dplyr::select(-second_geographic_identifier)

final_final <- final %>% dplyr::filter(!is.na(top_geographic_identifier))
qs::qsave(final, here::here("data/geographic_identifier_to_nominal.qs"))

plulvl0 <- lvl0  %>% dplyr::left_join(final, by = c("geographic_identifier" = "code", "source_authority")) %>%
  dplyr::rename(geographic_identifier_nom = top_geographic_identifier) %>% 
  dplyr::filter(is.na(geographic_identifier_nom))

plulvl0 <- plulvl0 %>% dplyr::left_join(cl_areal_grid %>% dplyr::mutate(code = as.character(code)) %>% 
                                          dplyr::select(code, geom_wkt), by = c("geographic_identifier" = "code"))

tm_shape(st_as_sf(plulvl0, wkt = "geom_wkt"))+tm_polygons()+tm_shape(global_nominal_catch_firms_level0_20241022092211 %>% dplyr::filter(geographic_identifier %in% c("WCPFC")))+tm_polygons(alpha = 0)

# On remarque pour 2 carrés, un IOTC et un IATTC cest une errreur dans le st_intersect ou plutôt dans les shape, on les associe au bon. 
# pour tous les carrés CCSBT qui ont rine c'est en fait quils sont WCPFC mais c'est pas la bonne shape. Le reste on enlève c'est out de la juridiction. pou rle moment
df <- data.frame(
  code = c(5233022, 5304080),
  source_authority = c("IOTC","IATTC"),  # Mettre NA pour les valeurs manquantes
  geographic_identifier_nom = c("IOTC_WEST", "EPO")
)


ccsbt <- plulvl0 %>% dplyr::select(source_authority, geographic_identifier) %>% dplyr::mutate(geographic_identifier_nom = "WCPFC") %>% dplyr::rename(code = geographic_identifier)

final_problematic <- rbind(df, ccsbt)

final_filtered <- rbind(final   %>% dplyr::rename(geographic_identifier_nom = top_geographic_identifier) , final_problematic)

qs::qsave(final_filtered, here::here("data/geographic_identifier_to_nominal.qs"))



# result_df <- find_geographic_identifier_with_authority(df1 = global_nominal_catch_firms_level0_20241022092211, df2 = inner_join, nominal_strata = nominal_strata)
# qs::qsave(result_df, "data/geographic_identifier_to_nominal.qs")

# zerorow <- result_df %>% dplyr::select(code, geographic_identifier_match) %>% dplyr::distinct() %>% dplyr::group_by(code) %>% 
#   dplyr::mutate(n = n_distinct(geographic_identifier_match)) %>% dplyr::filter(n != 1)
# 
# new_lvl0 <- lvl0 %>% dplyr::inner_join(result_df %>% dplyr::select(code, geographic_identifier_match), by = c("geographic_identifier" = "code"))

# t <- result_df %>% dplyr::mutate(new = case_when(geographic_identifier_match == "AT, AT-SE" ~ "AT-SE", 
#                                                  geographic_identifier_match == "IOTC_WEST, IOTC" ~ "IOTC_WEST", 
#                                                  geographic_identifier_match == "IOTC_EAST, IOTC" ~ "IOTC_EAST", 
#                                                  geographic_identifier_match == "AT, AT-NE" ~ "AT-NE", 
#                                                  geographic_identifier_match == "IOTC_WEST, IOTC" ~ "IOTC_WEST", 
#                                                  geographic_identifier_match == "IOTC_WEST, IOTC" ~ "IOTC_WEST", 
#                                                  geographic_identifier_match == "IOTC_WEST, IOTC" ~ "IOTC_WEST", 
#                                                  geographic_identifier_match == "IOTC_WEST, IOTC" ~ "IOTC_WEST", 
# ))
# inner_join <- st_as_sf(inner_join, wkt = "geom_wkt")
# global_nominal_catch_firms_level0_20241022092211 <- st_as_sf(global_nominal_catch_firms_level0_20241022092211, wkt = "geometry")
# Assign CRS to the global_nominal_catch_firms_level0_20241022092211 dataframe
# st_crs(global_nominal_catch_firms_level0_20241022092211) <- 4326
# 
# # Assign CRS to the inner_join dataframe (if needed)
# st_crs(inner_join) <- 4326
# 
# st_write(global_nominal_catch_firms_level0_20241022092211, con, "area.cwp_geo_nominal", layer_options = "OVERWRITE=YES")
# st_write(inner_join, con, "area.cwp_geo_level0", layer_options = "OVERWRITE=YES")
# 
# dbExecute(con, "
#   CREATE MATERIALIZED VIEW area.mv_cwp_geo_nominal AS
#   SELECT * FROM area.cwp_geo_nominal;
# ")
# dbExecute(con, "
#   CREATE MATERIALIZED VIEW area.mv_cwp_geo_level0 AS
#   SELECT * FROM area.cwp_geo_level0;
# ")
