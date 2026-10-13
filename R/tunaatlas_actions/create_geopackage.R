# geoflow action: write a GeoPackage file for each entity
# -----------------------------------------------------------------------------
# Input  : the harmonized CSV of the entity (data/<id>_harmonized.csv)
# Output : data/<id>.gpkg
#
# Two layouts (option "layout"):
#   "relational" (default) : two layers, the geometry is stored only once
#        - <id>      : the data, without geometry
#        - cwp_grid  : the grid cells used by the dataset (spatial layer)
#        join on <id>.geographic_identifier = cwp_grid.code
#   "flat" : one spatial layer <id>, the geometry is repeated on each row
#        (heavier file, but it opens directly as a map)
#
# The CSV only holds the CWP grid code (geographic_identifier), so the
# geometry is taken from a reference grid:
#   - option "grid_file" : any file readable by sf (gpkg, csv with WKT, ...)
#   - otherwise          : table area.cwp_grid of the output database
#
# Options (all optional):
#   layout         "relational" or "flat"
#   grid_file      path to the reference grid file
#   grid_code_col  name of the code column in the grid (default "code")
#   input_file     path to the CSV (default data/<id>_harmonized.csv)
# -----------------------------------------------------------------------------

create_geopackage <- function(action, entity, config) {
  
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required")
  
  opt <- function(name, default = NULL) {
    v <- action$getOption(name)
    if (is.null(v)) default else v
  }
  
  # Logging compatible with the different geoflow versions
  # (config$logger$INFO(), log_info(), or plain message)
  log_msg <- function(level, text) {
    f <- NULL
    old <- config[[paste0("logger.", tolower(level))]]
    lg <- config$logger
    if (is.function(old)) {
      f <- old
    } else if (!is.null(lg) && !is.function(lg) && is.function(lg[[level]])) {
      f <- lg[[level]]
    }
    if (!is.null(f)) f(text) else message(sprintf("[%s] %s", level, text))
  }
  log_info <- function(text) log_msg("INFO", text)
  log_warn <- function(text) log_msg("WARN", text)
  
  dataset_id <- entity$identifiers[["id"]]
  input_file <- opt("input_file", file.path("data", paste0(dataset_id, "_harmonized.csv")))
  output_file <- file.path("data", paste0(dataset_id, ".gpkg"))
  code_col <- opt("grid_code_col", "code")
  layout <- opt("layout", "relational")
  if (!layout %in% c("relational", "flat")) {
    stop("GeoPackage: option 'layout' must be 'relational' or 'flat'")
  }
  
  if (!file.exists(input_file)) {
    stop(sprintf("GeoPackage: input file '%s' not found", input_file))
  }
  
  # --- 1. Reference grid -----------------------------------------------------
  grid_file <- opt("grid_file")
  if (!is.null(grid_file)) {
    log_info(sprintf("GeoPackage: reading grid from file '%s'", grid_file))
    grid <- sf::st_read(grid_file, quiet = TRUE)
  } else {
    con <- config$software$output$dbi
    if (is.null(con)) {
      stop("GeoPackage: no 'grid_file' option and no output database connection")
    }
    log_info("GeoPackage: reading grid from area.cwp_grid")
    grid <- sf::st_read(con, query = sprintf(
      "SELECT %s AS code, geom FROM area.cwp_grid", code_col), quiet = TRUE)
    code_col <- "code"
  }
  if (is.na(sf::st_crs(grid))) sf::st_crs(grid) <- 4326
  grid_codes <- as.character(grid[[code_col]])
  grid_geom <- sf::st_cast(sf::st_geometry(grid), "MULTIPOLYGON")
  
  # --- 2. Data ---------------------------------------------------------------
  # Everything is read as text so that codes are kept as written in the CSV
  # (e.g. gear type "09.32" must not become the number 9.32). Only the
  # measurement value and the dates are then converted.
  df <- data.table::fread(input_file, colClasses = "character",
                          na.strings = c("", "NA"), data.table = FALSE)
  if (!"geographic_identifier" %in% names(df)) {
    stop("GeoPackage: column 'geographic_identifier' not found in the CSV")
  }
  if ("measurement_value" %in% names(df)) {
    df$measurement_value <- as.numeric(df$measurement_value)
  }
  for (col in intersect(c("time_start", "time_end"), names(df))) {
    df[[col]] <- as.Date(substr(df[[col]], 1, 10))
  }
  df$geom_wkt <- NULL
  
  idx <- match(df$geographic_identifier, grid_codes)
  n_missing <- sum(is.na(idx))
  if (n_missing > 0) {
    log_warn(sprintf(
      "GeoPackage: %s rows out of %s have no geometry in the grid",
      n_missing, nrow(df)))
  }
  
  # --- 3. Write --------------------------------------------------------------
  if (file.exists(output_file)) unlink(output_file)
  
  if (layout == "relational") {
    # Data table without geometry + grid restricted to the cells in use
    sf::st_write(df, output_file, layer = dataset_id, quiet = TRUE)
    used <- sort(unique(idx[!is.na(idx)]))
    grid_used <- sf::st_sf(code = grid_codes[used], geometry = grid_geom[used])
    sf::st_write(grid_used, output_file, layer = "cwp_grid", quiet = TRUE)
  } else {
    # One spatial layer; rows without a grid cell get an empty geometry
    if (n_missing > 0) {
      empty <- sf::st_sfc(sf::st_multipolygon(), crs = sf::st_crs(grid_geom))
      grid_geom <- c(grid_geom, empty)
      idx[is.na(idx)] <- length(grid_geom)
    }
    sf_data <- sf::st_sf(df, geometry = grid_geom[idx])
    sf::st_write(sf_data, output_file, layer = dataset_id, quiet = TRUE)
  }
  
  log_info(sprintf("GeoPackage: '%s' written (%s rows, layout '%s')",
                   output_file, nrow(df), layout))
  entity$addResource("geopackage", output_file)
}