# geoflow action: write a GeoParquet file for each entity
# -----------------------------------------------------------------------------
# Input  : the harmonized CSV of the entity (data/<id>_harmonized.csv)
# Output : data/<id>.parquet (GeoParquet, EPSG:4326)
#
# The CSV only holds the CWP grid code (geographic_identifier), so the
# geometry is taken from a reference grid:
#   - option "grid_file" : any file readable by sf (gpkg, csv with WKT, ...)
#   - otherwise          : table area.cwp_grid of the output database
#
# Options (all optional):
#   grid_file      path to the reference grid file
#   grid_code_col  name of the code column in the grid (default "code")
#   input_file     path to the CSV (default data/<id>_harmonized.csv)
# -----------------------------------------------------------------------------

create_geoparquet <- function(action, entity, config) {
  
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
  output_file <- file.path("data", paste0(dataset_id, ".parquet"))
  code_col <- opt("grid_code_col", "code")
  
  if (!file.exists(input_file)) {
    stop(sprintf("GeoParquet: input file '%s' not found", input_file))
  }
  
  # --- 1. Reference grid -----------------------------------------------------
  grid_file <- opt("grid_file")
  if (!is.null(grid_file)) {
    log_info(sprintf("GeoParquet: reading grid from file '%s'", grid_file))
    grid <- sf::st_read(grid_file, quiet = TRUE)
  } else {
    con <- config$software$output$dbi
    if (is.null(con)) {
      stop("GeoParquet: no 'grid_file' option and no output database connection")
    }
    log_info("GeoParquet: reading grid from area.cwp_grid")
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
    stop("GeoParquet: column 'geographic_identifier' not found in the CSV")
  }
  if ("measurement_value" %in% names(df)) {
    df$measurement_value <- as.numeric(df$measurement_value)
  }
  for (col in intersect(c("time_start", "time_end"), names(df))) {
    df[[col]] <- as.Date(substr(df[[col]], 1, 10))
  }
  
  # --- 3. Attach the geometry ------------------------------------------------
  # Rows without a matching grid cell are kept, with a missing geometry
  idx <- match(df$geographic_identifier, grid_codes)
  n_missing <- sum(is.na(idx))
  if (n_missing > 0) {
    log_warn(sprintf(
      "GeoParquet: %s rows out of %s have no geometry in the grid",
      n_missing, nrow(df)))
  }
  df$geom_wkt <- NULL
  
  # --- 4. Write --------------------------------------------------------------
  # The file is written with arrow, with GeoParquet 1.0.0 metadata:
  #   - geometry encoded as WKB, missing geometries stored as nulls
  #   - no "crs" key: by specification this means OGC:CRS84 (WGS 84, lon/lat)
  if (!requireNamespace("arrow", quietly = TRUE)) stop("Package 'arrow' is required")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required")
  
  # WKB is computed once per grid cell, then spread over the rows
  grid_wkb <- unclass(sf::st_as_binary(grid_geom))
  wkb <- vector("list", nrow(df))
  has_geom <- !is.na(idx)
  wkb[has_geom] <- grid_wkb[idx[has_geom]]
  
  tbl <- arrow::arrow_table(df)
  tbl <- tbl$AddColumn(tbl$num_columns, arrow::field("geometry", arrow::binary()),
                       arrow::chunked_array(arrow::Array$create(wkb, type = arrow::binary())))
  
  geo_column <- list(encoding = "WKB", geometry_types = list("MultiPolygon"))
  if (any(has_geom)) {
    bb <- sf::st_bbox(grid_geom[unique(idx[has_geom])])
    geo_column$bbox <- as.numeric(bb[c("xmin", "ymin", "xmax", "ymax")])
  }
  geo <- list(version = "1.0.0", primary_column = "geometry",
              columns = list(geometry = geo_column))
  meta <- tbl$metadata
  meta$geo <- as.character(jsonlite::toJSON(geo, auto_unbox = TRUE, digits = NA))
  tbl$metadata <- meta
  
  if (file.exists(output_file)) unlink(output_file)
  arrow::write_parquet(tbl, output_file)
  
  log_info(sprintf("GeoParquet: '%s' written (%s rows, %s with geometry)",
                   output_file, nrow(df), sum(has_geom)))
  entity$addResource("geoparquet", output_file)
}