`%||%` <- function(x, y) if (!is.null(x)) x else y
args <- commandArgs(trailingOnly = TRUE)
has_arg <- function(flag) {
  any(args == flag)
}
get_arg <- function(flag, default = NULL) {
  idx <- match(flag, args)
  if (!is.na(idx) && length(args) >= idx + 1) args[idx + 1] else default
}

# ------------------------------------------------------------
# Build a fast cell_index from your cwp_grid for a given resolution
# ------------------------------------------------------------
build_cell_index_from_cwp_grid <- function(cwp_grid, sp_resolution) {
  stopifnot(is.data.frame(cwp_grid))
  needed <- c("CWP_CODE", "X_COORD", "Y_COORD", "GRIDTYPE")
  miss <- setdiff(needed, names(cwp_grid))
  if (length(miss)) stop("cwp_grid is missing columns: ", paste(miss, collapse = ", "))
  
  # Filter rows by resolution using GRIDTYPE (adjust patterns if needed)
  gridtype_ok <- switch(
    as.character(sp_resolution),
    "1"  = grepl("1deg",  cwp_grid$GRIDTYPE, ignore.case = TRUE),
    "5"  = grepl("5deg",  cwp_grid$GRIDTYPE, ignore.case = TRUE),
    "10" = grepl("10deg", cwp_grid$GRIDTYPE, ignore.case = TRUE),
    grepl(paste0("\\b", sp_resolution, "deg\\b"), cwp_grid$GRIDTYPE, ignore.case = TRUE)
  )
  
  g <- cwp_grid[gridtype_ok, c("CWP_CODE", "X_COORD", "Y_COORD", "GRIDTYPE"), drop = FALSE]
  if (!nrow(g)) stop("No rows in cwp_grid match sp_resolution=", sp_resolution)
  
  # Axes from centers (these are clean; better than st_centroid)
  lon_vals <- sort(unique(as.numeric(g$X_COORD)))
  lat_vals <- sort(unique(as.numeric(g$Y_COORD)))
  nlon <- length(lon_vals)
  nlat <- length(lat_vals)
  
  # Map center coords to indices
  col <- match(as.numeric(g$X_COORD), lon_vals)
  row_geo <- match(as.numeric(g$Y_COORD), lat_vals)
  if (anyNA(col) || anyNA(row_geo)) stop("Failed to map some X_COORD/Y_COORD to axis indices.")
  
  # NetCDF row convention (row=1 at max latitude)
  row_nc <- nlat - row_geo + 1L
  
  cell_index <- data.frame(
    cwp_code = as.integer(g$CWP_CODE),
    row = as.integer(row_nc),
    col = as.integer(col),
    stringsAsFactors = FALSE
  )
  
  # Ensure unique mapping (row,col)
  key <- paste(cell_index$row, cell_index$col, sep = "_")
  if (anyDuplicated(key)) {
    dups <- unique(key[duplicated(key)])
    stop("Duplicate (row,col) in cwp_grid mapping for res=", sp_resolution,
         ". Example keys: ", paste(head(dups, 10), collapse = ", "))
  }
  
  # bbox for metadata (derived from centers)
  box <- c(
    xmin = min(lon_vals) - sp_resolution/2,
    xmax = max(lon_vals) + sp_resolution/2,
    ymin = min(lat_vals) - sp_resolution/2,
    ymax = max(lat_vals) + sp_resolution/2
  )
  
  list(
    cell_index = cell_index,
    longitudeVector = lon_vals,
    latitudeVector  = lat_vals,
    box = box
  )
}
export_netcdf_by_unit_and_resolution <- function(
    dataset_filtered,
    name,
    dims = c("gear_type_label", "species_label", "fishing_fleet_label"),
    path = "data",
    compression = 9,
    estimate_time = FALSE,
    estimate_n = 10,
    confirm = interactive(),
    log_file = NULL,
    verbose = TRUE,
    cwp_grid = NULL,
    cwp_code_col = NULL,
    one_by_species = FALSE,
    ...
) {
  stopifnot(is.data.frame(dataset_filtered))
  stopifnot(all(c("geographic_identifier", "measurement_unit") %in% names(dataset_filtered)))
  
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  # ---- logger (console + file) ----
  log_line <- function(fmt, ...) {
    if (!isTRUE(verbose)) return(invisible(NULL))
    line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", sprintf(fmt, ...))
    cat(line, "\n")
    if (!is.null(log_file) && nzchar(log_file)) {
      cat(line, file = log_file, append = TRUE, sep = "\n")
    }
    invisible(NULL)
  }
  
  # ---- split by resolution (prefix mapping fixed) ----
  split_by_resolution <- function(df) {
    gid_chr <- as.character(df$geographic_identifier)
    list(
      `1deg` = df[startsWith(gid_chr, "5"), , drop = FALSE],
      `5deg` = df[startsWith(gid_chr, "6"), , drop = FALSE]
    )
  }
  
  # ---- core writer: one dataset -> files by unit x resolution ----
  core_write_unit_res <- function(df_in, name_tag) {
    
    # helper: write one netcdf
    write_one <- function(df, file_tag) {
      if (!nrow(df)) return(NA_character_)
      md <- make_dataset_metadata(
        df,
        identifier = paste0(name_tag, "_", file_tag),
        fillvalue = -9999,
        cwp_code_col = cwp_code_col
      )
      
      convert_to_netcdf_simple(
        dataset = df,
        dataset_metadata = md,
        dimensions = dims,
        path = path,
        compression = compression,
        title = paste0(name_tag, "_", file_tag),
        summary = NULL,
        source = "local dataset",
        estimate_time = estimate_time,
        estimate_n = estimate_n,
        confirm = confirm,
        log_file = log_file,
        verbose = verbose,
        cwp_grid = cwp_grid,
        cwp_code_col = cwp_code_col,
        ...
      )
    }
    
    # split by measurement_unit
    units <- unique(as.character(df_in$measurement_unit))
    units <- units[!is.na(units) & nzchar(units)]
    if (!length(units)) stop("No non-NA measurement_unit found.")
    
    res <- list()
    
    for (u in units) {
      df_u <- df_in[df_in$measurement_unit == u, , drop = FALSE]
      df_split <- split_by_resolution(df_u)
      
      u_tag <- gsub("[^A-Za-z0-9_]+", "_", u)
      
      tag_1 <- paste0(u_tag, "_1deg")
      tag_5 <- paste0(u_tag, "_5deg")
      
      log_line("Export unit=%s | 1deg n=%s | 5deg n=%s",
               u, format(nrow(df_split$`1deg`), big.mark=" "),
               format(nrow(df_split$`5deg`), big.mark=" "))
      
      out1 <- write_one(df_split$`1deg`, tag_1)
      out5 <- write_one(df_split$`5deg`, tag_5)
      
      res[[u]] <- list(`1deg` = out1, `5deg` = out5)
    }
    
    res
  }
  
  # ---- main behaviour: optionally split by species ----
  if (!isTRUE(one_by_species)) {
    log_line("Mode: single export (no species loop) | name=%s", name)
    return(invisible(core_write_unit_res(dataset_filtered, name)))
  }
  
  # one_by_species = TRUE
  if (!("species" %in% names(dataset_filtered))) {
    stop("one_by_species=TRUE but dataset has no 'species' column.")
  }
  
  species_list <- sort(unique(as.character(dataset_filtered$species)))
  species_list <- species_list[!is.na(species_list) & nzchar(species_list)]
  if (!length(species_list)) stop("No non-NA species values found.")
  
  log_line("Mode: one file-set per species | n_species=%d", length(species_list))
  
  out_all <- vector("list", length(species_list))
  names(out_all) <- species_list
  
  for (sp in species_list) {
    df_sp <- dataset_filtered[as.character(dataset_filtered$species) == sp, , drop = FALSE]
    sp_name <- paste0(name, "_", sp)
    log_line("[START] species=%s | n=%s", sp, format(nrow(df_sp), big.mark=" "))
    out_all[[sp]] <- core_write_unit_res(df_sp, sp_name)
    log_line("[DONE ] species=%s", sp)
  }
  
  invisible(out_all)
}

# ------------------------------------------------------------
# convert_to_netcdf_simple (FAST spatial path + logs + estimate)
# ------------------------------------------------------------
convert_to_netcdf_simple <- function(
    dataset,
    dataset_metadata,
    dimensions = "all",              # "all" | "no" | c("gear_type","fishing_fleet",...)
    path = "data",
    specie = "all",
    compression = 9,
    title = NULL,
    summary = NULL,
    source = "local dataset",
    estimate_time = FALSE,
    estimate_n = 10,
    confirm = interactive(),
    log_file = NULL,
    verbose = TRUE,
    # NEW:
    cwp_grid = NULL,
    cwp_code_col = "cwp_code"
) {
  stopifnot(is.data.frame(dataset))
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  # Packages (offline)
  require(ncdf4)
  require(chron)
  require(plyr)
  
  log_line <- function(fmt, ...) {
    if (!isTRUE(verbose)) return(invisible(NULL))
    
    line <- paste0(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " - ",
      sprintf(fmt, ...)
    )
    
    # Always print to console
    cat(line, "\n")
    
    # Also append to file if provided
    if (!is.null(log_file) && nzchar(log_file)) {
      cat(line, file = log_file, append = TRUE, sep = "\n")
    }
    
    invisible(NULL)
  }
  
  # ---- Minimal metadata defaults ----
  identifier <- dataset_metadata$identifier %||% ""
  variable   <- dataset_metadata$variable   %||% "catch"
  unit       <- dataset_metadata$measurement_unit %||% ""
  fillval    <- dataset_metadata$FillValue %||% -9999
  
  sp_resolution <- dataset_metadata$sp_resolution
  if (!is.null(sp_resolution) && is.na(sp_resolution)) sp_resolution <- NULL
  
  # Nominal heuristic
  if (!is.null(identifier) && grepl("nominal_catch", identifier)) sp_resolution <- NULL
  
  # ---- Basic checks ----
  needed <- c("measurement_value", "time_start")
  miss <- setdiff(needed, names(dataset))
  if (length(miss)) stop("Missing columns in dataset: ", paste(miss, collapse = ", "))
  
  dataset$time_start <- as.Date(dataset$time_start)
  if ("time_end" %in% names(dataset)) dataset$time_end <- as.Date(dataset$time_end)
  
  # ---- Dimensions selection ----
  if (is.character(dimensions) && length(dimensions) == 1) {
    if (tolower(dimensions) == "all") {
      dims <- setdiff(
        names(dataset),
        c("measurement_value","geom_wkt","time_start","time_end","source_authority","measurement_type", cwp_code_col)
      )
    } else if (tolower(dimensions) == "no" || nchar(dimensions) == 0) {
      dims <- character(0)
    } else {
      dims <- dimensions
    }
  } else {
    dims <- dimensions
  }
  
  # ---- Aggregate ----
  aggBy <- c("time_start")
  if (!is.null(sp_resolution)) {
    if (cwp_code_col %in% names(dataset)) {
      aggBy <- c(cwp_code_col, aggBy)
    } else if ("geom_wkt" %in% names(dataset)) {
      aggBy <- c("geom_wkt", aggBy)
    } else {
      stop("sp_resolution is set but neither '", cwp_code_col, "' nor 'geom_wkt' exists in dataset.")
    }
  }
  if (length(dims)) aggBy <- c(aggBy, dims)
  
  t_agg0 <- Sys.time()
  dataset_agg <- stats::aggregate(dataset["measurement_value"], dataset[aggBy], FUN = "sum")
  names(dataset_agg)[names(dataset_agg) == "measurement_value"] <- "measurement_value"
  for (d in dims) dataset_agg[[d]] <- as.character(dataset_agg[[d]])
  dataset_agg$time_start <- as.Date(dataset_agg$time_start)
  t_agg <- as.numeric(difftime(Sys.time(), t_agg0, units = "secs"))
  log_line("Aggregate done: nrow(dataset)=%s -> nrow(dataset_agg)=%s | %.2fs",
           format(nrow(dataset), big.mark=" "), format(nrow(dataset_agg), big.mark=" "), t_agg)
  
  # ---- Determine spatial mode ----
  use_fast_spatial <- !is.null(sp_resolution) &&
    !is.null(cwp_grid) &&
    (cwp_code_col %in% names(dataset_agg))
  
  if (!is.null(sp_resolution) && !use_fast_spatial) {
    # fallback requires geom_wkt
    if (!"geom_wkt" %in% names(dataset_agg)) {
      stop("sp_resolution is set but fast spatial path not available (need cwp_grid + ", cwp_code_col, "), and dataset_agg has no geom_wkt.")
    }
  }
  
  if (use_fast_spatial) {
    log_line("Spatial mode: FAST using cwp_grid | res=%s | code_col=%s", sp_resolution, cwp_code_col)
    idx_pack <- build_cell_index_from_cwp_grid(cwp_grid = cwp_grid, sp_resolution = sp_resolution)
    
    # Join row/col
    cell_index <- idx_pack$cell_index
    names(cell_index)[1] <- cwp_code_col
    
    dataset_agg <- merge(dataset_agg, cell_index, by = cwp_code_col, all.x = TRUE)
    if (anyNA(dataset_agg$row) || anyNA(dataset_agg$col)) {
      stop("Some rows could not be mapped to (row,col). Check that your ", cwp_code_col,
           " values exist in cwp_grid for res=", sp_resolution)
    }
    
    longitudeVector <- idx_pack$longitudeVector
    latitudeVector  <- idx_pack$latitudeVector
    box <- idx_pack$box
    
  } else if (!is.null(sp_resolution)) {
    # SLOW fallback: bbox from geom_wkt coverage
    require(sf)
    if (!is.null(dataset_metadata$spatial_coverage)) {
      test <- data.frame(wkt = unique(dataset_metadata$spatial_coverage), id = 1)
      sptest <- sf::st_as_sf(test, wkt = "wkt", crs = 4326)
    } else {
      test <- data.frame(wkt = unique(dataset_agg$geom_wkt), id = seq_along(unique(dataset_agg$geom_wkt)))
      sptest <- sf::st_as_sf(test, wkt = "wkt", crs = 4326)
    }
    box <- sf::st_bbox(sptest)
    longitudeVector <- seq(from = box["xmin"] + (sp_resolution / 2),
                           to   = box["xmax"] - (sp_resolution / 2),
                           by   = sp_resolution)
    latitudeVector  <- seq(from = box["ymin"] + (sp_resolution / 2),
                           to   = box["ymax"] - (sp_resolution / 2),
                           by   = sp_resolution)
    log_line("Spatial mode: SLOW rasterize fallback | res=%s", sp_resolution)
  } else {
    log_line("Mode: NOMINAL (no spatial grid)")
  }
  
  # ---- Time dimension (julian days since 1950-01-01) ----
  dateVector1 <- sort(unique(dataset_agg$time_start))
  dateVector <- base::julian(
    x = as.numeric(format(as.Date(dateVector1),'%m')),
    d = as.numeric(format(as.Date(dateVector1),'%d')),
    y = as.numeric(format(as.Date(dateVector1),'%Y')),
    origin.=c(month = 1, day = 1, year = 1950)
  )
  
  # ---- Define NetCDF dims ----
  listDims <- list()
  if (!is.null(sp_resolution)) {
    dimX <- ncdf4::ncdim_def("longitude", "degrees_east", longitudeVector)
    dimY <- ncdf4::ncdim_def("latitude",  "degrees_north", latitudeVector)
    listDims <- append(listDims, list(dimX, dimY))
  }
  
  meaningValue <- list(names = list(), ref = list(), values = list())
  
  if (length(dims)) {
    for (dim in dims) {
      dimvals <- unique(dataset_agg[[dim]])
      if (is.numeric(dimvals)) {
        dimVector <- sort(dimvals)
        unit_dim <- ""
      } else {
        dimVector <- seq_along(dimvals)
        unit_dim <- ""
        meaningValue$names[[match(dim, dims)]]  <- dim
        meaningValue$ref[[match(dim, dims)]]    <- dimVector
        meaningValue$values[[match(dim, dims)]] <- dimvals
      }
      dimns <- ncdf4::ncdim_def(dim, unit_dim, dimVector)
      listDims <- append(listDims, list(dimns))
    }
  }
  
  dimT <- ncdf4::ncdim_def("time", "days since 1950-01-01 00:00:00", dateVector, unlim = FALSE)
  listDims <- append(listDims, list(dimT))
  
  # ---- Define variables ----
  var_crs <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(), missval = fillval, prec = "integer")
  
  var_main <- ncdf4::ncvar_def(
    name = as.character(variable),
    units = as.character(unit),
    dim = listDims,
    missval = fillval,
    prec = "float",
    compression = compression
  )
  
  # ---- Output filename ----
  nc_filetempo <- here::here(paste0("tmpfile",specie, identifier, ".nc"))

  
  nc <- ncdf4::nc_create(nc_filetempo, list(var_main, var_crs), force_v4 = TRUE, verbose = FALSE)
  
  # ---- Prepare indices for looping over non-spatial dims + time ----
  idx_df <- if (length(dims)) dataset_agg[dims] else data.frame(dummy = rep(NA, nrow(dataset_agg)))
  
  idx_df$time_start <- base::julian(
    x = as.numeric(format(as.Date(dataset_agg$time_start), "%m")),
    d = as.numeric(format(as.Date(dataset_agg$time_start), "%d")),
    y = as.numeric(format(as.Date(dataset_agg$time_start), "%Y")),
    origin. = c(month = 1, day = 1, year = 1950)
  )
  idx_df$time_start <- plyr::mapvalues(idx_df$time_start, from = dimT$vals, to = seq_along(dimT$vals))
  
  if (length(dims)) {
    for (d in dims) {
      if (d %in% meaningValue$names) {
        loc <- which(unlist(meaningValue$names) %in% d)
        idx_df[[d]] <- plyr::mapvalues(idx_df[[d]],
                                       from = meaningValue$values[[loc]],
                                       to   = meaningValue$ref[[loc]])
      } else if (is.numeric(dataset_agg[[d]])) {
        idx_df[[d]] <- plyr::mapvalues(idx_df[[d]],
                                       from = sort(unique(dataset_agg[[d]])),
                                       to   = seq_along(sort(unique(dataset_agg[[d]]))))
      }
    }
    dataset_agg[dims] <- idx_df[dims]
  }
  dataset_agg$time_start <- idx_df$time_start
  
  gridDimInd2 <- unique(idx_df[c(dims, "time_start")])
  # ---- Diagnostics for performance ----
  n_rows <- nrow(dataset)
  n_agg  <- nrow(dataset_agg)
  n_dims <- length(dims)
  
  n_time <- length(unique(dataset_agg$time_start))
  n_layers <- nrow(gridDimInd2)
  
  if (n_dims > 0) {
    n_combo_dims <- nrow(unique(idx_df[dims, drop = FALSE]))
  } else {
    n_combo_dims <- 1L
  }
  
  if (!is.null(sp_resolution)) {
    nlon <- length(longitudeVector)
    nlat <- length(latitudeVector)
    grid_cells <- nlon * nlat
  } else {
    nlon <- nlat <- grid_cells <- NA_integer_
  }
  
  log_line(
    "Diagnostics | nrow(dataset)=%s | nrow(agg)=%s | dims=%s (%d) | n_time=%d | n_combo_dims=%d | n_layers=%d | grid=%s",
    format(n_rows, big.mark = " "),
    format(n_agg,  big.mark = " "),
    if (n_dims) paste(dims, collapse = ",") else "none",
    n_dims,
    n_time,
    n_combo_dims,
    n_layers,
    if (!is.null(sp_resolution)) paste0(nlon, "x", nlat, "=", format(grid_cells, big.mark=" ")) else "nominal"
  )
  log_line("Numbers of species %s", length(unique(dataset$species)), big.mark=" ")

  
  # Keys + fast selection via split indices
  if (length(dims)) {
    key_cols <- c(dims, "time_start")
    key_all  <- apply(dataset_agg[, key_cols, drop = FALSE], 1, paste, collapse = "_")
    key_grid <- apply(gridDimInd2[, key_cols, drop = FALSE], 1, paste, collapse = "_")
  } else {
    key_all  <- dataset_agg$time_start
    key_grid <- gridDimInd2$time_start
  }
  key_all <- gsub(" ", "", key_all)
  split_index <- split(seq_along(key_all), key_all)
  
  log_line("Layers to write: %s", format(nrow(gridDimInd2), big.mark=" "))
  
  # ---- Estimate time (optional) ----
  if (isTRUE(estimate_time)) {
    
    n_layers <- nrow(gridDimInd2)
    if (n_layers == 0) {
      log_line("Estimate mode: 0 layers.")
    } else {
      
      # choose a more representative sample of layers
      n_test <- min(as.integer(estimate_n), n_layers)
      n_test <- max(n_test, min(50L, n_layers))   # force at least 50 if possible
      
      idx_begin <- seq_len(min(10L, n_layers))
      idx_end   <- seq.int(max(1L, n_layers - 9L), n_layers)
      idx_mid   <- unique(round(seq(1, n_layers, length.out = min(10L, n_layers))))
      idx_rand  <- sample.int(n_layers, size = min(n_test, n_layers), replace = FALSE)
      
      idx_sample <- unique(c(idx_begin, idx_mid, idx_end, idx_rand))
      idx_sample <- idx_sample[seq_len(min(length(idx_sample), n_test))]
      
      log_line("Estimate mode: sampling %d layers out of %d.", length(idx_sample), n_layers)
      
      # temp file to include real ncvar_put cost (+ compression)
      tmp_file <- tempfile(fileext = ".nc")
      nc_est <- ncdf4::nc_create(tmp_file, list(var_main, var_crs), force_v4 = TRUE, verbose = FALSE)
      
      # measure per-layer times
      layer_secs <- numeric(length(idx_sample))
      
      for (j in seq_along(idx_sample)) {
        i <- idx_sample[j]
        
        t0 <- proc.time()[["elapsed"]]
        
        k <- key_grid[i]
        sel <- split_index[[k]]
        if (length(sel)) {
          if (!is.null(sp_resolution)) {
            if (use_fast_spatial) {
              m <- matrix(fillval, nrow = length(latitudeVector), ncol = length(longitudeVector))
              m[cbind(dataset_agg$row[sel], dataset_agg$col[sel])] <- dataset_agg$measurement_value[sel]
              
              strt <- as.integer(unlist(gridDimInd2[i, , drop = TRUE]))
              ncdf4::ncvar_put(nc_est, var_main, vals = m,
                               start = c(1, 1, strt),
                               count = c(-1, -1, rep(1, length(strt))))
            } else {
              # (optional) slow path estimate if you keep it
            }
          } else {
            vals <- dataset_agg$measurement_value[sel]
            strt <- as.integer(unlist(gridDimInd2[i, , drop = TRUE]))
            ncdf4::ncvar_put(nc_est, var_main, vals = vals,
                             start = c(strt),
                             count = c(rep(1, length(strt))))
          }
        }
        
        layer_secs[j] <- proc.time()[["elapsed"]] - t0
      }
      
      ncdf4::nc_close(nc_est)
      unlink(tmp_file)
      
      # robust estimates
      med <- stats::median(layer_secs, na.rm = TRUE)
      p90 <- stats::quantile(layer_secs, probs = 0.90, na.rm = TRUE, names = FALSE)
      
      est_total_med <- med * n_layers
      est_total_p90 <- p90 * n_layers
      
      log_line(
        "Estimated runtime (robust): median ~%.1f min | pessimistic (p90) ~%.1f min | sample=%d layers. Do you agree? (yes/no)",
        est_total_med/60, est_total_p90/60, length(idx_sample)
      )
      
      if (isTRUE(confirm)) {
        ans <- tolower(trimws(readline("Proceed? (yes/no): ")))
        if (!ans %in% c("y", "yes")) stop("Aborted by user after time estimate.")
        log_line("User agreed. Proceeding with full write.")
      }
    }
  }
  
  # ---- Write data ----
  t_write0 <- Sys.time()
  
  if (!is.null(sp_resolution)) {
    if (use_fast_spatial) {
      # FAST: direct matrix fill + ncvar_put
      for (i in seq_len(nrow(gridDimInd2))) {
        k <- key_grid[i]
        sel <- split_index[[k]]
        if (!length(sel)) next
        
        m <- matrix(fillval, nrow = length(latitudeVector), ncol = length(longitudeVector))
        m[cbind(dataset_agg$row[sel], dataset_agg$col[sel])] <- dataset_agg$measurement_value[sel]
        
        strt <- as.integer(unlist(gridDimInd2[i, , drop = TRUE]))
        ncdf4::ncvar_put(nc, var_main, vals = m,
                         start = c(1, 1, strt),
                         count = c(-1, -1, rep(1, length(strt))))
      }
    } else {
      # SLOW fallback: rasterize
      require(sf); require(terra)
      dataset_sf <- sf::st_as_sf(dataset_agg, wkt = "geom_wkt", crs = 4326)
      rast_template <- terra::rast(
        nrows  = as.integer((box["ymax"] - box["ymin"]) / sp_resolution),
        ncols  = as.integer((box["xmax"] - box["xmin"]) / sp_resolution),
        extent = terra::ext(box["xmin"], box["xmax"], box["ymin"], box["ymax"]),
        crs    = "EPSG:4326"
      )
      for (i in seq_len(nrow(gridDimInd2))) {
        k <- key_grid[i]
        sel <- split_index[[k]]
        if (!length(sel)) next
        resTD_sf <- dataset_sf[sel, ]
        rast_res <- terra::rasterize(resTD_sf, rast_template,
                                     field = "measurement_value", fun = sum,
                                     background = fillval)
        m <- terra::as.matrix(rast_res, wide = TRUE)
        m <- t(m[nrow(m):1, , drop = FALSE])
        
        strt <- as.integer(unlist(gridDimInd2[i, , drop = TRUE]))
        ncdf4::ncvar_put(nc, var_main, vals = m,
                         start = c(1, 1, strt),
                         count = c(-1, -1, rep(1, length(strt))))
      }
    }
  } else {
    # Nominal
    for (i in seq_len(nrow(gridDimInd2))) {
      k <- key_grid[i]
      sel <- split_index[[k]]
      if (!length(sel)) next
      vals <- dataset_agg$measurement_value[sel]
      strt <- as.integer(unlist(gridDimInd2[i, , drop = TRUE]))
      ncdf4::ncvar_put(nc, var_main, vals = vals,
                       start = c(strt),
                       count = c(rep(1, length(strt))))
    }
  }
  
  t_write <- as.numeric(difftime(Sys.time(), t_write0, units = "secs"))
  log_line("Write completed in %.1fs | output=%s", t_write, nc_filetempo)
  
  # ---- Meanings attributes for character dims ----
  if (length(dims)) {
    for (d in dims) {
      loc <- match(d, unlist(meaningValue$names))
      if (!is.na(loc)) {
        ncdf4::ncatt_put(nc, d, paste0(d, "_values"),   paste(meaningValue$ref[[loc]], collapse = ","))
        ncdf4::ncatt_put(nc, d, paste0(d, "_meanings"), paste(gsub(" ", "_", as.character(meaningValue$values[[loc]])), collapse = " "))
        ncdf4::ncatt_put(nc, d, paste0(d, "_valid_range"),
                         paste(range(meaningValue$ref[[loc]]), collapse = ","))
      }
    }
  }
  
  # ---- CF-ish attributes ----
  if (!is.null(sp_resolution)) {
    ncdf4::ncatt_put(nc, "longitude", "standard_name", "longitude")
    ncdf4::ncatt_put(nc, "longitude", "axis", "X")
    ncdf4::ncatt_put(nc, "latitude",  "standard_name", "latitude")
    ncdf4::ncatt_put(nc, "latitude",  "axis", "Y")
  }
  ncdf4::ncatt_put(nc, "time", "standard_name", "time")
  ncdf4::ncatt_put(nc, "time", "axis", "T")
  ncdf4::ncatt_put(nc, "time", "calendar", "standard")
  
  # CRS grid_mapping variable
  ncdf4::ncatt_put(nc, "crs", "grid_mapping_name", "latitude_longitude")
  ncdf4::ncatt_put(nc, "crs", "longitude_of_prime_meridian", 0.0)
  ncdf4::ncatt_put(nc, "crs", "semi_major_axis", 6378137.0)
  ncdf4::ncatt_put(nc, "crs", "inverse_flattening", 298.257223563)
  ncdf4::ncatt_put(nc, as.character(variable), "grid_mapping", "crs")
  
  # ---- Global attributes ----
  ncdf4::ncatt_put(nc, 0, "title",   title   %||% paste0(variable, " dataset ", identifier))
  ncdf4::ncatt_put(nc, 0, "summary", summary %||% "")
  ncdf4::ncatt_put(nc, 0, "source",  source)
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.6")
  ncdf4::ncatt_put(nc, 0, "date_created",  as.character(Sys.time()))
  ncdf4::ncatt_put(nc, 0, "date_modified", as.character(Sys.time()))
  ncdf4::ncatt_put(nc, 0, "time_coverage_start", as.character(min(as.Date(dateVector1))))
  ncdf4::ncatt_put(nc, 0, "time_coverage_end",   as.character(max(as.Date(dateVector1))))
  
  if (!is.null(sp_resolution)) {
    ncdf4::ncatt_put(nc, 0, "geospatial_lat_min", box["ymin"])
    ncdf4::ncatt_put(nc, 0, "geospatial_lat_max", box["ymax"])
    ncdf4::ncatt_put(nc, 0, "geospatial_lon_min", box["xmin"])
    ncdf4::ncatt_put(nc, 0, "geospatial_lon_max", box["xmax"])
    ncdf4::ncatt_put(nc, 0, "geospatial_lat_units", "degrees_north")
    ncdf4::ncatt_put(nc, 0, "geospatial_lon_units", "degrees_east")
    ncdf4::ncatt_put(nc, 0, "geospatial_lat_resolution", sp_resolution)
    ncdf4::ncatt_put(nc, 0, "geospatial_lon_resolution", sp_resolution)
  }
  
  ncdf4::nc_close(nc)
  nc_file_final <- file.path(path, paste0(specie, identifier, ".nc"))
  if (file.exists(nc_file_final)) file.remove(nc_file_final)
  file.copy(from = nc_filetempo, nc_file_final)
  file.remove(nc_filetempo)
  return(nc_file_final)
}

# ------------------------------------------------------------
# Wrapper by species: logs to one file + forwards ... correctly
# ------------------------------------------------------------
export_netcdf_by_species <- function(
    dataset,
    dataset_metadata,
    path = "data",
    sp_resolution = 5,
    compression = 9,
    verbose = TRUE, 
    dimensions = "all",
    log_file = "export_netcdf_by_species.log",
    ...) {
  log_file = file.path(path, log_file)
  stopifnot(
    is.data.frame(dataset),
    all(c("species", "gear_type", "time_start", "measurement_value") %in% names(dataset))
  )
  possible_dims <- c(
    "source_authority",
    "fishing_fleet",
    "gear_type",
    "fishing_mode",
    "measurement_type",
    "measurement_unit"
  )
  if (is.null(dimensions) || identical(dimensions, "all")) {
    dimensions_to_include <- intersect(possible_dims, names(dataset))
  } else {
    dimensions_to_include <- intersect(dimensions, names(dataset))
  }
  
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  log_line <- function(fmt, ...) {
    if (!isTRUE(verbose)) return(invisible(NULL))
    
    line <- paste0(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " - ",
      sprintf(fmt, ...)
    )
    
    cat(line, file = log_file, append = TRUE, sep = "\n")
  }
  
  species_list <- sort(unique(dataset$species))
  results <- vector("list", length(species_list))
  names(results) <- species_list
  
  log_line("Starting NetCDF export | %s species", length(species_list))
  
  for (sp in species_list) {
    
    t0 <- Sys.time()
    
    sp_df <- dataset %>%
      dplyr::filter(species == sp)
    
    log_line("[START] species=%s | n=%s",
             sp, format(nrow(sp_df), big.mark = " "))
    
    # on enlève species (sinon ça devient une dimension implicite)
    sp_df_in <- sp_df %>%
      dplyr::select(-species)
    
    # metadata spécifique espèce
    md_sp <- dataset_metadata
    md_sp$identifier <- paste0("_", sp, "_", dataset_metadata$identifier)
    md_sp$sp_resolution <- sp_resolution
    
    log_line("nrow(dataset)      =", nrow(dataset), "\n")
    log_line("nrow(dataset_agg)  =", nrow(dataset_agg), "\n")
    log_line("n_layers (gridDimInd2) =", nrow(gridDimInd2), "\n")
    
    
    out <- tryCatch(
      convert_to_netcdf_simple(
        dataset = sp_df_in,
        dataset_metadata = md_sp,
        dimensions = dimensions_to_include,
        path = path,
        compression = compression,
        title = paste(md_sp$identifier, paste(dimensions_to_include, collapse = "_")),
        summary = "NetCDF export by species with gear_type dimension",
        source = "offline processing",
        ...
      ),
      error = function(e) e
    )
    
    dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    
    if (inherits(out, "error")) {
      log_line("[ERROR] species=%s | %.1fs | %s", sp, dt, out$message)
      results[[sp]] <- NA_character_
    } else {
      log_line("[DONE ] species=%s | %.1fs | file=%s", sp, dt, out)
      results[[sp]] <- out
    }
  }
  
  log_line("Finished NetCDF export")
  
  invisible(results)
}


make_dataset_metadata <- function(
    dataset,
    identifier = "my_dataset",
    fillvalue = -9999,
    cwp_code_col = c("cwp_code", "CWP_CODE", "geographic_identifier")
) {
  stopifnot(is.data.frame(dataset))
  
  # ---- measurement_unit ----
  mu <- if ("measurement_unit" %in% names(dataset)) unique(dataset$measurement_unit) else NA_character_
  mu <- mu[!is.na(mu)]
  measurement_unit <- if (length(mu) == 1) as.character(mu) else if (length(mu) == 0) "" else "t"
  
  # ---- variable inference from fixed column: measurement ----
  variable <- "catch"
  if ("measurement" %in% names(dataset)) {
    vals <- unique(as.character(dataset$measurement))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 1) {
      variable <- gsub("[^A-Za-z0-9_]", "_", vals)
    } else if (length(vals) > 1) {
      stop(
        "Multiple 'measurement' values found: ",
        paste(head(vals, 20), collapse = ", "),
        if (length(vals) > 20) " ..." else "",
        ". Please filter the dataset to a single measurement before exporting."
      )
    }
  }
  
  # ---- spatial resolution inference from cwp_code prefix (fixed mapping) ----
  # Fixed mapping (won't change):
  #   prefix 5 -> 1 degree
  #   prefix 6 -> 5 degree
  res_by_prefix <- c("5" = 1, "6" = 5)
  
  cwp_code_col <- match.arg(cwp_code_col)
  if (!cwp_code_col %in% names(dataset)) {
    stop("No cwp_code column found. Expected one of: ", paste(c("cwp_code", "CWP_CODE"), collapse = ", "))
  }
  
  codes <- dataset[[cwp_code_col]]
  codes <- codes[!is.na(codes)]
  if (!length(codes)) stop("No non-NA values found in column '", cwp_code_col, "'.")
  
  codes_chr <- as.character(codes)
  pref <- substr(codes_chr, 1, 1)
  
  res_vals <- unname(res_by_prefix[pref])
  res_vals <- res_vals[!is.na(res_vals)]
  
  if (!length(res_vals)) {
    stop(
      "Could not infer sp_resolution from '", cwp_code_col, "'. ",
      "Prefixes seen: ", paste(sort(unique(pref)), collapse = ", "),
      ". Expected prefixes among: ", paste(names(res_by_prefix), collapse = ", "),
      "."
    )
  }
  
  ures <- sort(unique(res_vals))
  if (length(ures) > 1) {
    stop(
      "Multiple spatial resolutions detected from '", cwp_code_col, "': ",
      paste(ures, collapse = ", "),
      ". Please split/filter the dataset by resolution before exporting."
    )
  }
  
  sp_resolution <- ures[1]
  sp_resolution_unit <- "degree"
  
  list(
    identifier = identifier,
    variable = variable,
    measurement_unit = measurement_unit,
    sp_resolution = sp_resolution,
    sp_resolution_unit = sp_resolution_unit,
    FillValue = fillvalue
  )
}

parse_parameter_filtering <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(list())
  
  parts <- strsplit(x, ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  
  res <- list()
  
  for (p in parts) {
    op <- NULL
    if (grepl("!=", p, fixed = TRUE)) {
      op <- "!="
      kv <- strsplit(p, "!=", fixed = TRUE)[[1]]
    } else if (grepl("=", p, fixed = TRUE)) {
      op <- "="
      kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    } else {
      next
    }
    
    if (length(kv) != 2) next
    
    key <- trimws(kv[1])
    val <- trimws(kv[2])
    
    if (key == "" || val == "" || toupper(val) == "NULL") next
    
    values <- trimws(strsplit(val, ",", fixed = TRUE)[[1]])
    values <- values[values != "" & toupper(values) != "NULL"]
    
    if (length(values)) {
      res[[key]] <- list(op = op, values = values)
    }
  }
  
  res
}

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

# stop("Stop")

# dataset <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20250620061443level_2_catch_2025/entities/global_catch_ird_level2_1950_2023/Markdown/RF_pass_8/data.qs")
# 
# dataset <- dataset %>% dplyr::filter(species %in% c("YFT", "SKJ", "ALB", "BET", "SWO", "SBF")) %>%
#   dplyr::distinct()
# 
# dataset_onedeg <- dataset %>%
#   dplyr::filter(startsWith(as.character(geographic_identifier), "5")) %>% 
#   dplyr::filter(measurement_unit == "Tons") %>% dplyr::mutate(measurement_unit = "t")%>%
#   dplyr::rename(cwp_code = geographic_identifier)
# 
# dataset_5deg <- dataset %>%
#   dplyr::filter(startsWith(as.character(geographic_identifier), "6")) %>%
#   dplyr::filter(measurement_unit == "Tons")%>% dplyr::mutate(measurement_unit = "t") %>% 
#   dplyr::rename(cwp_code = geographic_identifier)
# 
# dataset_metadata_5deg <- make_dataset_metadata(
#   dataset_5deg,
#   identifier = "gta_catch_1952_2023_5deg",
#   variable = "catch",
#   default_sp_resolution = 5
# )
# 
# dataset_metadata_1deg <- make_dataset_metadata(
#   dataset_onedeg,
#   identifier = "gta_catch_1952_2023_1deg",
#   variable = "catch",
#   default_sp_resolution = 1
# )
# 
# export_netcdf_by_species_gear(
#   dataset = dataset_5deg,                 
#   dataset_metadata = dataset_metadata_5deg, 
#   path = "data",
#   sp_resolution = 5,
#   dimensions = c("gear_type", "fishing_fleet"),
#   log_file = "export.log",
#   estimate_time = TRUE,
#   estimate_n = 1000,
#   confirm = TRUE,
#   cwp_grid = cwp_grid,                     
#   cwp_code_col = "cwp_code"                
# )
# 
# 
# export_netcdf_by_species_gear(
#   dataset = dataset_5deg,                 
#   dataset_metadata = dataset_metadata_5deg, 
#   path = "data",
#   sp_resolution = 5,
#   dimensions = c("gear_type"),
#   log_file = "export.log",
#   estimate_time = TRUE,
#   estimate_n = 100,
#   confirm = TRUE,
#   cwp_grid = cwp_grid,                     
#   cwp_code_col = "cwp_code"                
# )
# 
# export_netcdf_by_species_gear(
#   dataset = dataset_5deg,                 
#   dataset_metadata = dataset_metadata_5deg, 
#   path = "data",
#   sp_resolution = 5,
#   dimensions = c("gear_type", "fishing_fleet"),
#   log_file = "export.log",
#   estimate_time = TRUE,
#   estimate_n = 100,
#   confirm = TRUE,
#   cwp_grid = cwp_grid,                     
#   cwp_code_col = "cwp_code"                
# )
# 
# # IOTC 2000-2020 only for testing ----------------------------------------------
# 
# swo_5_deg <- dataset_5deg %>% dplyr::filter(source_authority == "IOTC" & time_start > 2000 )
# 
# dataset_swo_metadata_5deg <- make_dataset_metadata(
#   swo_5_deg,
#   identifier = "gta_catch_1952_2023_5deg",
#   variable = "catch",
#   default_sp_resolution = 5
# )
# 
# export_netcdf_by_species_gear(
#   dataset = swo_5_deg,                 
#   dataset_metadata = dataset_swo_metadata_5deg, 
#   path = "data",
#   sp_resolution = 5,
#   dimensions = c("gear_type"),
#   log_file = "export.log",
#   estimate_time = TRUE,
#   estimate_n = 100,
#   confirm = TRUE,
#   cwp_grid = cwp_grid,                     
#   cwp_code_col = "cwp_code"                
# )
# 
# export_netcdf_by_species_gear(
#   dataset = swo_5_deg,                 
#   dataset_metadata = dataset_swo_metadata_5deg, 
#   path = "data",
#   sp_resolution = 5,
#   dimensions = c("gear_type", "fishing_fleet"),
#   log_file = "export.log",
#   estimate_time = TRUE,
#   estimate_n = 100,
#   confirm = TRUE,
#   cwp_grid = cwp_grid,                     
#   cwp_code_col = "cwp_code"                
# )
# 
# # One deg -----------------------------------------------------------------
# 
# 
# 
# export_netcdf_by_species_gear(
#   dataset = dataset_onedeg,                 
#   dataset_metadata = dataset_metadata_1deg, 
#   path = "data",
#   sp_resolution = 1,
#   dimensions = "no",
#   log_file = "export.log",
#   estimate_time = TRUE,
#   estimate_n = 10,
#   confirm = TRUE,
#   cwp_grid = cwp_grid,                     
#   cwp_code_col = "cwp_code"                
# )
# 
# 
# 
