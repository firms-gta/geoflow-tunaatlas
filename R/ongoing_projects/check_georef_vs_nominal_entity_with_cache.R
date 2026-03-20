library(dplyr)

check_georef_vs_nominal_entity <- function(
    entity_dir,
    selected_species = c("SBF","YFT","SKJ","BET","ALB","SWO"),
    steps_to_run = NULL,
    use_cache = TRUE,
    force_recompute = FALSE,
    cache_dir_name = "check_georef_vs_nominal_cache"
) {
  
  log_message <- function(...) {
    cat(sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        paste(..., collapse = " "), "\n")
    flush.console()
  }
  
  read_or_compute_qs <- function(path, expr, use_cache = TRUE, force_recompute = FALSE, label = NULL) {
    if (use_cache && !force_recompute && file.exists(path)) {
      if (!is.null(label)) log_message("Loading cache:", label, "->", path)
      return(qs::qread(path))
    }
    if (!is.null(label)) log_message("Computing:", label)
    obj <- expr()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    qs::qsave(obj, path)
    if (!is.null(label)) log_message("Saved cache:", label, "->", path)
    obj
  }
  
  compare_level <- function(
    georef, nominal, group_cols, step_name, level_name,
    step_rank, file_time, keep_all_comp = FALSE, n_examples = 20,
    cache_path = NULL, use_cache = TRUE, force_recompute = FALSE
  ) {
    
    `%notin%` <- Negate(`%in%`)
    
    compute_compare <- function() {
      log_message("  -> Level:", level_name)
      
      georef_local <- georef
      nominal_local <- nominal
      
      if ("group_species_iattc_sharks" %notin% colnames(georef_local)) {
        georef_local$group_species_iattc_sharks <- recode_group_species(
          georef_local$species,
          georef_local$source_authority
        )
      }
      
      g <- georef_local %>%
        dplyr::filter(measurement_unit %in% c("t", "Tons")) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::summarise(
          georef = sum(measurement_value, na.rm = TRUE),
          .groups = "drop"
        )
      
      log_message("     georef grouped rows:", nrow(g))
      
      if ("group_species_iattc_sharks" %notin% colnames(nominal_local)) {
        nominal_local$group_species_iattc_sharks <- recode_group_species(
          nominal_local$species,
          nominal_local$source_authority
        )
      }
      
      n <- nominal_local %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::summarise(
          nominal = sum(measurement_value, na.rm = TRUE),
          .groups = "drop"
        )
      
      log_message("     nominal grouped rows:", nrow(n))
      
      comp <- dplyr::full_join(g, n, by = group_cols) %>%
        dplyr::mutate(
          georef = dplyr::coalesce(georef, 0),
          nominal = dplyr::coalesce(nominal, 0),
          georef_superior = (georef - nominal) > 1 & !is.na(nominal) & nominal != 0,
          diff = georef - nominal,
          diff_pct_nominal = dplyr::if_else(
            nominal > 0,
            100 * (georef - nominal) / nominal,
            NA_real_
          )
        )
      
      out <- tibble::tibble(
        step_rank = step_rank,
        step_name = step_name,
        file_time = file_time,
        level_name = level_name,
        strata = nrow(comp),
        georef_superior = sum(comp$georef_superior, na.rm = TRUE),
        pct_georef_superior = round(100 * mean(comp$georef_superior, na.rm = TRUE), 4)
      )
      
      strata_sup <- comp %>%
        dplyr::filter(georef_superior) %>%
        dplyr::arrange(dplyr::desc(diff)) %>%
        dplyr::mutate(
          step_rank = step_rank,
          step_name = step_name,
          file_time = file_time,
          level_name = level_name
        )
      
      strata_examples <- strata_sup %>%
        dplyr::slice_head(n = n_examples)
      
      log_message(
        "     done | strata:", out$strata,
        "| georef_superior:", out$georef_superior,
        "| pct:", out$pct_georef_superior
      )
      
      res <- list(
        summary = out,
        strata_sup = strata_sup,
        strata_examples = strata_examples
      )
      
      if (keep_all_comp) {
        res$all_comp <- comp
      }
      
      rm(g, n, comp, georef_local, nominal_local)
      invisible(gc())
      
      res
    }
    
    if (!is.null(cache_path)) {
      return(read_or_compute_qs(
        path = cache_path,
        expr = compute_compare,
        use_cache = use_cache,
        force_recompute = force_recompute,
        label = paste(step_name, level_name)
      ))
    }
    
    compute_compare()
  }
  
  log_message("Starting function")
  log_message("Entity directory:", entity_dir)
  
  if (!dir.exists(entity_dir)) {
    stop("Directory does not exist: ", entity_dir)
  }
  
  markdown_dir <- file.path(entity_dir, "Markdown")
  data_dir <- file.path(entity_dir, "data")
  global_cache_dir <- file.path(data_dir, cache_dir_name)
  
  if (!dir.exists(markdown_dir)) {
    stop("Markdown directory not found: ", markdown_dir)
  }
  
  if (!dir.exists(data_dir)) {
    stop("Data directory not found: ", data_dir)
  }
  
  log_message("Markdown directory:", markdown_dir)
  log_message("Data directory:", data_dir)
  
  nominal_candidates <- c(
    file.path(data_dir, "nominal_catch_for_raising.qs")
  )
  
  nominal_candidates <- nominal_candidates[file.exists(nominal_candidates)]
  
  if (length(nominal_candidates) == 0) {
    nominal_candidates <- list.files(
      path = data_dir,
      pattern = "nominal.*raising*\\.(rds|qs)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  }
  
  if (length(nominal_candidates) == 0) {
    stop("No nominal deteriorated dataset found in: ", data_dir)
  }
  
  nominal_path <- nominal_candidates[1]
  log_message("Nominal dataset found:", nominal_path)
  
  nominal <- read_or_compute_qs(
    path = file.path(global_cache_dir, "nominal_prepared.qs"),
    use_cache = use_cache,
    force_recompute = force_recompute,
    label = "nominal_prepared",
    expr = function() {
      log_message("Loading nominal dataset...")
      nom <- if (grepl("\\.qs$", nominal_path, ignore.case = TRUE)) {
        qs::qread(nominal_path)
      } else {
        readRDS(nominal_path)
      }
      
      nom %>%
        dplyr::mutate(year = lubridate::year(time_start)) %>%
        dplyr::filter(!(source_authority == "IOTC" & year %in% c(2022, 2023, 2024)))
    }
  )
  
  nominal_filtered <- read_or_compute_qs(
    path = file.path(global_cache_dir, "nominal_filtered.qs"),
    use_cache = use_cache,
    force_recompute = force_recompute,
    label = "nominal_filtered",
    expr = function() {
      nominal %>%
        dplyr::filter(species %in% selected_species)
    }
  )
  
  log_message("Nominal loaded, rows:", nrow(nominal))
  
  step_dirs <- list.dirs(markdown_dir, recursive = FALSE, full.names = TRUE)
  
  data_paths <- file.path(step_dirs, "data.qs")
  ancient_paths <- file.path(step_dirs, "ancient.qs")
  
  step_data_paths <- ifelse(file.exists(data_paths), data_paths,
                            ifelse(file.exists(ancient_paths), ancient_paths, NA))
  
  keep <- !is.na(step_data_paths)
  
  step_dirs <- step_dirs[keep]
  step_data_paths <- step_data_paths[keep]
  
  if (length(step_dirs) == 0) {
    stop("No step directories with data.qs or ancient.qs found in: ", markdown_dir)
  }
  
  step_info <- tibble::tibble(
    step_dir = step_dirs,
    step = basename(step_dirs),
    data_path = step_data_paths,
    mtime = file.info(step_data_paths)$mtime
  ) %>%
    dplyr::arrange(mtime) %>%
    dplyr::mutate(step_rank_original = dplyr::row_number())
  
  log_message("Total steps found:", nrow(step_info))
  
  for (k in seq_len(nrow(step_info))) {
    log_message(
      "  ", k, "/", nrow(step_info),
      " | rank=", step_info$step_rank_original[k],
      " | ", step_info$step[k],
      " | ", as.character(step_info$mtime[k])
    )
  }
  
  if (!is.null(steps_to_run)) {
    log_message("steps_to_run provided:", paste(steps_to_run, collapse = ", "))
    
    if (!is.numeric(steps_to_run)) {
      stop("steps_to_run must be numeric, e.g. c(1, 5, 10)")
    }
    
    steps_to_run <- as.integer(steps_to_run)
    steps_to_run <- unique(steps_to_run)
    steps_to_run <- steps_to_run[!is.na(steps_to_run)]
    steps_to_run <- steps_to_run[steps_to_run >= 1 & steps_to_run <= nrow(step_info)]
    
    if (length(steps_to_run) == 0) {
      stop("No valid step index in steps_to_run.")
    }
    
    step_info <- step_info %>%
      dplyr::slice(steps_to_run)
  }
  
  res_source_authority_species <- vector("list", nrow(step_info))
  res_source_authority_species_year <- vector("list", nrow(step_info))
  res_source_authority_species_year_strata <- vector("list", nrow(step_info))
  res_source_authority_species_year_fishing_fleet <- vector("list", nrow(step_info))
  res_source_authority_species_filtered <- vector("list", nrow(step_info))
  res_source_authority_species_year_filtered <- vector("list", nrow(step_info))
  res_source_authority_species_year_fishing_fleet_filtered <- vector("list", nrow(step_info))
  
  for (i in seq_len(nrow(step_info))) {
    step_dir <- step_info$step_dir[i]
    step_name <- step_info$step[i]
    data_path <- step_info$data_path[i]
    file_time <- step_info$mtime[i]
    step_cache_dir <- file.path(step_dir, cache_dir_name)
    
    log_message("--------------------------------------------------")
    log_message("Step", i, "/", nrow(step_info), ":", step_name)
    log_message("data.qs mtime:", as.character(file_time))
    
    georef <- read_or_compute_qs(
      path = file.path(step_cache_dir, "georef_prepared.qs"),
      use_cache = use_cache,
      force_recompute = force_recompute,
      label = paste(step_name, "georef_prepared"),
      expr = function() {
        log_message("Loading georef dataset:", data_path)
        qs::qread(data_path) %>%
          dplyr::mutate(year = lubridate::year(time_start)) %>%
          dplyr::filter(!(source_authority == "IOTC" & year %in% c(2022, 2023, 2024)))
      }
    )
    
    georef_filtered <- read_or_compute_qs(
      path = file.path(step_cache_dir, "georef_filtered.qs"),
      use_cache = use_cache,
      force_recompute = force_recompute,
      label = paste(step_name, "georef_filtered"),
      expr = function() {
        georef %>%
          dplyr::filter(species %in% selected_species)
      }
    )
    
    tmp_species <- compare_level(
      georef = georef,
      nominal = nominal,
      group_cols = c("source_authority", "group_species_iattc_sharks"),
      step_name = step_name,
      level_name = "source_authority_species",
      step_rank = step_info$step_rank_original[i],
      file_time = file_time,
      cache_path = file.path(step_cache_dir, "source_authority_species_all.qs"),
      use_cache = use_cache,
      force_recompute = force_recompute
    )
    res_source_authority_species[[i]] <- tmp_species$summary
    
    tmp_species_filtered <- compare_level(
      georef = georef_filtered,
      nominal = nominal_filtered,
      group_cols = c("source_authority", "group_species_iattc_sharks"),
      step_name = step_name,
      level_name = "source_authority_species",
      step_rank = step_info$step_rank_original[i],
      file_time = file_time,
      cache_path = file.path(step_cache_dir, "source_authority_species_selected.qs"),
      use_cache = use_cache,
      force_recompute = force_recompute
    )
    res_source_authority_species_filtered[[i]] <- tmp_species_filtered$summary
    
    tmp_year <- compare_level(
      georef = georef,
      nominal = nominal,
      group_cols = c("source_authority", "group_species_iattc_sharks", "year"),
      step_name = step_name,
      level_name = "source_authority_species_year",
      step_rank = step_info$step_rank_original[i],
      file_time = file_time,
      cache_path = file.path(step_cache_dir, "source_authority_species_year_all.qs"),
      use_cache = use_cache,
      force_recompute = force_recompute
    )
    res_source_authority_species_year[[i]] <- tmp_year$summary
    res_source_authority_species_year_strata[[i]] <- tmp_year$strata_sup
    
    tmp_year_filtered <- compare_level(
      georef = georef_filtered,
      nominal = nominal_filtered,
      group_cols = c("source_authority", "group_species_iattc_sharks", "year"),
      step_name = step_name,
      level_name = "source_authority_species_year",
      step_rank = step_info$step_rank_original[i],
      file_time = file_time,
      cache_path = file.path(step_cache_dir, "source_authority_species_year_selected.qs"),
      use_cache = use_cache,
      force_recompute = force_recompute
    )
    res_source_authority_species_year_filtered[[i]] <- tmp_year_filtered$summary
    
    tmp_year_fleet <- compare_level(
      georef = georef,
      nominal = nominal,
      group_cols = c("source_authority", "group_species_iattc_sharks", "year", "fishing_fleet"),
      step_name = step_name,
      level_name = "source_authority_species_year_fishing_fleet",
      step_rank = step_info$step_rank_original[i],
      file_time = file_time,
      cache_path = file.path(step_cache_dir, "source_authority_species_year_fishing_fleet_all.qs"),
      use_cache = use_cache,
      force_recompute = force_recompute
    )
    res_source_authority_species_year_fishing_fleet[[i]] <- tmp_year_fleet$summary
    
    tmp_year_fleet_filtered <- compare_level(
      georef = georef_filtered,
      nominal = nominal_filtered,
      group_cols = c("source_authority", "group_species_iattc_sharks", "year", "fishing_fleet"),
      step_name = step_name,
      level_name = "source_authority_species_year_fishing_fleet",
      step_rank = step_info$step_rank_original[i],
      file_time = file_time,
      cache_path = file.path(step_cache_dir, "source_authority_species_year_fishing_fleet_selected.qs"),
      use_cache = use_cache,
      force_recompute = force_recompute
    )
    res_source_authority_species_year_fishing_fleet_filtered[[i]] <- tmp_year_fleet_filtered$summary
    
    rm(georef, georef_filtered)
    invisible(gc())
    
    log_message("Finished step:", step_name)
  }
  
  log_message("Binding result tables...")
  
  source_authority_species <- dplyr::bind_rows(res_source_authority_species)
  source_authority_species_year <- dplyr::bind_rows(res_source_authority_species_year)
  source_authority_species_year_strata <- dplyr::bind_rows(res_source_authority_species_year_strata)
  source_authority_species_year_fishing_fleet <- dplyr::bind_rows(res_source_authority_species_year_fishing_fleet)
  source_authority_species_filtered <- dplyr::bind_rows(res_source_authority_species_filtered)
  source_authority_species_year_filtered <- dplyr::bind_rows(res_source_authority_species_year_filtered)
  source_authority_species_year_fishing_fleet_filtered <- dplyr::bind_rows(res_source_authority_species_year_fishing_fleet_filtered)
  
  invisible(gc())
  log_message("All done")
  
  return(list(
    all_species = list(
      source_authority_species = source_authority_species,
      source_authority_species_year = source_authority_species_year,
      source_authority_species_year_fishing_fleet = source_authority_species_year_fishing_fleet,
      source_authority_species_year_strata = source_authority_species_year_strata
    ),
    selected_species = list(
      source_authority_species = source_authority_species_filtered,
      source_authority_species_year = source_authority_species_year_filtered,
      source_authority_species_year_fishing_fleet = source_authority_species_year_fishing_fleet_filtered
    )
  ))
}
