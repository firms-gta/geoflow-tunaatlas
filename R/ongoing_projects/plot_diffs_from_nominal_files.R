plot_diffs_from_nominal_files <- function(
    main_dir,
    comparison_files,
    value_col = "sum_t",
    filter_list = NULL
) {
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(purrr)
  library(tibble)
  library(tools)
  
  main_dir <- path.expand(main_dir)
  comparison_files <- setNames(path.expand(unname(comparison_files)), names(comparison_files))
  
  if (!dir.exists(main_dir)) {
    stop("main_dir introuvable : ", main_dir)
  }
  
  read_for_diff <- function(file, value_col = "sum_t") {
    dat <- readr::read_csv(file, show_col_types = FALSE)
    
    if (!value_col %in% names(dat)) {
      stop(sprintf("La colonne '%s' est absente dans : %s", value_col, file))
    }
    
    dat[[value_col]] <- suppressWarnings(as.numeric(dat[[value_col]]))
    
    id_cols <- setdiff(names(dat), c(value_col, "sum_no", "lines"))
    
    if (length(id_cols) == 0) {
      return(tibble::tibble(
        id = "all",
        value = sum(dat[[value_col]], na.rm = TRUE)
      ))
    }
    
    dat %>%
      dplyr::select(
        id = dplyr::all_of(id_cols[1]),
        value = dplyr::all_of(value_col)
      )
  }
  
  read_for_lines <- function(file) {
    dat <- readr::read_csv(file, show_col_types = FALSE)
    
    id_cols <- setdiff(names(dat), c("sum_t", "sum_no", "lines"))
    
    if (!"lines" %in% names(dat)) {
      if (length(id_cols) == 0) {
        return(tibble::tibble(id = "all", lines = NA_real_))
      } else {
        return(tibble::tibble(id = dat[[id_cols[1]]], lines = NA_real_))
      }
    }
    
    dat[["lines"]] <- suppressWarnings(as.numeric(dat[["lines"]]))
    
    if (length(id_cols) == 0) {
      return(tibble::tibble(
        id = "all",
        lines = sum(dat[["lines"]], na.rm = TRUE)
      ))
    }
    
    dat %>%
      dplyr::select(
        id = dplyr::all_of(id_cols[1]),
        lines = .data$lines
      )
  }
  
  get_file_time <- function(path) {
    fi <- file.info(path)
    if (!is.na(fi$ctime)) fi$ctime else fi$mtime
  }
  
  candidate_files_all <- list.files(
    path = main_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  
  diff_results <- purrr::imap_dfr(comparison_files, function(nominal_file, target_filename) {
    if (!file.exists(nominal_file)) {
      stop(sprintf("Fichier nominal introuvable pour '%s' : %s", target_filename, nominal_file))
    }
    
    candidate_files <- candidate_files_all[basename(candidate_files_all) == target_filename]
    
    if (length(candidate_files) == 0) {
      warning(sprintf("Aucun fichier '%s' trouvé dans %s", target_filename, main_dir))
      return(NULL)
    }
    
    candidate_files <- candidate_files[order(sapply(candidate_files, get_file_time))]
    
    nominal <- read_for_diff(nominal_file, value_col = value_col) %>%
      dplyr::rename(nominal_value = value)
    
    comparison_name <- tools::file_path_sans_ext(target_filename)
    
    purrr::map2_dfr(candidate_files, seq_along(candidate_files), function(step_file, i) {
      step_data <- read_for_diff(step_file, value_col = value_col) %>%
        dplyr::rename(step_value = value)
      
      step_name <- basename(dirname(step_file))
      
      step_data %>%
        dplyr::full_join(nominal, by = "id") %>%
        dplyr::mutate(
          comparison = comparison_name,
          compared_file = target_filename,
          step = step_name,
          step_index = i,
          step_file = step_file,
          nominal_file = nominal_file,
          step_value = dplyr::coalesce(step_value, 0),
          nominal_value = dplyr::coalesce(nominal_value, 0),
          diff_value = step_value - nominal_value,
          diff_pct = dplyr::if_else(
            nominal_value == 0,
            NA_real_,
            100 * (step_value - nominal_value) / nominal_value
          )
        )
    })
  })
  
  lines_results <- purrr::imap_dfr(comparison_files, function(nominal_file, target_filename) {
    candidate_files <- candidate_files_all[basename(candidate_files_all) == target_filename]
    
    if (length(candidate_files) == 0) {
      return(NULL)
    }
    
    candidate_files <- candidate_files[order(sapply(candidate_files, get_file_time))]
    comparison_name <- tools::file_path_sans_ext(target_filename)
    
    purrr::map2_dfr(candidate_files, seq_along(candidate_files), function(step_file, i) {
      lines_data <- read_for_lines(step_file)
      step_name <- basename(dirname(step_file))
      
      lines_data %>%
        dplyr::mutate(
          comparison = comparison_name,
          compared_file = target_filename,
          step = step_name,
          step_index = i,
          step_file = step_file
        )
    })
  })
  
  if (is.null(diff_results) || nrow(diff_results) == 0) {
    stop("Aucune comparaison n'a pu être réalisée.")
  }
  
  diff_results <- diff_results %>%
    dplyr::group_by(comparison) %>%
    dplyr::mutate(
      step = factor(step, levels = unique(step[order(step_index)]))
    ) %>%
    dplyr::ungroup()
  
  if (!is.null(filter_list)) {
    diff_results <- diff_results %>%
      dplyr::filter(
        purrr::map2_lgl(
          comparison, id,
          ~ {
            vals <- filter_list[[.x]]
            is.null(vals) || .y %in% vals
          }
        )
      )
  }
  
  if (!is.null(lines_results) && nrow(lines_results) > 0) {
    lines_results <- lines_results %>%
      dplyr::group_by(comparison) %>%
      dplyr::mutate(
        step = factor(step, levels = unique(step[order(step_index)]))
      ) %>%
      dplyr::ungroup()
  }
  if (!is.null(lines_results) && !is.null(filter_list)) {
    lines_results <- lines_results %>%
      dplyr::filter(
        purrr::map2_lgl(
          comparison, id,
          ~ {
            vals <- filter_list[[.x]]
            is.null(vals) || .y %in% vals
          }
        )
      )
  }
  
  comparison_names <- unique(diff_results$comparison)
  
  diff_plots <- lapply(comparison_names, function(comp) {
    dat <- diff_results %>% dplyr::filter(comparison == comp)
    
    ggplot2::ggplot(
      dat,
      ggplot2::aes(x = step, y = diff_pct, color = id, group = id)
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
      ) +
      ggplot2::labs(
        x = "Étapes / dossiers",
        y = "% de différence vs nominal",
        color = "Identifiant",
        title = paste("% de différence vs nominal -", comp)
      )
  })
  names(diff_plots) <- comparison_names
  
  lines_plots <- list()
  if (!is.null(lines_results) && nrow(lines_results) > 0) {
    lines_plots <- lapply(comparison_names, function(comp) {
      dat <- lines_results %>% dplyr::filter(comparison == comp)
      
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = step, y = lines, color = id, group = id)
      ) +
        ggplot2::geom_line(linewidth = 0.6) +
        ggplot2::geom_point(size = 1.5) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
        ) +
        ggplot2::labs(
          x = "Étapes / dossiers",
          y = "Nombre de lignes",
          color = "Identifiant",
          title = paste("Évolution du nombre de lignes -", comp)
        )
    })
    names(lines_plots) <- comparison_names
  }
  
  list(
    diff_data = diff_results,
    lines_data = lines_results,
    diff_plots = diff_plots,
    lines_plots = lines_plots
  )
}
