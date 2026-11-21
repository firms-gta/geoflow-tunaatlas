require(dplyr)
require(qs)

get_step_durations <- function(entity_dir, steps_to_run = NULL) {
  
  if (!dir.exists(entity_dir)) {
    stop("Directory does not exist: ", entity_dir)
  }
  
  markdown_dir <- file.path(entity_dir, "Markdown")
  
  if (!dir.exists(markdown_dir)) {
    stop("Markdown directory not found: ", markdown_dir)
  }
  
  step_dirs <- list.dirs(markdown_dir, recursive = FALSE, full.names = TRUE)
  step_data_paths <- file.path(step_dirs, "data.qs")
  keep <- file.exists(step_data_paths)
  
  step_dirs <- step_dirs[keep]
  step_data_paths <- step_data_paths[keep]
  
  if (length(step_dirs) == 0) {
    stop("No step directories with data.qs found in: ", markdown_dir)
  }
  
  step_info <- tibble::tibble(
    step_dir = step_dirs,
    step = basename(step_dirs),
    data_path = step_data_paths,
    file_time = file.info(step_data_paths)$mtime
  ) %>%
    dplyr::arrange(file_time) %>%
    dplyr::mutate(
      step_rank = dplyr::row_number()
    )
  
  if (!is.null(steps_to_run)) {
    steps_to_run <- unique(as.integer(steps_to_run))
    steps_to_run <- steps_to_run[!is.na(steps_to_run)]
    steps_to_run <- steps_to_run[steps_to_run >= 1 & steps_to_run <= nrow(step_info)]
    
    if (length(steps_to_run) == 0) {
      stop("No valid step index in steps_to_run.")
    }
    
    step_info <- step_info %>%
      dplyr::slice(steps_to_run)
  }
  
  step_info %>%
    dplyr::mutate(
      time_from_previous_sec = as.numeric(difftime(file_time, dplyr::lag(file_time), units = "secs")),
      time_from_previous_min = round(time_from_previous_sec / 60, 2),
      cumulative_sec = as.numeric(difftime(file_time, first(file_time), units = "secs")),
      cumulative_min = round(cumulative_sec / 60, 2)
    ) %>%
    dplyr::select(
      step_rank,
      step,
      file_time,
      time_from_previous_sec,
      time_from_previous_min,
      cumulative_sec,
      cumulative_min
    )
}


step_times <- get_step_durations(
  "~/firms-gta/geoflow-tunaatlas/jobs/20260313105100/entities/global_catch_ird_level2_1950_2024",
  steps_to_run = 1:33
)

readr::write_csv(x = step_times, file = "backup.csv")
