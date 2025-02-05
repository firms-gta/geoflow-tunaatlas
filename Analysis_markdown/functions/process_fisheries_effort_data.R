#' Process and Plot Fisheries Data
#'
#' This function processes fisheries data (catch or effort), applies filtering, calculates statistics, and generates plots of the results.
#'
#' @param sub_list_dir_2 List of directories containing the data files.
#' @param parameter_fact Character string specifying the type of data ("catch" or "effort").
#' @param parameter_filtering List of filtering parameters to be passed to the filtering function.
#'
#' @return A list containing the processed data frame and the generated plots.
#' @examples
#' \dontrun{
#' result <- process_fisheries_data(sub_list_dir_2, "catch", parameter_filtering)
#' print(result$processed_data)
#' print(result$second_graf)
#' print(result$no_fish_plot)
#' print(result$tons_plot)
#' }
process_fisheries_effort_data <- function(sub_list_dir_2, parameter_filtering) {
  main <- filtering_function(qs::qread(paste0(sub_list_dir_2[1], "/data.qs")), parameter_filtering = parameter_filtering)
  top_units <- main %>% 
    dplyr::group_by(measurement_unit) %>% 
    dplyr::summarize(total_value = sum(measurement_value, na.rm = TRUE)) %>% 
    dplyr::arrange(desc(total_value)) %>% 
    dplyr::slice_head(n = 5) %>% 
    dplyr::pull(measurement_unit)
  
  df <- data.frame(Step = character(), Explanation = character(), Functions = character(), Options = character())
  prev_values <- setNames(rep(NA, length(top_units)), top_units)
  
  for (unit in top_units) {
    df[[paste0(unit, "_value")]] <- numeric()
    # df[[paste0(unit, "_difference")]] <- numeric()
    df[[paste0(unit, "_difference_percent")]] <- numeric()
  }
  
  for (i in sub_list_dir_2) {
    Explanation <- readLines(paste0(i, "/explanation.txt"))[1]
    Functions <- readLines(paste0(i, "/functions.txt"))[1]
    Options <- if (file.exists(paste0(i, "/options_written.txt"))) readLines(paste0(i, "/options_written.txt"))[1] else "None"
    
    main <- filtering_function(qs::qread(paste0(i, "/data.qs")), parameter_filtering = parameter_filtering)
    effort_now <- main %>% dplyr::filter(measurement_unit %in% top_units) %>% 
      dplyr::group_by(measurement_unit) %>% 
      dplyr::summarize(value = sum(measurement_value, na.rm = TRUE))
    
    step <- tail(strsplit(i, "/")[[1]], 1)
    data_i <- data.frame(Step = step, Explanation = Explanation, Functions = Functions, Options = Options)
    
    for (unit in top_units) {
      value <- effort_now %>% dplyr::filter(measurement_unit == unit) %>% dplyr::pull(value)
      prev_value <- prev_values[[unit]]
      difference <- ifelse(is.na(prev_value), NA, value - prev_value)
      difference_percent <- ifelse(is.na(prev_value) | prev_value == 0, NA, -100 * ((prev_value - value) / prev_value))
      data_i[[paste0(unit, "_value")]] <- value
      # data_i[[paste0(unit, "_difference")]] <- difference
      data_i[[paste0(unit, "_difference_percent")]] <- difference_percent
      prev_values[[unit]] <- value
    }
    df <- rbind(df, data_i)
  }
  
  df$Step <- factor(df$Step, levels = unique(df$Step))
  df[df == -Inf] <- 0
  
  reduced <- df %>% dplyr::mutate(across(contains("_value"), ~ . / 1000000, .names = "Millions_of_{col}")) %>% dplyr::select(-c(Explanation, Functions, Options))
  
  top2_units <- top_units[seq_len(min(2, length(top_units)))]
  
  effort_plots <- list()
  for (unit in top2_units) {
    effort_plot <- ggplot(df, aes(x = Step, y = .data[[paste0(unit, "_value")]], group = 1)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + 
      ggtitle(paste("Evolution of effort in", unit)) +
      theme(axis.text.x = element_text(angle = 90))
    effort_plots[[unit]] <- effort_plot
  }
  cowplot <- cowplot::plot_grid(plotlist = effort_plots)
  columns_to_color <- NULL
  fig.capp <- 'Evolution of effort during the process'
  
  return(list(reduced = reduced, df2 = df, second_graf = cowplot, cowplot = cowplot, columns_to_color = columns_to_color, fig.capp = fig.capp))
}