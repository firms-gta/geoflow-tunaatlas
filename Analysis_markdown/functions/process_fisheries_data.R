#' Process and Plot Fisheries Data
#'
#' This function processes fisheries data, applies filtering, calculates statistics, and generates plots of the results.
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
process_fisheries_data <- function(sub_list_dir_2, parameter_fact, parameter_filtering) {
  
  if (parameter_fact == "catch") {
    
    if (dir.exists("Markdown")) {
      nominal_dataset <- readr::read_csv("data/global_nominal_catch_firms_level0.csv")
      nominal_dataset <- filtering_function(nominal_dataset, parameter_filtering = parameter_filtering)
    }
    
    if (exists("nominal_dataset")) {
      nominal <- sum(nominal_dataset$measurement_value)
    } else {
      nominal <- 1
    }
    
    df <- data.frame(matrix(ncol = 13, nrow = 1))
    colnames(df) <- c(
      paste0(tail(str_split(paste0(sub_list_dir_2), "/")[[1]], n = 1)),
      "Explanation", "Functions",
      "Options", "Tons", "Number of fish", "Lines", "Difference (in % of tons)", "Difference in tons", "Difference (in % of fish)", "Difference in number of fish", "Difference (in % of lines)", "Percentage of nominal"
    )
    
    main <- filtering_function(readRDS(paste0(sub_list_dir_2[1], "/rds.rds")), parameter_filtering = parameter_filtering)
    tons_init <- sum((main %>% dplyr::filter(measurement_unit %in% c("MTNO", "MT", "t", "Tons")))$measurement_value)
    nofish_init <- sum((main %>% dplyr::filter(measurement_unit %in% c("NOMT", "NO", "no", "Number of fish")))$measurement_value)
    lines_init <- nrow(main)
    
    for (i in sub_list_dir_2) {
      Explanation <- readLines(paste0(i, "/explanation.txt"))[1]
      Functions <- readLines(paste0(i, "/functions.txt"))[1]
      if (file.exists(paste0(i, "/options_written.txt"))) {
        Options <- readLines(paste0(i, "/options_written.txt"))[1]
      } else {
        Options <- "Aucune"
      }
      if (isNullList(parameter_filtering)) {
        sums <- read_csv(paste0(i, "/sums.csv"))
        sum_t <- sums$sum_t
        sum_no <- sums$sum_no
        nrow <- sums$lines
      } else {
        main <- filtering_function(readRDS(paste0(i, "/rds.rds")), parameter_filtering = parameter_filtering)
        sum_t <- sum((main %>% dplyr::filter(measurement_unit %in% c("MTNO", "MT", "t", "Tons")))$measurement_value)
        sum_no <- sum((main %>% dplyr::filter(measurement_unit %in% c("NOMT", "NO", "no", "Number of fish")))$measurement_value)
        nrow <- nrow(main)
      }
      
      step <- tail(str_split(paste0(i), "/")[[1]], n = 1)
      Difference_percent <- -100 * ((tons_init - sum_t) / tons_init)
      Difference_tons <- -(tons_init - sum_t)
      Difference_no <- -(nofish_init - sum_no)
      Difference_percent_lines <- -100 * ((lines_init - nrow) / lines_init)
      Difference_percent_no <- -100 * ((nofish_init - sum_no) / nofish_init)
      percentage_of_nominal <- round((sum_t * 100) / nominal, 1)
      sums <- data.frame(sum_t, sum_no, nrow)
      data_i <- cbind(step,
                      Explanation, Functions,
                      Options,
                      sums, Difference_percent, Difference_tons, Difference_percent_no, Difference_no, Difference_percent_lines, percentage_of_nominal)
      names(data_i) <- colnames(df)
      df <- rbind(df, data_i)
      tons_init <- sum_t
      nofish_init <- sum_no
      lines_init <- nrow
    }
    
    df2 <- df[-1, ]
    df2[df2 == -Inf] <- 0
    colnames(df2)[1] <- "Step"
    
    reduced <- df2 %>%
      dplyr::mutate(`Millions of tons` = `Tons` / 1000000, `Millions of fish` = `Number of fish` / 1000000) %>%
      dplyr::select(Step, `Millions of tons`, `Millions of fish`,
                    "Difference (in % of tons)", "Difference (in % of fish)", "Percentage of nominal") %>%
      dplyr::mutate(`Step number` = as.numeric(row_number()))
    
    reduced$Step <- factor(reduced$Step, levels = (reduced %>% dplyr::arrange(`Step number`))$Step)
    
    coeff <- 3
    temperatureColor <- "#69b3a2"
    priceColor <- rgb(0.2, 0.6, 0.9, 1)
    
    second_graf <- ggplot(reduced, aes(x = Step, group = 1)) +
      geom_line(aes(y = `Millions of tons`, group = 1), size = 0.5, color = priceColor) +
      geom_point(aes(y = `Millions of tons`, group = 1)) +
      geom_line(aes(y = `Millions of fish` / coeff, group = 1), size = 0.5, color = temperatureColor) +
      geom_point(aes(y = `Millions of fish` / coeff)) +
      scale_y_continuous(
        name = "Tons",
        sec.axis = sec_axis(~ . * coeff, name = "Number of fish")
      ) +
      theme(
        axis.title.y = element_text(color = priceColor, size = 8),
        axis.title.y.right = element_text(color = temperatureColor, size = 8)
      ) +
      ggtitle("Evolution of the repartition of captures depending on units and Steps") +
      theme(axis.text.x = element_text(angle = 90))
    
    no_fish_plot <- ggplot(reduced, aes(x = Step, group = 1)) +
      geom_line(aes(y = `Millions of fish`, group = 1), size = 0.5) +
      theme(axis.text.x = element_text(angle = 90))
    
    tons_plot <- ggplot(reduced, aes(x = Step, group = 1)) +
      geom_line(aes(y = `Millions of tons`, group = 1), size = 0.5) +
      theme(axis.text.x = element_text(angle = 90))
    
    cowplot <- cowplot::plot_grid(no_fish_plot, tons_plot)
    columns_to_color = c("Difference (in % of tons)","Difference (in % of fish)")
    fig.capp = 'Evolution of captures in tons and number of fish during the process'
    
  } else if (parameter_fact == "effort") {
    
    nominal <- 1
    df <- data.frame(matrix(ncol = 13, nrow = 1))
    colnames(df) <- c(paste0(tail(str_split(paste0(sub_list_dir_2), "/")[[1]], n = 1)),
                      "Explanation", "Functions",
                      "Options", "Sum in hooks", "Sum in fishing days", "Lines", "Difference (in % of hooks)", "Difference in hooks", "Difference (in % of fishing days)", "Difference in fishing days",  "Difference (in % of lines)", "Percentage of nominal"
    )
    
    main <- filtering_function(readRDS(paste0(sub_list_dir_2[1], "/rds.rds")), parameter_filtering = parameter_filtering)
    hooks_init <- sum((main %>% dplyr::filter(measurement_unit %in% c("HOOKS", "HOOKS")))$measurement_value)
    fishing_days_init <- sum((main %>% dplyr::filter(measurement_unit %in% c("FDAYS", "FDAYS")))$measurement_value)
    lines_init <- nrow(main)
    
    for (i in sub_list_dir_2) {
      sums <- read.csv(paste0(i, "/sums.csv"))
      Explanation <- readtext(paste0(i, "/explanation.txt"))[1]
      Functions <- pull(readtext(paste0(i, "/functions.txt"))[1])
      if (file.exists(paste0(i, "/options_written.txt"))) {
        Options <- pull(readtext(paste0(i, "/options_written.txt"))[1])
      } else {
        Options <- "Aucune"
      }
      main <- filtering_function(readRDS(paste0(i, "/rds.rds")), parameter_filtering = parameter_filtering)
      sum_hooks <- sum((main %>% dplyr::filter(measurement_unit %in% c("HOOKS", "HOOKS")))$measurement_value)
      sum_fdays <- sum((main %>% dplyr::filter(measurement_unit %in% c("FDAYS", "FDAYS")))$measurement_value)
      nrow <- nrow(main)
      
      step <- tail(str_split(paste0(i), "/")[[1]], n = 1)
      Difference_percent <- -100 * ((hooks_init - sum_hooks) / hooks_init)
      Difference_hooks <- -(hooks_init - sum_fdays)
      Difference_fdays <- -(fishing_days_init - sum_hooks)
      Difference_percent_lines <- -100 * ((lines_init - nrow) / lines_init)
      Difference_percent_fdays <- -100 * ((fishing_days_init - sum_fdays) / fishing_days_init)
      percentage_of_nominal <- (sum_hooks * 100) / nominal
      sums <- as.data.frame(data.frame(sum_hooks, sum_fdays, nrow))
      data_i <- cbind(step,
                      Explanation, Functions,
                      Options,
                      sums, Difference_percent, Difference_hooks, Difference_percent_fdays, Difference_fdays, Difference_percent_lines, percentage_of_nominal)
      names(data_i) <- colnames(df)
      df <- rbind(df, data_i)
      hooks_init <- sum_hooks
      fishing_days_init <- sum_fdays
      lines_init <- nrow
    }
    
    df2 <- df[-1, ]
    df2[df2 == -Inf] <- 0
    colnames(df2)[1] <- "Step"
    
    reduced <- df2 %>% mutate(`Sum in millions of hooks` = `Sum in hooks` / 1000000, `Sum in millions of days` = `Sum in fishing days` / 1000000) %>% 
      dplyr::select(Step, `Sum in millions of hooks`, `Sum in millions of days`,
                    "Difference (in % of hooks)","Difference (in % of fishing days)") %>% 
      dplyr::mutate(`Step number` = as.numeric(row_number()))
    
    reduced$Step <- factor(reduced$Step, levels = (reduced %>% arrange(`Step number`))$Step)
    
    coeff <- 3
    temperatureColor <- "#69b3a2"
    priceColor <- rgb(0.2, 0.6, 0.9, 1)
    
    second_graf <- ggplot(reduced, aes(x = Step, group = 1)) + 
      geom_line(aes(y = `Sum in millions of hooks`, group = 1), size = 0.5, color = priceColor) + geom_point(aes(y = `Sum in millions of hooks`, group = 1)) +
      geom_line(aes(y = `Sum in millions of days` / coeff, group = 1), size = 0.5, color = temperatureColor) + geom_point(aes(y = `Sum in millions of days` / coeff))  +
      scale_y_continuous(
        name = "hooks",
        sec.axis = sec_axis(~ . * coeff, name = "fishing days")
      ) + 
      theme(
        axis.title.y = element_text(color = priceColor, size = 8),
        axis.title.y.right = element_text(color = temperatureColor, size = 8)
      ) +
      ggtitle("Evolution of the repartition of captures depending on units and Steps") +
      theme(axis.text.x = element_text(angle = 90))
    
    fishing_days_plot <- ggplot(reduced, aes(x = Step, group = 1)) + geom_line(aes(y = `Sum in millions of days`, group = 1), size = 0.5) + theme(axis.text.x = element_text(angle = 90))
    
    hooks_plot <- ggplot(reduced, aes(x = Step, group = 1)) + geom_line(aes(y = `Sum in millions of hooks`, group = 1), size = 0.5) + theme(axis.text.x = element_text(angle = 90))
    
    cowplot <- cowplot::plot_grid(hooks_plot, fishing_days_plot)
    
    columns_to_color = c("")
    fig.cap = 'Evolution of captures in fishing days and hooks during the process'
    
  }
  
  colnames(df2)[1] <-  "Treatment"
  
  
  df2 <- df2%>% dplyr::select(c("Treatment", "Explanation", "Functions"))%>% 
    rowwise()%>% mutate_all(function(.)toString(.)) %>% mutate_all(function(x)gsub("_", "-", x)) %>% 
    dplyr::mutate_if(is.character,~str_replace_all( ., "_", "-" )) %>%  dplyr::mutate_if(is.character,~str_replace_all( ., "\n", " " )) %>% 
    dplyr::mutate_if(is.character,~str_replace_all( ., "  ", " " ))%>%mutate_if(is.character, toString)
  
  return(list(reduced = reduced, cowplot = cowplot, second_graf = second_graf, df2 = df2,  
              columns_to_color = columns_to_color, fig.capp = fig.capp))
  
}





