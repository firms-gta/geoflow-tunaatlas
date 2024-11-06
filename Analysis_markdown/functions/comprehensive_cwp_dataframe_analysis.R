#'
#' This function performs a comprehensive analysis of initial and final datasets, including summary of differences, grouping differences, dimension comparisons, temporal and spatial analyses.
#'
#' @param init Initial dataset.
#' @param final Final dataset.
#' @param fig.path Path to save figures.
#' @param parameter_fact Fact parameter, default is "#catch".
#' @param parameter_short Whether to use short parameter names, default is FALSE.
#' @param parameter_columns_to_keep Columns to keep in the comparison.
#' @param parameter_diff_value_or_percent Difference value or percent, default is "Difference (in %)".
#' @param parameter_UNK_for_not_standards_unit Whether to use UNK for non-standard units, default is TRUE.
#' @param parameter_mapped Whether the data is mapped, default is TRUE.
#' @param parameter_filtering Filtering parameters.
#' @param parameter_time_dimension Time dimension parameters, default is c("time_start").
#' @param parameter_geographical_dimension Geographical dimension parameter, default is "geographic_identifier".
#' @param parameter_geographical_dimension_groupping Geographical dimension grouping parameter, default is "GRIDTYPE".
#' @param parameter_colnames_to_keep Column names to keep in the parameter.
#' @param outputonly Whether to output only.
#' @param print_map Whether to print the map, default is FALSE.
#' @param parameter_resolution_filter Resolution filter parameter.
#' @param parameter_titre_dataset_1 Title for dataset 1.
#' @param parameter_titre_dataset_2 Title for dataset 2.
#' @param unique_analyse Whether the analysis is unique.
#' @return A list containing results of various analyses.
#' @export
comprehensive_cwp_dataframe_analysis <- function(parameter_init, parameter_final, 
                                                 fig.path = getwd(),
                                                 parameter_fact = "catch",
                                                 parameter_short = FALSE,
                                                 parameter_columns_to_keep = c("Precision", "measurement_unit", "Values dataset 1",
                                                                               "Values dataset 2", "Loss / Gain",
                                                                               "Difference (in %)", "Dimension",
                                                                               "Difference in value"),
                                                 parameter_diff_value_or_percent = "Difference (in %)",
                                                 parameter_UNK_for_not_standards_unit = TRUE,
                                                 parameter_mapped = TRUE,
                                                 parameter_filtering = list(species = NULL, fishing_fleet = NULL),
                                                 parameter_time_dimension = c("time_start"),
                                                 parameter_geographical_dimension = "geographic_identifier",
                                                 parameter_geographical_dimension_groupping = "GRIDTYPE",
                                                 parameter_colnames_to_keep = "all",
                                                 outputonly = FALSE,
                                                 plotting_type = "view",
                                                 print_map = TRUE,
                                                 shapefile_fix = NULL,
                                                 continent = NULL,
                                                 coverage = TRUE,
                                                 parameter_resolution_filter = NULL,
                                                 parameter_titre_dataset_1 = "Dataset 1",
                                                 parameter_titre_dataset_2 = "Dataset 2",
                                                 unique_analyse = FALSE, 
                                                 removemap = FALSE) {
  # Process 'parameter_init'
  if (is.character(parameter_init)) {
    init <- read_data(parameter_init) %>% 
      dplyr::filter(!is.na(measurement_value))#%>% head(10000)
  } else if (is.data.frame(parameter_init)) {
    init <- parameter_init%>% 
      dplyr::filter(!is.na(measurement_value))
  } else {
    stop("Invalid 'parameter_init'")
  }
  
  # Process 'parameter_final'
  if (unique_analyse) {
    final <- init[0, ]
  } else {
    if (is.character(parameter_final)) {
      final <- read_data(parameter_final)%>% 
        dplyr::filter(!is.na(measurement_value))
    } else if (is.data.frame(parameter_final)) {
      final <- parameter_final%>% 
        dplyr::filter(!is.na(measurement_value))
    } else {
      stop("Invalid 'parameter_final'")
    }
  }
  
  if(!print_map | parameter_geographical_dimension_groupping %notin% colnames(init)){
    init <- init %>% dplyr::mutate(GRIDTYPE = "GRIDTYPE")
    final <- final %>% dplyr::mutate(GRIDTYPE = "GRIDTYPE")
  }
  
  if (is_null_or_not_exist(parameter_titre_dataset_2) & !unique_analyse) {
    if(!is.data.frame(parameter_final)) {
      parameter_titre_dataset_2 <- last_path_reduced(as.character(parameter_final))
    } else {
      parameter_titre_dataset_2 <- "Dataset 2"
    }
  } else if (unique_analyse) {
    parameter_titre_dataset_2 <- "NONE"
  } else {
    parameter_titre_dataset_2 <- parameter_titre_dataset_2
  }
  
  parameter_titre_dataset_2 <- gsub("_", "-", parameter_titre_dataset_2)
  parameter_titre_dataset_1 <- gsub("_", "-", parameter_titre_dataset_1)
  
  #cat("Tidying data...\n")
  if(length(parameter_colnames_to_keep) ==1 &&  parameter_colnames_to_keep == "all") {
    parameter_colnames_to_keep <- colnames(init)
  }
  parameter_colnames_to_keep <- unique(c(parameter_colnames_to_keep, parameter_geographical_dimension_groupping, parameter_geographical_dimension, parameter_time_dimension))
  
  init <- tidying_data(init, parameter_colnames_to_keep_dataframe = parameter_colnames_to_keep, time_dimension = parameter_time_dimension)
  final <- tidying_data(final, parameter_colnames_to_keep_dataframe = parameter_colnames_to_keep, time_dimension = parameter_time_dimension)
  
  colnames_intersect <- intersect(colnames(init), colnames(final))
  
  init <- init %>% dplyr::select(colnames_intersect)
  final <- final %>% dplyr::select(colnames_intersect)
  
  #cat("Renaming geographic identifiers and handling non-standard units...\n")
  formals(function_geographic_identifier_renaming_and_not_standards_unit, envir = environment())$geo_dim = parameter_geographical_dimension
  formals(function_geographic_identifier_renaming_and_not_standards_unit, envir = environment())$parameter_fact = parameter_fact
  formals(function_geographic_identifier_renaming_and_not_standards_unit, envir = environment())$parameter_UNK_for_not_standards_unit = parameter_UNK_for_not_standards_unit
  formals(function_geographic_identifier_renaming_and_not_standards_unit, envir = environment())$geo_dim_group = parameter_geographical_dimension_groupping
  
  init <- function_geographic_identifier_renaming_and_not_standards_unit(init)
  final <- function_geographic_identifier_renaming_and_not_standards_unit(final)
  
  #cat("Applying filtering function...\n")
  formals(filtering_function, envir = environment())$parameter_filtering = parameter_filtering
  init <- filtering_function(init)
  
  if(nrow(init) == 0){
    stop("Filtering has removed all rows")
  }
  
  if (unique_analyse) {
    final <- init[0,]
  } else {
    final <- filtering_function(final)
  }
  
  # Ensure all necessary variables exist and set default values
  if (!exists("fig.path")) {
    fig.path <- getwd()
  }
  
  #cat("Grouping differences...\n")
  groupping_differences_list <- groupping_differences(init, final, parameter_time_dimension, parameter_geographical_dimension, parameter_geographical_dimension_groupping)
  
  Groupped_all <- groupping_differences_list$Groupped_all
  Other_dimensions <- groupping_differences_list$Other_dimensions
  Other_dimensions <- Other_dimensions[Other_dimensions != "time_end"]
  
  time_dimension_list_groupped <- groupping_differences_list$Groupped_time_dimension
  
  GrouppedGRIDTYPE <- groupping_differences_list$GrouppedGRIDTYPE
  
  if (!unique_analyse) {
    summary_of_differences <- compute_summary_of_differences(init, final, parameter_titre_dataset_1, parameter_titre_dataset_2)
    compare_strata_differences_list <- compare_strata_differences(init, final, Groupped_all, parameter_titre_dataset_1, parameter_titre_dataset_2, parameter_columns_to_keep, unique_analyse)
    compare_strata_differences_list$title <- paste0("Disappearing or appearing strata between ", parameter_titre_dataset_1, " and ", parameter_titre_dataset_2)
    
    
    compare_dimension_differences_list <- compare_dimension_differences(Groupped_all, Other_dimensions, parameter_diff_value_or_percent, parameter_columns_to_keep, topn = 6)
    compare_dimension_differences_list$title <-paste0("Difference between the non appearing/disappearing stratas between ", parameter_titre_dataset_1, " and ", parameter_titre_dataset_2)
    
    if (length(parameter_time_dimension) != 0) {
      plot_titles_list <- compare_temporal_differences(parameter_time_dimension, init, final, parameter_titre_dataset_1, parameter_titre_dataset_2, unique_analyse = FALSE)
    }
    
    if (length(parameter_geographical_dimension) != 0) {
      Geographicdiff <- geographic_diff(init, final, shapefile_fix, parameter_geographical_dimension, parameter_geographical_dimension_groupping, continent, plotting_type = plotting_type, parameter_titre_dataset_1, 
                                        parameter_titre_dataset_2, outputonly)
      if(removemap){
        Geographicdiff$plott <- NULL
        gc()
      }
    }
    
  } else {
    summary_of_differences <- NULL
    compare_strata_differences_list <- NULL
    plot_titles_list <- NULL
    Geographicdiff <- NULL
    compare_dimension_differences_list <- NULL
    GrouppedGRIDTYPE <- NULL
  }
  
  if (length(parameter_time_dimension) != 0 && coverage) {
    time_coverage_analysis_list <- time_coverage_analysis(time_dimension_list_groupped, parameter_time_dimension, parameter_titre_dataset_1, parameter_titre_dataset_2, unique_analyse)
  } else {
    time_coverage_analysis_list <- NULL
  }
  
  if(coverage){
    other_dimension_analysis_list <- other_dimension_analysis(Other_dimensions, init, final, parameter_titre_dataset_1, parameter_titre_dataset_2, unique_analyse)
  } else {
    other_dimension_analysis_list <- NULL
  }
  if (print_map && coverage) {
    spatial_coverage_analysis_list <- spatial_coverage_analysis(init, final, parameter_titre_dataset_1, parameter_titre_dataset_2, shapefile_fix, plotting_type = plotting_type, continent, TRUE, GrouppedGRIDTYPE, savingimages = FALSE)
  } else {
    spatial_coverage_analysis_list <- NULL
  }
  combined_summary_histogram_function <- function(init, parameter_titre_dataset_1 = "Init", final, parameter_titre_dataset_2 = "Final") {
    setDT(init)  
    summary_number_row_init <- init[, .(Number_different_stratas = .N), by = measurement_unit]
    summary_number_row_init[, data_source := parameter_titre_dataset_1]  
    
    
    setDT(final)  
    summary_number_row_final <- final[, .(Number_different_stratas = .N), by = measurement_unit]
    summary_number_row_final[, data_source := parameter_titre_dataset_2] 
    combined_summary <- rbind(summary_number_row_init, summary_number_row_final)
    combined_summary[, Percent := Number_different_stratas / sum(Number_different_stratas) * 100, by = data_source]
    
    
    
    total_rows <- combined_summary[, .(Total_rows = sum(Number_different_stratas)), by = data_source]
    combined_summary <- merge(combined_summary, total_rows, by = "data_source")
    
    combined_summary_histogram <- ggplot(combined_summary, aes(x = factor(paste0(data_source, "\n(Number of different stratas=", Total_rows, ")")), y = Percent, fill = measurement_unit)) +
      geom_bar(stat = "identity", position = "fill") +  # Stacked bars with 100% scale
      scale_fill_manual(values = c("Tons" = "#4C72B0", "Number of fish" = "#55A868")) +  # Custom blue-green colors
      scale_y_continuous(labels = scales::percent_format()) + geom_text(aes(label = paste0(round(Percent, 1), "%")), 
                                                                        position = position_fill(vjust = 0.5), color = "black") +# y-axis as percentages
      labs(title = "Distribution of number strata for each measurement_unit by dataset",
           x = "Dataset",
           y = "Percentage",
           fill = "Measurement unit") +
      theme_minimal()
    return(combined_summary_histogram)
  }
  
  combined_summary_histogram <- combined_summary_histogram_function(init, parameter_titre_dataset_1, final, parameter_titre_dataset_2)
  
  rm(init)
  rm(final)
  gc()
  
  return(list(combined_summary_histogram = combined_summary_histogram, 
    summary_of_differences = summary_of_differences,
    compare_strata_differences_list = compare_strata_differences_list,
    groupping_differences_list = groupping_differences_list,
    compare_dimension_differences_list = compare_dimension_differences_list,
    plot_titles_list = plot_titles_list,
    Geographicdiff = Geographicdiff,
    time_coverage_analysis_list = time_coverage_analysis_list,
    spatial_coverage_analysis_list = spatial_coverage_analysis_list,
    other_dimension_analysis_list = other_dimension_analysis_list,
    Other_dimensions = Other_dimensions, 
    coverage = coverage, unique_analyse = unique_analyse, 
    parameter_titre_dataset_1 = parameter_titre_dataset_1, 
    parameter_titre_dataset_2 = parameter_titre_dataset_2, 
    parameter_filtering = parameter_filtering, 
    parameter_resolution_filter = parameter_resolution_filter, 
    fig.path = fig.path
  ))
}
