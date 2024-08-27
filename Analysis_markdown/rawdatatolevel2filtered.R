

species_lvl2 <- unique(lvl2RF2$species)
parameter_filteringgearspecies <- list(species = c("ALB", "SKJ" , "YFT",  "SBF" ,"BFT",  "SWO" , "BET","PBF"), gear_type = c("09.32", "01.1", "01.2"))

formals(comprehensive_cwp_dataframe_analysis, envir = environment())$parameter_colnames_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet", 
                                                                              "fishing_mode", "geographic_identifier", 
                                                                              "measurement_unit", "measurement_value", "GRIDTYPE", 
                                                                              "species_group", "Gear")

child_env_first_to_last_result_filtered <- comprehensive_cwp_dataframe_analysis(
  parameter_init = sub_list_dir_2[1],
  parameter_final = sub_list_dir_2[length(sub_list_dir_2)],
  fig.path = parameters_child_global$fig.path,
  parameter_filtering = parameter_filteringgearspecies,
  shapefile_fix = shapefile.fix,
  continent = continent,
  parameter_titre_dataset_1 = basename(sub_list_dir_2[1]),
  parameter_titre_dataset_2 = basename(sub_list_dir_2[length(sub_list_dir_2)])
)
new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/initfinalfiltered_",basename(sub_list_dir_2[1]),"_", basename(sub_list_dir_2[length(sub_list_dir_2)])))

dir.create(new_path, recursive = TRUE)
child_env_first_to_last_result_filtered$fig.path = new_path

child_env_first_to_last_result_filtered$step_title_t_f <- FALSE
child_env_first_to_last_result_filtered$parameter_short <- FALSE
child_env_first_to_last_result_filtered$parameter_mapped <- TRUE
child_env_first_to_last_result_filtered$unique_analyse <- FALSE
child_env_first_to_last_result_filtered$parameter_titre_dataset_1 <- basename(sub_list_dir_2[1])
child_env_first_to_last_result_filtered$parameter_titre_dataset_2 <- basename(sub_list_dir_2[length(sub_list_dir_2)])
child_env_first_to_last_result_filtered$child_header <- "#"
child_env_first_to_last_result_filtered$parameter_filtering <- parameter_filteringgearspecies

child_env_first_to_last_result_filteredenv <- list2env(child_env_first_to_last_result_filtered, parent = new.env())


rmarkdown::render("comparison.Rmd", envir = child_env_first_to_last_result_filteredenv, output_format = "bookdown::html_document2", 
                  output_file = "comparison_raw_to_lvl2RF2_filtered")


child_env_level1_to_last_result_filtered <- comprehensive_cwp_dataframe_analysis(
  parameter_init = sub_list_dir_2[5],
  parameter_final = sub_list_dir_2[length(sub_list_dir_2)],
  fig.path = parameters_child_global$fig.path,
  parameter_filtering = parameter_filteringgearspecies,
  shapefile_fix = shapefile.fix,
  continent = continent,
  parameter_titre_dataset_1 = basename(sub_list_dir_2[5]),
  parameter_titre_dataset_2 = basename(sub_list_dir_2[length(sub_list_dir_2)]),
  unique_analyse = FALSE
)



child_env_level1_to_last_result_filtered$parameter_filtering <- parameter_filteringgearspecies

child_env_level1_to_last_result_filtered$step_title_t_f <- FALSE
child_env_level1_to_last_result_filtered$parameter_short <- FALSE
child_env_level1_to_last_result_filtered$parameter_mapped <- TRUE
child_env_level1_to_last_result_filtered$unique_analyse <- FALSE
child_env_level1_to_last_result_filtered$parameter_titre_dataset_1 <- basename(sub_list_dir_2[5])
child_env_level1_to_last_result_filtered$parameter_titre_dataset_2 <- basename(sub_list_dir_2[length(sub_list_dir_2)])
child_env_level1_to_last_result_filtered$child_header <- "#"
new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/level1level2"))

dir.create(new_path, recursive = TRUE)
child_env_level1_to_last_result_filtered$fig.path = new_path

child_env_level1_to_last_result_filteredenv <- list2env(child_env_level1_to_last_result_filtered, parent = new.env())

rmarkdown::render("comparison.Rmd", envir = child_env_level1_to_last_result_filteredenv, output_format = "bookdown::html_document2", 
                  output_file = "comparisonlevel1tolevel2filteredgearspecies")




## Level 1 to level 2RF1

child_env_level1_to_RF1_result_filtered <- comprehensive_cwp_dataframe_analysis(
  parameter_init = sub_list_dir_2[5],
  parameter_final = sub_list_dir_2[6],
  fig.path = parameters_child_global$fig.path,
  parameter_filtering = parameter_filteringgearspecies,
  shapefile_fix = shapefile.fix,
  continent = continent,
  parameter_titre_dataset_1 = "Level1dataset",
  parameter_titre_dataset_2 = "Level2R1dataset",
  unique_analyse = FALSE
)



child_env_level1_to_RF1_result_filtered$parameter_filtering <- parameter_filteringgearspecies

child_env_level1_to_RF1_result_filtered$step_title_t_f <- FALSE
child_env_level1_to_RF1_result_filtered$parameter_short <- FALSE
child_env_level1_to_RF1_result_filtered$parameter_mapped <- TRUE
child_env_level1_to_RF1_result_filtered$unique_analyse <- FALSE
child_env_level1_to_RF1_result_filtered$parameter_titre_dataset_1 <- basename(sub_list_dir_2[5])
child_env_level1_to_RF1_result_filtered$parameter_titre_dataset_2 <- basename(sub_list_dir_2[6])
child_env_level1_to_RF1_result_filtered$child_header <- "#"
new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/level1level2RF1"))

dir.create(new_path, recursive = TRUE)
child_env_level1_to_RF1_result_filtered$fig.path = new_path

child_env_level1_to_RF1_result_filteredenv <- list2env(child_env_level1_to_RF1_result_filtered, parent = new.env())

rmarkdown::render("comparison.Rmd", envir = child_env_level1_to_RF1_result_filteredenv, output_format = "bookdown::html_document2", 
                  output_file = "comparisonlevel1tolevel2filteredgearspecies")


