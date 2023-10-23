Tidying_and_mapping_data = function(action, entity, config) {

  # Define all required packages
  required_packages <- c(
    "webshot", "here", "usethis", "ows4R", "sp", "data.table", "flextable", 
    "readtext", "sf", "dplyr", "stringr", "tibble", "bookdown", "knitr", 
    "purrr", "readxl", "base", "remotes", "utils", "DBI", "odbc", "rlang", 
    "kableExtra", "readr", "tidyr", "ggplot2", "stats", "RColorBrewer", 
    "cowplot", "tmap", "RPostgreSQL", "curl", "officer", "gdata", "googledrive",
    "R3port", "reshape2", "tools" 
  )
  
  # Install and load required packages
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
  
  lapply(required_packages, install_and_load)
  
  # Set options and configurations
  harmonized <- entity$resources$harmonized
  output_name_dataset_mapped <- gsub("harmonized", "mapped", harmonized)
  if (grepl("nominal", harmonized)){
    return()
  }
  
  opts <- action$options
  con <- config$software$output$dbi
  options(encoding = "UTF-8")
  opts$fact <- ifelse(grepl("effort", harmonized), "effort", "catch")
  recap_each_step <- TRUE
  
  # Define step logger function
  stepLogger <- function(level, step, msg) {
    config$logger.info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  
  # Define the base URL for scripts
  base_url <- "~/Documents/geoflow-tunaatlas/tunaatlas_scripts/generation"
  
  # Source scripts from URLs
  scripts <- c(
    "map_codelists.R"
  )
  source("~/Documents/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/spatial_curation_data_mislocated.R")
  source("~/Documents/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/curation_absurd_converted_data.R")
  
  lapply(scripts, function(script) source(file.path(base_url, script)))
  
  source("~/Documents/geoflow-tunaatlas/tunaatlas_scripts/pre-harmonization/outside_juridiction.R")
  
  # Local script sourcing
  local_scripts <- file.path("~/Documents/geoflow-tunaatlas/Analysis_markdown/", scripts)
  
  # Additional scripts for reporting and functions
  reporting_functions <- c(
    "functions/write_options_to_csv.R",
    "functions/function_recap_each_step.R",
    "functions/copyrmd.R",
    "functions/function_write_RDS.R"
  )
  
  lapply(reporting_functions, function(func) {
    source(file.path("~/Documents/geoflow-tunaatlas/Analysis_markdown/", func))})
  
  # Save options in a CSV file
  write_options_to_csv(opts)
  

  stepnumber <- 1

  df_to_load <- as.data.frame(readr::read_csv(harmonized, guess_max=0)) %>%
    mutate(measurement_value = as.numeric(measurement_value))
  
  `%notin%` <- Negate(`%in%`)
  
  if("species" %notin% colnames(df_to_load)){
    fact = "effort"
    opts$fact <- "effort"
  }

  if(recap_each_step){
    function_recap_each_step(
      "rawdata",
      df_to_load)
  }

# Curation absurd converted data ------------------------------------------
  stepLogger(level = 0, step = stepnumber, msg = "Curation absurd converted data")
  stepnumber = stepnumber+1

  max_conversion_factor <-
    read.csv("~/Documents/geoflow-tunaatlas/data/max_conversion_factor.csv")

  curation_absurd_converted_data_list <-
    curation_absurd_converted_data(georef_dataset = df_to_load,
                                   max_conversion_factor = max_conversion_factor)
  
  georef_dataset <- curation_absurd_converted_data_list$georef_dataset

  if(exists("not_conform_conversion_factors")){
    rm(not_conform_conversion_factors)
  }
  
  not_conform_conversion_factors <- curation_absurd_converted_data_list$conversion_factor_not_to_keep

  if(nrow(not_conform_conversion_factors) != 0){

    function_recap_each_step(
      "Removing_absurd_nomt",
      georef_dataset,
      "In this step, we target implausible data. We check data having declaration both in NOMT and MTNO and if the conversion factor is implausible.
					   We either remove NOMT strata which corresponds to MTNO declaration implausible or me remove the corresponding MTNO data. More details are available in the pdf file attached.",
      "spatial_curation_data_mislocated"
    )
    saveRDS(not_conform_conversion_factors, "Markdown/report/not_conform_conversion_factors.rds")

  }



#----------Standardizing unit of measures---------------------------------------------------------------------------------------------------------------------------
stepLogger(level = 0, step = stepnumber, msg = "Standardizing unit of measures")
stepnumber = stepnumber+1
#-------------------------------------------------------------------------------------------------------------------------------------

unit_weight_to_remap = c("MT", "MTNO")
unit_number_to_remap = c("NO", "NOMT")
georef_dataset <- georef_dataset %>%
  dplyr::mutate(
    measurement_unit = case_when(
      measurement_unit %in% unit_weight_to_remap ~ "t",
      measurement_unit %in% unit_number_to_remap ~ "no",
      TRUE ~ measurement_unit
    )
  )

#--------Negative or null values ------------------------------------------------------------------------------------------------------------------------------------
#Negative or null values 
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

negative_values <- georef_dataset %>% dplyr::filter(measurement_value <= 0)
if(nrow(negative_values)!=0){
if(recap_each_step){
  function_recap_each_step(
    "negative_values",
    georef_dataset,
    paste0(
      "In this step,handle negative values in the measurement_values of the data"
    ) 
    
  )
  
  saveRDS(negative_values,"data/negative_values.rds")
  
  # names_list_irregular_areas <-
  #   c("negative_values") #file we want to save
  # 
  # try(lapply(names_list_irregular_areas, function_write_RDS))
}
}


#--------irregular areas------------------------------------------------------------------------------------------------------------------------------------
#Irregular areas handling in case of area not corresponding to cwp
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

opts$irregular_area <- "remove"
stepLogger(level = 0, step = stepnumber, msg = "Irregular areas handling")
stepnumber = stepnumber+1

  spatial_curation <-
    spatial_curation(con, georef_dataset, opts$irregular_area)
  georef_dataset <- spatial_curation$df
  
  if(exists("removed_irregular_areas")){
    rm(removed_irregular_areas)
  }

  removed_irregular_areas <- spatial_curation$df_input_areas_not_curated
  stats_irregular_areas <- spatial_curation$stats
  


  if (!is.null(removed_irregular_areas) && is.data.frame(removed_irregular_areas) && nrow(removed_irregular_areas) != 0) {


    if(recap_each_step){
      function_recap_each_step(
        "irregular_area_handling",
        georef_dataset,
        paste0(
          "In this step, we handle areas that does not match cwp grids norme"
        ) ,
        "function_overlapped"
      )
      
      saveRDS(removed_irregular_areas,"data/removed_irregular_areas.rds")
      
      # names_list_irregular_areas <-
      #   c("removed_irregular_areas", "stats_irregular_areas") #file we want to save
      # 
      # try(lapply(names_list_irregular_areas, function_write_RDS))
    }

}




#-----------spatial_curation_data_mislocated------------------------------------------------------

opts$spatial_curation_data_mislocated <- "remove"

  stepLogger(level = 0, step = stepnumber, msg = sprintf("Reallocation of mislocated data  (i.e. on land areas or without any spatial information) (data with no spatial information have the dimension 'geographic_identifier' set to 'UNK/IND' or 'NA'). Option is: [%s] ", opts$spatial_curation_data_mislocated))
  stepnumber = stepnumber+1


  spatial_curation_data_mislocated_list <- spatial_curation_data_mislocated(
    entity = entity,
    config = config,
    df = georef_dataset,
    spatial_curation_data_mislocated = opts$spatial_curation_data_mislocated
  )

  if(exists("areas_in_land")){
    rm(areas_in_land)
  }

  georef_dataset <- spatial_curation_data_mislocated_list$dataset
  areas_in_land <- spatial_curation_data_mislocated_list$areas_in_land

  if (!is.null(areas_in_land) && is.data.frame(areas_in_land) && nrow(areas_in_land) != 0) {

    if(recap_each_step){
    function_recap_each_step(
      "Realocating_removing_mislocated_data",
      georef_dataset,
      "In this step, the mislocated data is hanlded. Either removed, reallocated or let alone, the data on continent and the data outside the competent rfmo area are targeted. ",
      "spatial_curation_data_mislocated"
    )
    saveRDS(areas_in_land,"data/areas_in_land.rds")
      # names_list_irregular_areas <-
      #   c("areas_in_land") #file we want to save
      # 
      # try(lapply(names_list_irregular_areas, function_write_RDS))

  }
  gc()


  }




# Outside juridiction -----------------------------------------------------

function_outside_juridiction <- function_outside_juridiction(georef_dataset,
                                               con)
if(exists("outside_juridiction")){
  rm(outside_juridiction)
}
  
georef_dataset <- function_outside_juridiction$georef_dataset
outside_juridiction <- function_outside_juridiction$outside_juridiction

if(!is.null(outside_juridiction) && is.data.frame(outside_juridiction) && nrow(outside_juridiction) != 0) {

  if(recap_each_step){
    function_recap_each_step(
      "outside_juridiction",
      georef_dataset,
      paste0(
        "In this step, we handle areas that does not match with the shape of the source_authority declaring"
      ) ,
      "function_outside_juridiction",
    )
    saveRDS(outside_juridiction,"data/outside_juridiction.rds")
    
    # names_outside_juridiction <-
    #   c("outside_juridiction") #file we want to save
    # 
    # try(lapply(names_outside_juridiction, function_write_RDS))
  }

  }
#----------Map code lists -------------------------------------------------------------------------------------------------------------------------------------------------
#Map to CWP standard codelists (if not provided by tRFMO according to the CWP RH standard data exchange format)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options("OutDec" = ".")
source_authority_to_map = if(!is.null(opts$source_authority_to_map)) opts$source_authority_to_map else c("CCSBT", "IATTC", "WCPFC")

if(any(unique(georef_dataset$source_authority)%in%source_authority_to_map)){
    stepLogger(level = 0, step = stepnumber, msg = "Map to CWP standard codelists (if not provided by tRFMO according to the CWP RH standard data exchange format)")
    stepnumber = stepnumber+1
    config$logger.info(
      "Reading the CSV containing the dimensions to map + the names of the code list mapping datasets. Code list mapping datasets must be available in the database."
    )
    mapping_csv_mapping_datasets_url <- "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv"
    mapping_dataset <-
      read.csv(
        mapping_csv_mapping_datasets_url,
        stringsAsFactors = F,
        colClasses = "character"
      )
    mapping_keep_src_code <- if (!is.null(opts$mapping_keep_src_code)) opts$mapping_keep_src_code else FALSE

    config$logger.info("Mapping code lists of georeferenced datasets...")
    mapping_codelist <-
      map_codelists(
        con,
        opts$fact,
        mapping_dataset = mapping_dataset,
        dataset_to_map = georef_dataset,
        mapping_keep_src_code,
        summary_mapping = TRUE,
        source_authority_to_map = source_authority_to_map
      ) #this map condelist function is to retrieve the mapping dataset used

        georef_dataset <- mapping_codelist$dataset_mapped
        
        if(dimension_to_map=="fishing_fleet"){
          replace_unk <- "NEI"
        } else if(dimension_to_map=="species"){
          replace_unk <- "MZZ"
        }
        if(dimension_to_map=="gear_type"){
          replace_unk <- "99.9"
        } else {replace_unk <- "UNK"}
        
       df_input <- df_input %>% mutate(ifelse(fishing_fleet == "UNK", "NEI", fishing_fleet),
                                       ifelse(species == "UNK", "MZZ", species),
                                       ifelse(gear_type == "UNK", "99.9", gear_type))

    config$logger.info("Mapping code lists of georeferenced datasets OK")


    if(recap_each_step){

      recap_mapping <- mapping_codelist$recap_mapping
      stats_total <- mapping_codelist$stats_total
      not_mapped_total <- mapping_codelist$not_mapped_total

      # names_list <-
      #   c("recap_mapping", "stats_total", "not_mapped_total") #file we want to save
      
      saveRDS(not_mapped_total,"data/not_mapped_total.rds")
      saveRDS(recap_mapping,"data/recap_mapping.rds")
      

      # try(lapply(names_list, function_write_RDS))

      # if(nrow(not_mapped_total)!=0){
      #   rmarkdown::render(, not_mapped_total)
      # }

      config$logger.info("Saving recap of mapping ok")

      function_recap_each_step(
        "mapping_codelist",
        georef_dataset,
        "This step is to map all the data with the same codes for gears, species, and fishingfleet,
	that is to say, to put all the data coming from different RFMOs in the same langage with the same
	codes. Coding systems and nomenclatures used to describe the data may differ according to tRFMOs.
	Codes used by the tuna RFMOs in their respective datasets were mapped with global code lists for
	gear (ISSCFG), flag (ISO3 countries codes), and species (ASFIS). Some codes could not be mapped
	to standard code lists, for some tRFMOs own-defined codes that usually are aggregation of existing
	codes (e.g. flag ’IDPH’ standing for Indonesia and Philippines within WCPFC or the species “Otun”
	standing for other tuna within for ICCAT). In those cases, the code was set to UNK (Unknown). For
	species and gears, these codes were mapped with more aggregated code lists, i.e. resp. group of species
	and groups of gears.",
        "map_codelists"
      )
      
      df_mapping_final_this_dimension <- recap_mapping %>%
        # dplyr::filter(species %in% unique(georef_dataset$species)) %>%
        # dplyr::filter(fishing_fleet %in% unique(georef_dataset$fishing_fleet)) %>%
        # dplyr::filter(gear_type %in% unique(georef_dataset$gear_type)) %>%
        dplyr::filter(source_authority %in% unique(georef_dataset$source_authority))
      # rmarkdown::render("~/Documents/geoflow-tunaatlas/Analysis_markdown/Checking_raw_files_markdown/Mapping_recap.Rmd",
      #                   envir = environment(),
      #                   output_dir = getwd())
      gc()

}

}


    files_to_check <- c("data/not_conform_conversion_factors.rds",
                        "data/removed_irregular_areas.rds",
                        "data/areas_in_land.rds",
                        "data/outside_juridiction.rds",
                        "data/not_mapped_total.rds")

    if(any(file.exists(files_to_check))) {
      parameter_directory <- getwd()
      base::options(knitr.duplicate.label = "allow")
      # rmarkdown::render("~/Documents/geoflow-tunaatlas/Analysis_markdown/Checking_raw_files_markdown/Report_on_raw_data.Rmd",
      #                   envir = environment(),
      #                   output_dir = getwd())
    }






#Filter on species under mandate for FIRMS level 0
#-------------------------------------------------------------------------------------------------
stepLogger(level = 0, step = stepnumber, msg = "Filter on species under mandate for FIRMS level 0")
stepnumber = stepnumber+1

# Temporary patch for ASFIS RMJ --> RMM
if("species" %in% colnames(georef_dataset)){
georef_dataset <- georef_dataset %>% dplyr::mutate(species = case_when(species == "RMJ" ~ "RMM", TRUE ~ species))
opts$fact <- "effort"
fact <- "effort"

# Filtering on species under mandate --------------------------------------
# done base on mapping between source_authority (tRFMO) and species
stepLogger(level = 0, step = stepnumber, msg = "Filtering on species under mandate, targeted by GTA")
stepnumber = stepnumber +1
#url_asfis_list <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_species_level0.csv"

url_mapping_asfis_rfmo = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/cross-term/codelist_mapping_source_authority_species.csv"
species_to_be_kept_by_rfmo_in_level0 <- readr::read_csv(url_mapping_asfis_rfmo) %>% dplyr::distinct()
georef_dataset <- georef_dataset %>% dplyr::inner_join(species_to_be_kept_by_rfmo_in_level0, 
  by = c("species" = "species", "source_authority" = "source_authority"))

if(recap_each_step){
  function_recap_each_step(
    "Filtering species",
    georef_dataset,
    paste0(
      "Filtering species on the base of the file ",
      url_mapping_asfis_rfmo,
      " to keep only the species under mandate of tRFMOs. This file contains " ,
      as.character(length(nrow(
        species_to_be_kept_by_rfmo_in_level0
      ))),
      " species."
    ),
    "inner_join"  ,
    NULL
  )
}
}

 

#we do an aggregation by dimensions
dataset <-
  georef_dataset %>% group_by(.dots = setdiff(colnames(georef_dataset), "measurement_value")) %>% dplyr::summarise(measurement_value =
                                                                                                                     sum(measurement_value))

config$logger.info("LEVEL %s => STEP aggregated")

dataset <- data.frame(dataset)

#@eblondel additional formatting for next time support
dataset$time_start <- as.Date(dataset$time_start)
dataset$time_end <- as.Date(dataset$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(
  paste0(format(min(dataset$time_start), "%Y"), "-01-01"),
  paste0(format(max(dataset$time_end), "%Y"), "-12-31"),
  sep = "/"
)
config$logger.info("temporal extent")

entity$setTemporalExtent(dataset_temporal_extent)

config$logger.info("seting temporal extend")

#@geoflow -> export as csv


config$logger.info(output_name_dataset_mapped)

write.csv(dataset, output_name_dataset_mapped, row.names = FALSE)

config$logger.info("writted")

entity$addResource("mapped", output_name_dataset_mapped)

config$logger.info("addingressource")
gc()

}
