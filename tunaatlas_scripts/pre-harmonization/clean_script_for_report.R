#' Clean and Prepare Script for Reporting
#'
#' This function takes a path to a script and performs a series of cleaning operations to prepare it for reporting.
#' The script is fetched remotely, cleaned of certain elements, and transformed to meet specific requirements.
#'
#' @param script_path The URL path to the script that needs to be cleaned.
#'
#' @details The cleaning process includes:
#' - Removing function definitions and their opening braces.
#' - Reversing the script lines to handle closing braces correctly.
#' - Stripping spaces before comment hashes and removing specific keywords.
#' - Adjusting variable assignments and updating lines that write CSV files.
#' - Adding lines dynamically based on the content of the script, such as setting specific dataset paths.
#' - Injecting an introduction and additional processing lines at the beginning and end of the script respectively.
#'
#' The function modifies the script by removing and replacing parts of the code that are not needed
#' for the final report and by adding necessary clarifications and data processing steps.
#'
#' @return A character vector containing the lines of the modified script.
#'
#' @examples
#' script_path <- "https://path.to.your/script.R"
#' cleaned_script <- clean_script(script_path)
#' writeLines(cleaned_script, "path/to/your/cleaned_script.R")
#'
#' @import httr
clean_script <- function(script_path) {
  require(httr)
  # uncomment thos lines to create the Global action for geoflow to be done
  response <- GET(script_path)
  content_lines <- readLines(textConnection(content(response, "text")))
  # Remove function definitions and the opening braces following them
  lines <- str_remove_all(content_lines, "^function.*\\{")
  
  # Reverse the array to handle the closing brace
  lines_reversed <- rev(lines)
  # Remove the first occurrence of the closing brace
  lines_reversed[which(str_detect(lines_reversed, "\\}"))[1]] <- str_remove(lines_reversed[which(str_detect(lines_reversed, "\\}"))[1]], "\\}")
  # Restore the original order of lines
  lines <- rev(lines_reversed)
  
  # Replace 'output_name_dataset' assignments with a specific fixed string
  lines <- str_replace_all(lines, "output_name_dataset\\s*<-.*", 'output_name_dataset <- "Dataset_harmonized.csv"')
  
  # Remove lines containing only dashes
  lines <- lines[!str_detect(lines, "^#\\-+$")]
  
  # Remove lines containing specific keywords
  lines <- lines[!str_detect(lines, "entity|action|config|codelists|@geoflow|@eblondel|current|code_lists")]
  
  # Update write.csv lines and add a line for georef_dataset assignment
  for (i in seq_along(lines)) {
    if (str_detect(lines[i], "write\\.csv")) {
      dataset_name <- str_extract(lines[i], "(?<=write\\.csv\\()(.*?)(?=,)")
      georef_line <- sprintf("georef_dataset <- %s", dataset_name)
      lines <- append(lines, georef_line, after = i)
    }
  }  
  
  
  if(grepl("effort",  script_path)){
    line_mapping = 'mapping_codelist <- map_codelists_no_DB(fact, mapping_dataset = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv", dataset_to_map = georef_dataset, mapping_keep_src_code = FALSE, summary_mapping = TRUE, source_authority_to_map = c("IATTC", "CCSBT", "WCPFC", "ICCAT", "IOTC"))'
    line_fact <- 'fact <- "effort"' 
  } else { 
    line_fact <- 'fact <- "catch"'
    line_mapping = 'mapping_codelist <- map_codelists_no_DB(fact, mapping_dataset = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv", dataset_to_map = georef_dataset, mapping_keep_src_code = FALSE, summary_mapping = TRUE, source_authority_to_map = c("IATTC", "CCSBT", "WCPFC"))'
    }
  
  lines_add <- ''
  
  if(sum(grepl("path_to_raw_dataset_catch",  as.character(lines)))>0){
    lines_add <- c(lines_add, 'path_to_raw_dataset_catch <- ') 
    lines_add <- c(lines_add, 'path_to_raw_dataset_effort <- ') 
    
  } else if(sum(grepl("path_to_raw_dataset1",  as.character(lines)))>0){
    lines_add <- c(lines_add, 'path_to_raw_dataset1 <- XLS_WCPFC.csv') 
    lines_add <- c(lines_add, 'path_to_raw_dataset2 <- XLS_WCPO.csv') 
    
  } else if(sum(grepl("path_to_raw_dataset",  lines))) {
    lines_add <- c(lines_add, 'path_to_raw_dataset <- ') 
  }
  
  # Add extra lines for data processing 
  
  intro_lines <- c(
    "# # Introduction",
    "# ",
    "# This R Markdown document is designed to transform data that is not in CWP format into CWP format.",
    "# Initially, it changes the format of the data; subsequently, it maps the data to adhere to CWP standards.",
    paste0("# This markdown is automatically created from the function:", script_path ,  ", the documentation keep the format of roxygen2 skeleton"),
    "# A summary of the mapping process is provided. The path to the dataset is specified, you will find on this same repository on github the first line of each dataset. The datasets are named after the historical name provided by tRFMOs while exporting and may change. The information provided in the Rmd allows to understand correctly which dataset should be used in this markdown.",
    "# Additional operations are performed next to verify other aspects of the data, such as the consistency of the geolocation, the values, and the reported catches in numbers and tons.",
    "# If you are interested in further details, the results and codes are available for review.", 
    "#*Each `.Rmd` script requires the user to knit the dataset at the beginning of the script in order to execute the harmonization process correctly. It is also possible to run the code chunk by chunk but be sure to be in the correct working directory i.e. the one of the .Rmd*"
  )
  
  setup_lines <- c(
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)",
    "```"
  )
  
  extra_lines <- c(
    "#' @ Load pre-harmonization scripts and apply mappings",
    
    "download.file('https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/pre-harmonization/map_codelists_no_DB.R', destfile = 'local_map_codelists_no_DB.R')",
    "source('local_map_codelists_no_DB.R')",
    line_fact,
    line_mapping,
    "#' @ Handle unmapped values and save the results",
    "georef_dataset <- mapping_codelist$dataset_mapped %>% dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == 'UNK', 'NEI', fishing_fleet), gear_type = ifelse(gear_type == 'UNK', '99.9', gear_type))",
    "fwrite(mapping_codelist$recap_mapping, 'recap_mapping.csv')",
    "fwrite(georef_dataset, 'CWP_dataset.csv')",
    "# Display the first few rows of the mapping summaries",
    "print(head(mapping_codelist$recap_mapping))"
  )
  
  lines <- c(intro_lines, setup_lines, lines_add, lines, extra_lines)
  
  
  
  return(lines)
}
