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
  lines_reversed[which(str_detect(lines_reversed, "\\}$"))[1]] <- str_remove(lines_reversed[which(str_detect(lines_reversed, "\\}$"))[1]], "\\}$")
  # Restore the original order of lines
  lines <- rev(lines_reversed)
  
  # Remove spaces in front of the hash symbol for comments
  lines <- gsub("^\\s*#", "#", lines)
  # Remove lines containing specific keywords
  lines <- lines[!str_detect(lines, "entity|action|config|codelists|@geoflow|@eblondel|current")]
  
  # Replace 'output_name_dataset' assignments with a specific fixed string
  lines <- str_replace_all(lines, "output_name_dataset\\s*<-.*", 'output_name_dataset <- "Dataset_harmonized.csv"')
  
  # Update write.csv lines and add a line for georef_dataset assignment
  for (i in seq_along(lines)) {
    if (str_detect(lines[i], "write\\.csv")) {
      dataset_name <- str_extract(lines[i], "(?<=write\\.csv\\()(.*?)(?=,)")
      georef_line <- sprintf("georef_dataset <- %s", dataset_name)
      lines <- append(lines, georef_line, after = i)
    }
  }  
  
  
  if(grepl("effort",  script_path)){
    line_fact <- 'fact <- "effort"' 
  } else { line_fact <- 'fact <- "catch"'}
  
  lines_add <- ''
  
  if(sum(grepl("path_to_raw_dataset_catch",  as.character(lines)))>0){
    lines_add <- c(lines_add, 'path_to_raw_dataset_catch <- ') 
    lines_add <- c(lines_add, 'path_to_raw_dataset_effort <- ') 
    
  } else if(sum(grepl("path_to_raw_dataset",  lines))) {
    lines_add <- c(lines_add, 'path_to_raw_dataset <- ') 
  }
  
  # Add extra lines for data processing 
  
  intro_lines <- c(
    "# # Introduction",
    "# ",
    "# This R Markdown document is designed to transform data that is not in CWP format into CWP format.",
    "# Initially, it changes the format of the data; subsequently, it maps the data to adhere to CWP standards.",
    "# This markdown is created from a function so the documentation keep the format of roxygen2 skeleton",
    "# A summary of the mapping process is provided. Please specify `path_to_raw_dataset`, `path_to_raw_dataset_catch` or/and `path_to_raw_dataset_effort`, the historical name as received from tRFMOs is specified in the markdown",
    "# Additional operations are performed next to verify other aspects of the data, such as the consistency of the geolocation, the values, and the reported catches in numbers and tons.",
    "# If you are interested in further details, the results and codes are available for review."
  )
  
  extra_lines <- c(
    "# Load pre-harmonization scripts and apply mappings",
    "download.file('https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developpement/tunaatlas_scripts/pre-harmonization/map_codelists_no_DB.R', destfile = 'local_map_codelists_no_DB.R')",
    "source('local_map_codelists_no_DB.R')",
    line_fact,
    "mapping_codelist <- map_codelists_no_DB(fact, mapping_dataset = 'https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv', dataset_to_map = georef_dataset, mapping_keep_src_code = FALSE, summary_mapping = TRUE, source_authority_to_map = c('IATTC', 'CCSBT', 'WCPFC'))",
    "# Handle unmapped values and save the results",
    "georef_dataset <- mapping_codelist$dataset_mapped %>% dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == 'UNK', 'NEI', fishing_fleet), species = ifelse(species == 'UNK', 'MZZ', species), gear_type = ifelse(gear_type == 'UNK', '99.9', gear_type))",
    "fwrite(mapping_codelist$recap_mapping, 'recap_mapping.csv')",
    "fwrite(mapping_codelist$not_mapped_total, 'not_mapped_total.csv')",
    "# Display the first few rows of the mapping summaries",
    "print(head(mapping_codelist$recap_mapping))",
    "print(head(mapping_codelist$not_mapped_total))"
  )
  
  lines <- c(intro_lines,lines_add,  lines, extra_lines)
  
  
  
  return(lines)
}
