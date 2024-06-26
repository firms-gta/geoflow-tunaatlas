# Load necessary libraries
library(httr)
library(jsonlite)
library(readr)
library(dplyr)

#' Download Specific CSV File from Zenodo using DOI and MD5 Checksum
#'
#' This function downloads a specific CSV file from a Zenodo record given its DOI and MD5 checksum.
#'
#' @param doi A character string representing the Zenodo record DOI.
#' @param md5_checksum A character string representing the MD5 checksum of the specific CSV file.
#'
#' @return A data frame containing the data from the downloaded CSV file.
#' @examples
#' \dontrun{
#' doi <- "10.5281/zenodo.11460074"
#' key <- "global_catch_firms_level0_public"
#' data <- download_zenodo_csv(doi, md5_checksum)
#' }
#' @export
download_zenodo_csv <- function(doi, key) {
  # Convert DOI to Zenodo API URL
  zenodo_url <- paste0("https://zenodo.org/api/records/", sub("10.5281/zenodo.", "", doi))
  
  # Get the metadata from Zenodo
  response <- httr::GET(zenodo_url)
  if (response$status_code == 200) {
    # Parse the metadata
    metadata <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    
    # Extract the list of files
    files <- metadata$files
    
    # Find the specific CSV file using md5 checksum
    target_file <- files %>% dplyr::filter(key == key)
    
    if (nrow(target_file) > 0) {
      # Get the correct download link
      download_url <- target_file$links$self
      
      # Download the specific CSV file
      temp_file <- tempfile(fileext = ".csv")
      download.file(download_url, temp_file)
      
      # Read the CSV file into R
      data <- read_csv(temp_file)
      return(data)
    } else {
      stop("The specified CSV file with the given md5 checksum was not found.")
    }
  } else {
    stop("Failed to retrieve metadata. Status code: ", response$status_code)
  }
}
