#' Extract URLs Following '@' Symbol
#'
#' Scans a specified column in a dublin core format (for geoflow) dataset, for strings that contain URLs following
#' an '@' symbol, extracts these URLs, and returns them as a character vector.
#' Note: This function assumes the column is of type character.
#'
#' @param data A dataframe containing the data to be scanned.
#'
#' @return A character vector containing the extracted URLs.
#' @importFrom stringr str_detect str_remove str_remove_all
#'
#' @examples
#' # Assuming `df` is your dataframe and `text_column` is the column with URLs
#' extracted_urls <- extract_urls_from_column(df, "text_column")
#' print(extracted_urls)
extract_urls_from_column <- function(data, column_name = "Data") {
  # Ensure the column exists in the dataframe
  if (!column_name %in% names(data)) {
    stop("Column not found in the dataframe")
  }
  
  # Convert column to character to ensure string operations work as expected
  column_data <- as.character(data[[column_name]])
  
  # Initialize an empty vector to store extracted URLs
  extracted_urls <- character()
  
  # Regex pattern to match URLs following '@'
  pattern <- "@https?://[\\w\\-._~:/?#\\[\\]@!$&'()*+,;=]+"
  
  # Extract URLs
  for (text in column_data) {
    urls <- str_extract_all(text, pattern)
    urls <- urls[[1]] # Extract URLs from list structure
    if (length(urls) > 0) {
      urls <- sub("@", "", urls, fixed = TRUE) # Remove '@' from the URLs
      extracted_urls <- c(extracted_urls, urls) # Add to the result vector
    }
  }
  
  return(extracted_urls)
}
