#' Convert Latitude and Longitude to CWP Grid Code
#'
#' This function converts latitude and longitude coordinates into a 7-digit CWP grid code
#' following FAO's Coordinated Working Party (CWP) Areal Grid System.
#'
#' @param df_input A data frame containing latitude, longitude, and grid size information.
#' @param colname_latitude The name of the column containing latitude values.
#' @param colname_longitude The name of the column containing longitude values.
#' @param colname_squaresize The name of the column containing the grid size (e.g., 1, 5, 10).
#'
#' @return A data frame with an additional column `geographic_identifier` containing the 7-digit CWP grid code.
#' @export
#'
#' @examples
#' df_test <- data.frame(latitude = c(12.4, -8.6, -5.2, 47.3), 
#'                       longitude = c(34.7, -75.2, 12.1, -122.5), 
#'                       squaresize = c(1, 1, 5, 10))
#' df_result <- cwp_grid_from_latlon(df_test, "latitude", "longitude", "squaresize")
#' print(df_result)
#' Convert Latitude and Longitude to CWP Grid Code
#'
#' This function converts latitude and longitude coordinates into a 7-digit CWP grid code
#' following FAO's Coordinated Working Party (CWP) Areal Grid System.
#'
#' @param df_input A data frame containing latitude, longitude, and grid size information.
#' @param colname_latitude The name of the column containing latitude values.
#' @param colname_longitude The name of the column containing longitude values.
#' @param colname_squaresize The name of the column containing the grid size (e.g., 1, 5, 10).
#'
#' @return A data frame with an additional column `geographic_identifier` containing the 7-digit CWP grid code.
#' @export
#'
#' @examples
#' df_test <- data.frame(latitude = c(12.4, -8.6, -5.2, 47.3), 
#'                       longitude = c(34.7, -75.2, 12.1, -122.5), 
#'                       squaresize = c(1, 1, 5, 10))
#' df_result <- cwp_grid_from_latlon(df_test, "latitude", "longitude", "squaresize")
#' print(df_result)
cwp_grid_from_latlon <- function(df_input, colname_latitude, colname_longitude, colname_squaresize) {
  library(dplyr)
  
  # Determine the quadrant based on latitude and longitude
  df_input <- df_input %>%
    dplyr::mutate(
      quadrant = case_when(
        !!sym(colname_latitude) >= 0 & !!sym(colname_longitude) >= 0 ~ 1,  # NE (Northeast)
        !!sym(colname_latitude) < 0 & !!sym(colname_longitude) >= 0 ~ 2,  # SE (Southeast)
        !!sym(colname_latitude) < 0 & !!sym(colname_longitude) < 0 ~ 3,  # SW (Southwest)
        !!sym(colname_latitude) >= 0 & !!sym(colname_longitude) < 0 ~ 4   # NW (Northwest)
      )
    )
  
  # Assign grid size code based on FAO standards
  df_input <- df_input %>%
    dplyr::mutate(
      grid_code = case_when(
        !!sym(colname_squaresize) == 1 ~ 5,   # 1° x 1°
        !!sym(colname_squaresize) == 5 ~ 6,   # 5° x 5°
        !!sym(colname_squaresize) == 10 ~ 7,  # 10° x 10°
        !!sym(colname_squaresize) == 20 ~ 8,  # 20° x 20°
        !!sym(colname_squaresize) == 30 ~ 9,  # 30° x 30°
        TRUE ~ 9  # Default to 9 if grid size is unknown
      )
    )
  
  # Adjust latitude and longitude based on quadrant for correct CWP grid reference
  df_input <- df_input %>%
    dplyr::mutate(
      adjusted_lat = case_when(
        quadrant %in% c(1, 4) ~ floor(!!sym(colname_latitude) / !!sym(colname_squaresize)) * !!sym(colname_squaresize),  # Lower for NE & NW
        quadrant %in% c(2, 3) ~ ceiling(!!sym(colname_latitude) / !!sym(colname_squaresize)) * !!sym(colname_squaresize)  # Upper for SE & SW
      ),
      adjusted_lon = case_when(
        quadrant %in% c(1, 2) ~ floor(!!sym(colname_longitude) / !!sym(colname_squaresize)) * !!sym(colname_squaresize),  # Leftmost for NE & SE
        quadrant %in% c(3, 4) ~ ceiling(!!sym(colname_longitude) / !!sym(colname_squaresize)) * !!sym(colname_squaresize)  # Rightmost for SW & NW
      )
    )
  
  # Convert latitude and longitude to absolute values for formatting
  lat_abs <- abs(df_input$adjusted_lat)
  lon_abs <- abs(df_input$adjusted_lon)
  
  # Construct the 7-digit geographic identifier in the CWP format
  df_input <- df_input %>%
    dplyr::mutate(
      geographic_identifier = sprintf("%d%d%02d%03d", 
                                      grid_code, 
                                      quadrant, 
                                      floor(lat_abs), 
                                      floor(lon_abs))
    ) %>%
    dplyr::select(-c(quadrant, adjusted_lon, adjusted_lat, grid_code))  # Remove intermediate columns
  
  return(df_input)
}
