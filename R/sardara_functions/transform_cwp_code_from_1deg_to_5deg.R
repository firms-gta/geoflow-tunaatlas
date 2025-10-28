#' Transform Geographic Identifier
#'
#' Adjusts a CWP grid code from a 1-degree square to a 5-degree square, 
#' taking into account the quadrant and grid size.
#'
#' @param input_identifier A numeric value representing the input CWP grid code,
#'        which is a 7-digit number starting with '5'.
#' @return A numeric value representing the transformed CWP grid code, 
#'         which is a 7-digit number starting with '6'.
#' @examples
#' transformGeographicIdentifier(5431055)
#' @export
#'
#' @importFrom stats setNames

transform_cwp_code_from_1deg_to_5deg <- function(input_identifier) {
  adjustLongitude <- function(longitude, quadrant) {
    if (quadrant %in% c(1, 2)) {  # NE or SE
      return(longitude - longitude %% 5)
    } else {  # SW or NW
      return(longitude - longitude %% 5)
    }
  }
  adjustLatitude <- function(latitude, quadrant) {
    if (quadrant %in% c(1, 4)) {  # NE or NW
      return(latitude - latitude %% 5)
    } else {  # SE or SW
      return(latitude - latitude %% 5)
    }
  }
  
  input_str <- as.character(input_identifier)
  
  if (nchar(input_str) != 7 || substr(input_str, 1, 1) != "5") {
    print("Invalid CWP code. The code must be a 7-digit number representing a 1-degree square.")
    return(input_str)
    }
  
  quadrant <- as.numeric(substr(input_str, 2, 2))
  latitude <- as.numeric(substr(input_str, 3, 4))
  longitude <- as.numeric(substr(input_str, 5, 7))
  
  latitude_adj <- adjustLatitude(latitude, quadrant)
  longitude_adj <- adjustLongitude(longitude, quadrant)
  
  new_cwp_code <- as.numeric(paste0("6", quadrant, sprintf("%02d", latitude_adj), sprintf("%03d", longitude_adj)))
  
  new_cwp_code <- as.character(new_cwp_code)
  
  return(new_cwp_code)
}


