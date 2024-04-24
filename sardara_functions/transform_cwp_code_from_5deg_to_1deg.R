#' Generate 1-Degree Square CWP Codes within a 5-Degree Square
#'
#' This function takes a 5-degree square CWP code and generates the 25 corresponding 
#' 1-degree square CWP codes within it. Each code is adjusted based on the quadrant
#' and grid size of the input 5-degree square.
#'
#' @param input_5degree_square A numeric or character string representing the 
#'        7-digit CWP code for a 5-degree square. The code should start with '6'.
#' @param onlyinocean A logical representing wether yes or no the returned cwp squares need to be in the ocean only
#' @param cwp_grid_erased The cwp_grid_erased dataset that can be found in FAO website so it does not need to be load each time
#' @param con A connection to the Database containing the cwp_grid_erased if not loaded yet
#'
#' @return A numeric vector of 25 CWP codes, each representing a 1-degree square 
#'         within the specified 5-degree square.
#'
#' @examples
#' input_5degree_square <- 6430055
#' one_degree_squares <- generateOneDegreeSquares(input_5degree_square)
#' print(one_degree_squares)
#'
#' @export
transform_cwp_code_from_5deg_to_1deg <- function(input_5degree_square, onlyinocean = FALSE,cwp_grid_erased = NULL, con = NULL) {
  input_str <- as.character(input_5degree_square)
  
  # Ensure the input is a valid 7-digit CWP code for a 5-degree square
  if (nchar(input_str) != 7 || substr(input_str, 1, 1) != "6") {
    stop("Invalid CWP code. The code must be a 7-digit number representing a 5-degree square.")
  }
  
  quadrant <- substr(input_str, 2, 2)
  base_latitude <- as.numeric(substr(input_str, 3, 4))
  base_longitude <- as.numeric(substr(input_str, 5, 7))
  
  one_degree_squares <- vector("numeric", 25)
  index <- 1
  
  for (lat in base_latitude:(base_latitude + 4)) {
    for (lon in base_longitude:(base_longitude + 4)) {
      one_degree_squares[index] <- as.numeric(paste0("5", quadrant, sprintf("%02d", lat), sprintf("%03d", lon)))
      index <- index + 1
    }
  }
  if(onlyinocean){
    if(is.null(cwp_grid_erased)){
    cwp_grid_erased <- st_read(con,query = "SELECT * from area.cwp_grid")
    }
    one_degree_squares <- one_degree_squares[one_degree_squares %in% cwp_grid_erased$cwp_code]
  }
  
  
  
  return(one_degree_squares)
}

originalCode <- 6205170 # Example CWP code for a 1-degree square
newCode <- transform_cwp_code_from_5deg_to_1deg(originalCode, onlyinocean = TRUE, cwp_grid_erased = cwp_grid_erased,con = con)
print(newCode)
