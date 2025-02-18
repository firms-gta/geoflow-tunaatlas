#' Convert a catch dataset into CWP format
#'
#' This function transforms a dataset containing catch information into the CWP (Coordinating Working Party) format.
#'
#' @param df A dataframe containing catch data.
#' @return A transformed dataframe with columns formatted according to CWP standards.
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' @keywords IATTC, tuna, billfish, sharks, fisheries, data harmonization, longline catches and efforts
#' Input data sample (after importing as data.frame in R):
# A tibble: 6 × 26
# Year Month Flag  LatC5 LonC5 Hooks  BSHn  CCLn  FALn  MAKn  OCSn  RSKn  SKHn  SMAn  SPNn  THRn BSHmt CCLmt FALmt MAKmt
# <dbl> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1  1979     1 KOR   -22.5 -128. 62901     0     0     0     0     0     0    30     0     0     0     0     0     0     0
# 2  1979     1 KOR   -22.5 -122. 75482     0     0     0     0     0     0    33     0     0     0     0     0     0     0
# 3  1979     1 KOR   -17.5 -132. 41705     0     0     0     0     0     0     1     0     0     0     0     0     0     0
# 4  1979     1 KOR   -17.5 -128. 23322     0     0     0     0     0     0    23     0     0     0     0     0     0     0
# 5  1979     1 KOR   -17.5 -122. 42136     0     0     0     0     0     0    10     0     0     0     0     0     0     0
# 6  1979     1 KOR   -12.5 -148. 26128     0     0     0     0     0     0     3     0     0     0     0     0     0     0
# # ℹ 6 more variables: OCSmt <dbl>, RSKmt <dbl>, SKHmt <dbl>, SMAmt <dbl>, SPNmt <dbl>, THRmt <dbl>
# to # final data sample:
# # A tibble: 6 × 10
# source_authority species gear_type fishing_fleet fishing_mode time_start time_end   measurement_unit measurement_value geographic_identifier
# <chr>            <chr>   <chr>     <chr>         <chr>        <date>     <date>     <chr>                        <dbl> <chr>                
#   1 IATTC            BET     UNK       JPN           UNK          1954-10-01 1954-10-31 no                             163 6406138              
# 2 IATTC            YFT     UNK       JPN           UNK          1954-10-01 1954-10-31 no                              45 6406138              
# 3 IATTC            BIL     UNK       JPN           UNK          1954-10-01 1954-10-31 no                              37 6406138              
# 4 IATTC            BUM     UNK       JPN           UNK          1954-10-01 1954-10-31 no                              92 6406138              
# 5 IATTC            MLS     UNK       JPN           UNK          1954-10-01 1954-10-31 no                               2 6406138              
# 6 IATTC            SWO     UNK       JPN           UNK          1954-10-01 1954-10-31 no                               4 6406138
#' @export
function(action, entity, config){
  library(dplyr)
  library(tidyr)
  require(readr)
  filename1 <- entity$data$source[[1]] #data
  # Historical name for the dataset at source  PublicLLSharkMt.csv and PublicLLTunaBillfishMt.csv
  filename2 <- entity$data$source[[2]] #structure
  # Historical name for the dataset at source  iattc_catch_code_lists.csv
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  df <- readr::read_csv(path_to_raw_dataset)
  df <- df %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("(mt|n)$"), 
      names_to = "species_unit", values_to = "measurement_value"
    ) %>%
    dplyr::mutate(
      species = gsub("(mt|n)$", "", species_unit),
      measurement_unit = ifelse(grepl("mt$", species_unit), "t", "no"),
      time_start = as.Date(paste(Year, Month, "01", sep = "-")),
      time_end = as.Date(time_start) + lubridate::days(lubridate::days_in_month(time_start) - 1),
      fishing_fleet = Flag,
      fishing_mode = "UNK",
      source_authority = "IATTC",
      gear_type = "UNK", measurement = "catch", measurement_type = "RC" # Retained catches
    ) %>%
    dplyr::select(
      source_authority,  
      species,
      gear_type,  
      fishing_fleet,
      fishing_mode,
      time_start,
      time_end,
      LatC5, LonC5,
      measurement_unit,measurement_type,measurement,
      measurement_value
    )
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/pre-harmonization/cwp_grid_from_latlon.R")
  
  df$Square_size <- 5 # 5-degree squares
  df <- cwp_grid_from_latlon(df, colname_latitude = "LatC5", colname_longitude = "LonC5", colname_squaresize = "Square_size")
  df <- df %>% dplyr::select(-c(Square_size, LatC5, LonC5)) %>% dplyr::filter(measurement_value != 0)
  
  df$time_start <- as.Date(df$time_start)
  df$time_end <- as.Date(df$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(df$time_start), "%Y"), "-01-01"),
    paste0(format(max(df$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
  write.csv(df, output_name_dataset, row.names = FALSE)
  output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
  file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
  #----------------------------------------------------------------------------------------------------------------------------  
  entity$addResource("source", path_to_raw_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  
}
