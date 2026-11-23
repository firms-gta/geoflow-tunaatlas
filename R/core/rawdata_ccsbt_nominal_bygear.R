#' Harmonize CCSBT Nominal Catch Dataset
#'
#' This function harmonizes the nominal catch dataset provided by the Commission for the Conservation of Southern Bluefin Tuna (CCSBT), preparing it for integration into the Tuna Atlas database.
#'
#' @details The function processes input datasets to match the standardized format required for integration into the Tuna Atlas,
#'          including adjustments to column names, units conversion, and data aggregation.
#'          Metadata integration is conditional, based on whether it will be loaded into the Tuna Atlas database.
#'
#' @seealso \code{\link{format_time_db_format}} for converting CCSBT Longline data structure.
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (ccsbt/nominal/bygear/action_ccsbt_nominal_bygear.R).
gta_rawdata_ccsbt_nominal_bygear <- function(data) {
  # Samples of the input and output data (from the former script)
  ## Catches
  # Input data sample (after importing as data.frame in R):
  # A tibble: 6 × 6
  # Calendar_Year Flag_Code Flag         Ocean    Gear      Catch_mt
  # <dbl> <chr>     <chr>        <chr>    <chr>        <dbl>
  #   1          1965 AU        Australia    Indian   Unspecif…   4675.
  # 2          1965 AU        Australia    Pacific  Unspecif…   2201.
  # 3          1965 JP        Japan        Atlantic Longline      15.3
  # 4          1965 JP        Japan        Indian   Longline   28095.
  # 5          1965 JP        Japan        Pacific  Longline   12579.
  # 6          1965 ZA        South Africa Indian   Longline       2
  # final data sample:
  # fishing_fleet gear_type time_start   time_end geographic_identifier fishing_mode species measurement_type
  # 1            JP  Longline 1965-01-01 1965-01-12              Atlantic          ALL     SBF              ALL
  # 2            JP  Longline 1968-01-01 1968-01-12              Atlantic          ALL     SBF              ALL
  # 3            JP  Longline 1969-01-01 1969-01-12              Atlantic          ALL     SBF              ALL
  # 4            JP  Longline 1970-01-01 1970-01-12              Atlantic          ALL     SBF              ALL
  # 5            JP  Longline 1971-01-01 1971-01-12              Atlantic          ALL     SBF              ALL
  # 6            JP  Longline 1972-01-01 1972-01-12              Atlantic          ALL     SBF              ALL
  # measurement_unit measurement_value source_authority
  # 1                t          15.33201            CCSBT
  # 2                t         411.48727            CCSBT
  # 3                t        1869.37842            CCSBT
  # 4                t        7574.64216            CCSBT
  # 5                t        2125.58909            CCSBT
  # 6                t        3928.10401            CCSBT
  source(here::here("./R/sardara_functions/format_time_db_format.R"))
  #packages
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------
  #config --> the global config of the workflow
  
  #----------------------------------------------------------------------------------------------------------------------------
  
  CCSBT_NC <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(readxl::read_excel(path_to_raw_dataset, sheet = "Sheet1")))
  
  CCSBT_NC <- CCSBT_NC %>% dplyr::select(Year = Calendar_Year, fishing_fleet = Flag_Code, 
                                         geographic_identifier = Ocean, gear_type = Gear, 
                                         measurement_value = Catch_mt)
  #Year and period
  CCSBT_NC$MonthStart<-1
  CCSBT_NC$Period<-12
  #Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
  CCSBT_NC<-format_time_db_format(CCSBT_NC)
  
  #School
  CCSBT_NC$fishing_mode<-"UNK"
  
  #Species
  CCSBT_NC$species<-"SBF"
  
  #CatchType
  CCSBT_NC$measurement_type<-"NC"
  
  #Geographic identifier
  CCSBT_NC <- CCSBT_NC  %>% dplyr::mutate(geographic_identifier = case_when(geographic_identifier == "Indian"~"IOTC", 
                                                                            geographic_identifier == "Pacific" ~ "WCPFC",
                                                                            geographic_identifier == "Atlantic" ~ "AT", 
                                                                            TRUE ~ geographic_identifier))
  
  #measurement_unit
  CCSBT_NC$measurement_unit<-"t"
  
  
  # remove 0 and NA values 
  CCSBT_NC <- CCSBT_NC[CCSBT_NC$measurement_value != 0,]
  CCSBT_NC <- CCSBT_NC[!is.na(CCSBT_NC$measurement_value),] 
  
  NC <- aggregate(CCSBT_NC$measurement_value,
                  FUN = sum,
                  by = list(
                    fishing_fleet = CCSBT_NC$fishing_fleet,
                    gear_type = CCSBT_NC$gear_type,
                    time_start = CCSBT_NC$time_start,
                    time_end = CCSBT_NC$time_end,
                    geographic_identifier = CCSBT_NC$geographic_identifier,
                    fishing_mode = CCSBT_NC$fishing_mode,
                    species = CCSBT_NC$species,
                    measurement_type = CCSBT_NC$measurement_type,
                    measurement_unit = CCSBT_NC$measurement_unit
                  )
  )
  
  colnames(NC)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
  
  NC$source_authority<-"CCSBT"
  NC$measurement <- "catch"
  NC$measurement_processing_level<-"raised"
  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
  NC$time_start <- as.Date(NC$time_start)
  NC$time_end <- as.Date(NC$time_end)
  NC$measurement_processing_level <- "unknown" 
  NC
}
