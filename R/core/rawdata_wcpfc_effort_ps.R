#' Harmonize WCPFC Purse Seine Effort Datasets
#'
#' This function harmonizes the WCPFC Purse Seine effort datasets,
#' preparing them for integration into the Tuna Atlas database according to specified format requirements.
#'
#' @details This function modifies the dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @seealso \code{\link{WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD}} for converting WCPFC Drifnet data structure,
#'          \code{\link{convertDSD_wcpfc_nc}} for nominal catch data structure.
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (wcpfc/effort/ps/action_wcpfc_effort_ps.R).
gta_rawdata_wcpfc_effort_ps <- function(data) {
  
  #packages
  
    
      
  
  
# Input data sample:
# YY MM LAT5 LON5 DAYS SETS_UNA SETS_LOG SETS_DFAD SETS_AFAD SETS_OTH SKJ_C_UNA YFT_C_UNA BET_C_UNA OTH_C_UNA SKJ_C_LOG YFT_C_LOG BET_C_LOG OTH_C_LOG SKJ_C_DFAD
# 1967  2  30N 135E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  2  30N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  2  35N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  2  40N 140E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  2  40N 145E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# 1967  3  30N 135E    0        0        0         0         0        0         0         0         0         0         0         0         0         0          0
# YFT_C_DFAD BET_C_DFAD OTH_C_DFAD SKJ_C_AFAD YFT_C_AFAD BET_C_AFAD OTH_C_AFAD SKJ_C_OTH YFT_C_OTH BET_C_OTH OTH_C_OTH
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0
#          0          0          0          0          0          0          0         0         0         0         0


# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  ALL    S 1970-01-01 1970-02-01  6100135    ALL        DAYS     53
#  ALL    S 1970-01-01 1970-02-01  6100135    LOG        SETS     55
#  ALL    S 1970-02-01 1970-03-01  6100125    ALL        DAYS     42
#  ALL    S 1970-02-01 1970-03-01  6100125    LOG        SETS     42
#  ALL    S 1970-02-01 1970-03-01  6100135    ALL        DAYS     57
#  ALL    S 1970-02-01 1970-03-01  6100135    LOG        SETS     50


#----------------------------------------------------------------------------------------------------------------------------
#config --> the global config of the workflow
# Historical name for the dataset at source  WCPFC_S_PUBLIC_BY_YR_MON.csv
# Historical name for the dataset at source  wcpfc_effort_code_lists.csv

#----------------------------------------------------------------------------------------------------------------------------


##Efforts

DF <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(read.csv(path_to_raw_dataset)))
colnames(DF) <- toupper(colnames(DF))
DF <- as.data.frame(DF)
source(here::here("./R/sardara_functions/harmo_spatial_3.R"))
DF <- harmo_spatial_3(DF, 
                                       "LAT5", "LON5", 5, 6)  %>% dplyr::select(-CWP_GRID)

efforts <- DF %>%
  dplyr::mutate(
    time_start = as.Date(paste(YY, MM, "01", sep = "-")),
    time_end = lubridate::ceiling_date(time_start, unit = "month") - 1
  ) %>%
  tidyr::pivot_longer(
    cols = c(DAYS, SETS_UNA, SETS_LOG, SETS_DFAD, SETS_AFAD, SETS_OTH),
    names_to = "EffortUnits",
    values_to = "Effort"
  ) %>%
  dplyr::mutate(
    School = dplyr::case_when(
      grepl("UNA", EffortUnits) ~ "UNA",
      grepl("LOG", EffortUnits) ~ "LOG",
      grepl("DFAD", EffortUnits) ~ "DFAD",
      grepl("AFAD", EffortUnits) ~ "AFAD",
      grepl("OTH", EffortUnits) ~ "OTH",
      EffortUnits == "DAYS" ~ "OTH"
    ),
    EffortUnits = dplyr::if_else(EffortUnits == "DAYS", "DAYS", "SETS"),
    Flag = "ALL",
    Gear = "S"
  ) %>%
  dplyr::select(Flag, Gear, time_start, time_end, AreaName, School, EffortUnits, Effort) %>%
  dplyr::filter(Effort > 0)



#-----------------------------------------------------------
colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"WCPFC"
efforts$measurement <- "effort" 
efforts$measurement_processing_level <- "unknown" 
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
  efforts
}
