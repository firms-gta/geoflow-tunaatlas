#' Harmonize data structure of ICCAT by operation mode effort datasets
#'
#' This function harmonizes the structure of ICCAT catch-and-effort datasets
#' based on operation modes. It adapts the dataset structure for compatibility with
#' the Tuna Atlas database, considering only specific fields as mandatory. The
#' function also handles optional metadata integration if provided.
#'
#' @seealso \code{FUN_efforts_ICCAT_CE_keep_all_efforts()} for converting ICCAT task 2,
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (iccat/effort/ps_byschool/action_iccat_effort_ps_byschool.R).
#' @export
gta_rawdata_iccat_effort_ps_byschool <- function(data) {
keep_fleet_instead_of_flag=FALSE
RFMO_CE <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(read_excel(path_to_raw_dataset, sheet = "Data")))

RFMO_CE$FleetCode_short <- sub("-.*", "", RFMO_CE$FleetCode) # fleet code only what is after the '-'

names(RFMO_CE)[names(RFMO_CE) == 'FleetCode_short'] <- 'FishingFleet'
RFMO_CE <- RFMO_CE[, c("FishingFleet", setdiff(names(RFMO_CE), "FishingFleet"))] # oput flag in first position


# ## If we want in the output dataset the column 'FleetCode' instead of 'flag'
# 
# if(keep_fleet_instead_of_flag==TRUE){
#   RFMO_CE$Flag<-NULL
#   names(RFMO_CE)[names(RFMO_CE) == 'Fishingfleet'] <- 'Flag'
# }

##Efforts

last_column_not_catch_value=27
RFMO_CE<-RFMO_CE[,-(last_column_not_catch_value:ncol(RFMO_CE))] 

efforts_pivot_ICCAT<-FUN_efforts_ICCAT_CE_keep_all_efforts(RFMO_CE,c("Eff1","Eff2","Eff3","Eff4","Eff5"),c("Eff1Type","Eff2Type","Eff3Type","Eff4Type","Eff5Type"))

# School
# The format changed, the school is now in the FishMode column

efforts_pivot_ICCAT <- efforts_pivot_ICCAT %>% dplyr::mutate(FishMode = ifelse(FishMode == "n/a", "OTH", FishMode)) 
efforts_pivot_ICCAT<- efforts_pivot_ICCAT %>% dplyr::rename("School" = "FishMode")
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
efforts_pivot_ICCAT <- efforts_pivot_ICCAT %>% dplyr::rename(SquareTypeCode = GeoStrataCode) # to match definition in ICCAT_CE_effort_pivotDSD_to_harmonizedDSD
efforts_pivot_ICCAT$Lat <- floor(abs(efforts_pivot_ICCAT$Lat)) # we put floor as independently of the quadrant the floor always correspond to the cwp
efforts_pivot_ICCAT$Lon <- floor(abs(efforts_pivot_ICCAT$Lon))
efforts<-ICCAT_CE_effort_pivotDSD_to_harmonizedDSD(efforts_pivot_ICCAT,colToKeep_efforts)
efforts$AreaName <- as.character(as.integer(efforts$AreaName))
colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"ICCAT"
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
efforts$geographic_identifier <- format(as.integer(efforts$geographic_identifier), scientific = FALSE, trim = TRUE)
  efforts
}
