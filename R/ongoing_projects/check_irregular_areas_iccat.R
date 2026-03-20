
library(readr)
removed_irregular_areas <- read_csv("jobs/20260224084808_raw_data_georef_effort/entities/effort_iccat_level0__byschool/data/removed_irregular_areas.csv")
RFMO_CE <- readxl::read_excel("jobs/20260224084808_raw_data_georef_effort/entities/effort_iccat_level0__byschool/data/t2ce_ETRO-PS1991-2024_bySchool_v1.xlsx", 
                                                sheet = "Data")


RFMO_CE$FleetCode_short <- sub("-.*", "", RFMO_CE$FleetCode)

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

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/FUN_efforts_ICCAT_CE_keep_all_efforts.R")
efforts_pivot_ICCAT<-FUN_efforts_ICCAT_CE_keep_all_efforts(RFMO_CE,c("Eff1","Eff2","Eff3","Eff4","Eff5"),c("Eff1Type","Eff2Type","Eff3Type","Eff4Type","Eff5Type"))

# School
# The format changed, the school is now in the FishMode column

efforts_pivot_ICCAT <- efforts_pivot_ICCAT %>% dplyr::mutate(FishMode = ifelse(FishMode == "n/a", "UNK", FishMode)) 
efforts_pivot_ICCAT<- efforts_pivot_ICCAT %>% dplyr::rename("School" = "FishMode")
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/ICCAT_CE_effort_pivotDSD_to_harmonizedDSD.R")
efforts_pivot_ICCAT <- efforts_pivot_ICCAT %>% dplyr::rename(SquareTypeCode = GeoStrataCode) # to match defi
efforts<-ICCAT_CE_effort_pivotDSD_to_harmonizedDSD(efforts_pivot_ICCAT,colToKeep_efforts)
efforts$AreaName <- as.character(as.integer(efforts$AreaName))
colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"ICCAT"

require(dplyr)

irregular_area <- left_join(removed_irregular_areas,t2ce_ETRO_PS1991_2024_bySchool_v1)



