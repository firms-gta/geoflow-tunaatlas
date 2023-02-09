#setwd("D:/sandbox-geoflow/geoflow-tunaatlas/dcf")
library(googledrive)
library(readr)


#POC 1 to emulate a standardized dataset to be submit to dcf-shiny
#based on an extracted 
gta_global_nc <- "global_nominal_catch_firms_level0.csv"
gdr = googledrive::drive_get(gta_global_nc)
googledrive::drive_download(file = gdr, path = gta_global_nc)

gta_global_nc_data <- readr::read_csv("global_nominal_catch_firms_level0.csv")
iotc <- gta_global_nc_data[gta_global_nc_data$source_authority == "IOTC",]
iotc$source_authority <- NULL

cwp_rh_valid_data <- data.frame(
	#who
	fishingfleet = iotc$fishingfleet,
	#when
	time = paste(iotc$time_start, iotc$time_end, sep = "/"),
	time_start = iotc$time_start,
	time_end = iotc$time_end,
	#where
	geographic_identifier = substr(iotc$geographic_identifier, 2,3), #to align on standard codes
	#how
	gear_type = iotc$gear,
	#what
	species = iotc$species,
	#measurement
	measurement = "catch",
	measurement_type = "nominal",
	mesurement_value = iotc$value,
	measurement_unit = "t",
	measurement_obs = NA,
	stringsAsFactors = FALSE
)
