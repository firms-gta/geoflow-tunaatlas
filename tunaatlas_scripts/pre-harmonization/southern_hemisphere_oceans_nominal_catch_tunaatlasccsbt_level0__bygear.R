######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = CCSBT_Global_Catch.xlsx, title = Harmonize data structure of CCSBT nominal catch, abstract = Harmonize the structure of CCSBT nominal catch dataset (pid of output file = southern_hemisphere_oceans_nominal_catch_tunaatlasccsbt_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/033iit, value = "https://goo.gl/033iit";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv. , value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 
# This function is to harmonize dataset provided by the CCSBT prior to integration in the Global Tuna Atlas workflow. 
# The output dataset follows the CWP Standard for Reference Harmonization.


#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#' 
#' @keywords Commission for the Conservation of Southern Bluefin Tuna CCSBT tuna RFMO Sardara Global database on tuna fishieries
#'

##Catches
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

function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
  #packages
  
  
  if(!require(reshape)){
    install.packages("reshape")
    require(reshape)
  }
  if(!require(readxl)){
    install.packages("readxl")
    require(readxl)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@geoflow --> with this script 2 objects are pre-loaded
  #config --> the global config of the workflow
  #entity --> the entity you are managing
  #get data from geoflow current job dir
  filename1 <- entity$data$source[[1]] #data
  filename2 <- entity$data$source[[2]] #structure
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  #----------------------------------------------------------------------------------------------------------------------------
  
  CCSBT_NC <- readxl::read_excel(path_to_raw_dataset, sheet = "Sheet1")
  
  CCSBT_NC <- CCSBT_NC %>% dplyr::select(Year = Calendar_Year, fishing_fleet = Flag_Code, 
                                  geographic_identifier = Ocean, gear_type = Gear, 
                                  measurement_value = Catch_mt)
  #Year and period
  CCSBT_NC$MonthStart<-1
  CCSBT_NC$Period<-12
  #Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
  CCSBT_NC<-format_time_db_format(CCSBT_NC)
  
  #School
  CCSBT_NC$fishing_mode<-"ALL"
  
  #Species
  CCSBT_NC$species<-"SBF"
  
  #CatchType
  CCSBT_NC$measurement_type<-"ALL"
  
  #Geographic identifier
  CCSBT_NC <- CCSBT_NC  %>% dplyr::mutate(geographic_identifier = case_when(geographic_identifier == "Indian"~"IOTC", 
                                        geographic_identifier == "Pacific" ~ "WCPO", TRUE ~ geographic_identifier))
  
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
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
  NC$time_start <- as.Date(NC$time_start)
  NC$time_end <- as.Date(NC$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(NC$time_start), "%Y"), "-01-01"),
    paste0(format(max(NC$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #@geoflow -> export as csv
  output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".xlsx"))[1], "_harmonized.csv"), path_to_raw_dataset)
  write.csv(NC, output_name_dataset, row.names = FALSE)
  output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".xlsx"))[1], "_codelists.csv"), path_to_raw_dataset)
  file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
  #----------------------------------------------------------------------------------------------------------------------------  
  entity$addResource("source", path_to_raw_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  
}