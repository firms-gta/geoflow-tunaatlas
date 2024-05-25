#' Harmonize IATTC PSSetType Effort Datasets
#'
#' This function harmonizes the IATTC PSSetType effort datasets,
#' preparing them for integration into the Tuna Atlas database, according to specified format requirements.
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details This function modifies the dataset to ensure compliance with the standardized
#'          format, including renaming, reordering, and recalculating specific fields as necessary.
#'          Metadata integration is contingent on the intended use within the Tuna Atlas database.
#'
#' @import dplyr
#' @import readr
#' @importFrom stringr str_replace
#' @seealso \code{\link{IATTC_CE_efforts_pivotDSD_to_harmonizedDSD}} 
#' @export
#' @keywords data harmonization, fisheries, IATTC, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}

# Catch input data sample:
# Record Spp DTypeID Number Weight
#  11407 ALB       2     17     NA
#  11407 BUM       2      4     NA
#  11407 BLM       2      2     NA
#  11407 SWO       2     10     NA
#  11407 BET       2    403     NA
#  11407 BIL       2      1     NA


# Catch: final data sample:
# FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
#  USA   LL 1992-07-01 1992-08-01  6425135    ALL     BSH       ALL         NO     4
#  USA   LL 1993-04-01 1993-05-01  6425135    ALL     BSH       ALL         NO    75
#  USA   LL 1993-04-01 1993-05-01  6430135    ALL     BSH       ALL         NO    15
#  USA   LL 1993-05-01 1993-06-01  6425135    ALL     BSH       ALL         NO    24
#  USA   LL 1994-03-01 1994-04-01  6425135    ALL     BSH       ALL         NO    14
#  USA   LL 1994-03-01 1994-04-01  6430135    ALL     BSH       ALL         NO     4
function(action, entity, config){
  
  #packages
  
    
  if(!require(reshape)){
    install.packages("reshape")
    require(reshape)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  if(!require(tidyr)){
    install.packages("tidyr")
    require(tidyr)
  }

  
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@geoflow --> with this script 2 objects are pre-loaded
  #config --> the global config of the workflow
  #entity --> the entity you are managing
  #get data from geoflow current job dir
  filename_catch <- entity$data$source[[1]] #catch data
# Historical name for the dataset at source  PublicCatchOrigFormatTunaBillfish.csv or PublicCatchOrigFormatShark.csv
# Historical name for the dataset at source  
  filename_effort <- entity$data$source[[2]] #effort data
# Historical name for the dataset at source  PublicEffortOrigFormatTunaBillfish.csv or PublicEffortOrigFormatShark.csv (keep the same order as for catch)
  filename_str <- entity$data$source[[3]] #structure
# Historical name for the dataset at source  iattc_effort_code_lists.csv
  path_to_raw_dataset_catch <- entity$getJobDataResource(config, filename_catch)
  path_to_raw_dataset_effort <- entity$getJobDataResource(config, filename_effort)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  #----------------------------------------------------------------------------------------------------------------------------
  
  ## Catches
  catches<-read.csv(path_to_raw_dataset_catch, stringsAsFactors = F)
  efforts<-read.csv(path_to_raw_dataset_effort, stringsAsFactors = F)
  # catches <- melt(catches, id.vars=c("Record","Spp","DTypeID"))  #@juldebar error with melt function from reshape package
  # catches <-melt(as.data.table(catches),id.vars=c("Record","Spp","DTypeID"))
  efforts <- efforts %>% tidyr::gather(variable, value, -c("Record","Lat", "Lon", "Year", "Month", "FlagAbv"))
  # remove values=0
  
  efforts <- efforts  %>% 
    dplyr::filter( ! value %in% 0 ) %>%
    dplyr::filter( ! is.na(value)) 
  colnames(efforts)[colnames(efforts)=="FlagAbv"] <- "FishingFleet"
  colnames(efforts)[colnames(efforts)=="variable"] <- "EffortUnits"
  colnames(efforts)[colnames(efforts)=="value"] <- "Effort"
  
  # Set catchunit values
  # DType code 1 means that the data was submitted in both weight and number for the same catch
  # DType code 2 means that the data was submitted as number only
  # DType code 3 means that the data was submitted as weight only
  
  #Initialisation
  
  efforts$SquareSize<-5
  efforts$CodeSquareSize<-6
  efforts$Gear<-"LL"
  efforts$SetType<-"ALL"
  efforts <- efforts %>% select(-Record)
  
  # Reach the catches harmonized DSD using a function in IATTC_functions.R
  colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName", "School","EffortUnits","Effort")
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/IATTC_CE_efforts_pivotDSD_to_harmonizedDSD.R")
  efforts<-IATTC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts,colToKeep_efforts)
  
  colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
  efforts$source_authority<-"IATTC"
  efforts$measurement <- "effort"
  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
  efforts$time_start <- as.Date(efforts$time_start)
  efforts$time_end <- as.Date(efforts$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(efforts$time_start), "%Y"), "-01-01"),
    paste0(format(max(efforts$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #@geoflow -> export as csv
  output_name_dataset <- gsub(filename_catch, paste0(unlist(strsplit(filename_catch,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset_catch)
  write.csv(efforts, output_name_dataset, row.names = FALSE)
  output_name_codelists <- gsub(filename_catch, paste0(unlist(strsplit(filename_catch,".csv"))[1], "_codelists.csv"), path_to_raw_dataset_catch)
  file.rename(from = entity$getJobDataResource(config, filename_str), to = output_name_codelists)
  #----------------------------------------------------------------------------------------------------------------------------  
  entity$addResource("source", path_to_raw_dataset_catch)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  
}
