#' Harmonize WCPFC Longline Effort Datasets
#'
#' This function harmonizes WCPFC Longline effort datasets, preparing them
#' for integration into the Tuna Atlas database according to specified format requirements.
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
#' @import tidyr
#' @import readr
#' @importFrom stringr str_replace
#' @seealso \code{\link{harmo_time_2}} for converting time data structure,
#'          \code{\link{harmo_spatial_3}} for converting lat lon to cwp grid format,
#'          \code{\link{WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD}} for Pole-and-line data structure,
#' @export
#' @keywords data harmonization, fisheries, WCPFC, longline, tuna
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}


# wd<-getwd()
# download.file(path_to_raw_dataset,destfile=paste(wd,"/dbf_file.DBF",sep=""), method='auto', quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
# path_to_raw_dataset=paste(wd,"/dbf_file.DBF",sep="")



# Input data sample:
# YY MM LAT5 LON5   HHOOKS ALB_C ALB_N   YFT_C YFT_N   BET_C BET_N MLS_C MLS_N  BLM_C BLM_N  BUM_C BUM_N  SWO_C SWO_N OTH_C OTH_N
# 2000  1  00N 120E 12391.11 0.000     0 267.338 10056  58.850  1537 0.627    15 11.391   249 18.203   314  9.998   189 0.120     4
# 2000  1  00N 125E 16349.59 0.000     0 352.417 13256  77.975  2036 0.827    19 15.030   329 24.018   414 13.192   249 0.158     5
# 2000  1  00N 130E  7091.08 0.000     0 130.454  4630  37.695   903 0.200     5  3.870    83  6.418   109  4.714    93 0.038     1
# 2000  1  00N 135E  6113.85 1.276    73  75.469  2431 115.868  2575 0.037     1  0.058     1  6.948    90  2.719    38 0.245     4
# 2000  1  00N 140E  9904.92 1.350    77 176.963  6266 251.303  6084 0.462    11  1.527    38 12.150   187  4.200    52 0.296     9
# 2000  1  00N 145E  8679.03 0.428    24 122.945  4613 144.910  3579 0.537    12 11.062   237  8.748   137  6.326   110 0.000     0

# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits  Effort
#  ALL    L 2000-01-01 2000-02-01  6100120    ALL      HHOOKS 1239111
#  ALL    L 2000-01-01 2000-02-01  6100125    ALL      HHOOKS 1634959
#  ALL    L 2000-01-01 2000-02-01  6100130    ALL      HHOOKS  709108
#  ALL    L 2000-01-01 2000-02-01  6100135    ALL      HHOOKS  611385
#  ALL    L 2000-01-01 2000-02-01  6100140    ALL      HHOOKS  990492
#  ALL    L 2000-01-01 2000-02-01  6100145    ALL      HHOOKS  867903

function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_3.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD.R")
  #packages
  
    
  
  if(!require(readr)){
    install.packages("readr")
    require(readr)
  }
  
  if(!require(tidyr)){
    install.packages("tidyr")
    require(tidyr)
  }
  
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  
  
  if(!require(reshape)){
    install.packages("reshape")
    require(reshape)
  }
  
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@geoflow --> with this script 2 objects are pre-loaded
  #config --> the global config of the workflow
  #entity --> the entity you are managing
  filename1 <- entity$data$source[[1]] #data
  filename2 <- entity$data$source[[2]] #structure
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  #----------------------------------------------------------------------------------------------------------------------------
  
  
  DF <- read.table(path_to_raw_dataset, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)
  
  DF$cwp_grid=NULL 
  colnames(DF)<-toupper(colnames(DF))
  if(any(DF$FLAG_ID == "")) DF[DF$FLAG_ID == "",]$FLAG_ID <- "UNK"
  DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:6])))
  
  DF<- DF %>% 
    dplyr::filter( ! value %in% 0 ) %>%
    dplyr::filter( ! is.na(value)) 
  DF$variable<-as.character(DF$variable)
  colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
  
  DF$EffortUnits<-substr(DF$Species, nchar(DF$Species), nchar(DF$Species))
  
  DF$Species<-sub('_C', '', DF$Species)
  DF$Species<-sub('_N', '', DF$Species)
  
  DF$School<-"OTH"
  
  DF$EffortUnits<-colnames(DF[6])    
  colnames(DF)[6]<-"Effort"
  
  
  efforts_pivot_WCPFC=DF
  efforts_pivot_WCPFC$Gear<-"L"
  
  # School
efforts_pivot_WCPFC$School<-"UNK"
efforts_pivot_WCPFC$Gear<-"L"

colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")

efforts_pivot_WCPFC$RFMO <- "WCPFC"
efforts_pivot_WCPFC$Ocean <- "PAC_W"
efforts_pivot_WCPFC$FishingFleet <- efforts_pivot_WCPFC$FLAG_ID 
efforts_pivot_WCPFC <- harmo_time_2(efforts_pivot_WCPFC, 
                                                "YY", "MM")
efforts_pivot_WCPFC <- harmo_spatial_3(efforts_pivot_WCPFC, 
                                                   "LAT_SHORT", "LON_SHORT", 5, 6) 
efforts_pivot_WCPFC$CatchType <- "ALL"
efforts_pivot_WCPFC$Effort <- efforts_pivot_WCPFC$value
efforts <- efforts_pivot_WCPFC[colToKeep_efforts]
rm(efforts_pivot_WCPFC)
efforts[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(efforts[, 
                                                                        c("AreaName", "FishingFleet")], 2, function(x) {
                                                                          gsub(" *$", "", x)
                                                                        }), stringsAsFactors = FALSE)
efforts <- efforts %>% filter(!Effort %in% 0) %>% filter(!is.na(Effort))
efforts <- as.data.frame(efforts)
efforts <- aggregate(efforts$Effort,
                     by = list(
                       FishingFleet = efforts$FishingFleet,
                       Gear = efforts$Gear,
                       time_start = efforts$time_start,
                       time_end = efforts$time_end,
                       AreaName = efforts$AreaName,
                       School = efforts$School,
                       Species = efforts$Species,
                       EffortUnits = efforts$EffortUnits
                     ),
                     FUN = sum)
colnames(efforts)[colnames(efforts)=="x"] <- "Effort"

#We multiply the longline effort (gear=L) by 100 (since effort is given in hundreds of hooks)
efforts_pivot_WCPFC$Effort<- efforts_pivot_WCPFC$Effort*100


# Reach the efforts harmonized DSD using a function in WCPFC_functions.R
efforts<-WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_WCPFC,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"WCPFC"
efforts$measurement <- "effort" 
}



