######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = west_pacific_ocean_effort_5deg_1m_ll_tunaatlaswcpfc_level0, title = Harmonize data structure of WCPFC Longline effort datasets, abstract = Harmonize the structure of WCPFC catch-and-effort datasets: 'longline_60' 'longline_70' 'longline_80' 'longline_90' 'longline_00' (pid of output file = west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__1950to1970 or west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__1970to1980 or west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__1980to1990 or west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__1990to2000 or west_pacific_ocean_effort_5deg_1m_ll_tunaatlasWCPFC_level0__2000). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/pLHB56, value = "https://goo.gl/pLHB56";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' This script works with any dataset that has the first 5 columns named and ordered as follow: {YY|MM|LAT5|LON5|HHOOKS} followed by a list of columns specifing the species codes with "_N" for catches expressed in number and "_T" for catches expressed in tons
#' 
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Western and Central Pacific Fisheries Commission WCPFC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_wcpfc_ce_Driftnet}} to convert WCPFC task 2 Drifnet data structure, \code{\link{convertDSD_wcpfc_ce_Longline}} to convert WCPFC task 2 Longline data structure, \code{\link{convertDSD_wcpfc_ce_Pole_and_line}} to convert WCPFC task 2 Pole-and-line data structure, \code{\link{convertDSD_wcpfc_ce_PurseSeine}} to convert WCPFC task 2 Purse seine data structure, \code{\link{convertDSD_wcpfc_nc}} to convert WCPFC task 1 data structure  


if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
}
if(!require(foreign)){
  install.packages("foreign")
}

require(rtunaatlas)
require(foreign)

wd<-getwd()
download.file(path_to_raw_dataset,destfile=paste(wd,"/dbf_file.DBF",sep=""), method='auto', quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
path_to_raw_dataset=paste(wd,"/dbf_file.DBF",sep="")



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


##Efforts

# Reach the efforts pivot DSD using a function in WCPFC_functions.R
efforts_pivot_WCPFC<-FUN_efforts_WCPFC_CE (path_to_raw_dataset)
efforts_pivot_WCPFC$Gear<-"L"

#We multiply the longline effort (gear=L) by 100 (since effort is given in hundreds of hooks)
efforts_pivot_WCPFC$Effort<- efforts_pivot_WCPFC$Effort*100


# Reach the efforts harmonized DSD using a function in WCPFC_functions.R
colToKeep_efforts <- c("Flag","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
efforts<-WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_WCPFC,colToKeep_efforts)

colnames(efforts)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","unit","value")
efforts$source_authority<-"WCPFC"
dataset<-efforts

### Compute metadata
#if (path_to_metadata_file!="NULL"){
#  source("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/transform/compute_metadata.R")
#} else {
#  df_metadata<-NULL
#  df_codelists<-NULL
#}


## To check the outputs:
# str(dataset)
# str(df_metadata)
# str(df_codelists)



