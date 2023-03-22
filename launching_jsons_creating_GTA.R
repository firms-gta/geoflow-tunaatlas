
# if(!require(renv)){
#     install.packages("renv")
#     require(renv)
#   }
# renv::restore()
# #
if(!require(remotes)){
  install.packages("remotes")
  require(remotes)
}




if(!require(tinytex)){
  install.packages("tinytex")
  require(tinytex)
}
if(!require(geoflow)){
  remotes::install_github("r-geoflow/geoflow")
  require(geoflow)}

if(!require(RSQLite)){
  install.packages("RSQLite")
  require(RSQLite)
}
if(!require(RPostgreSQL)){
  install.packages("RPostgreSQL")
  require(RPostgreSQL)
}
if(!require(RPostgres)){
  install.packages("RPostgres")
  require(RPostgres)
}

if(!require(googledrive)){
  install.packages("googledrive")
  require(googledrive)
}
if(!require(DBI)){
  install.packages("DBI")
  require(DBI)
}
if(!require(gsheet)){
  install.packages("gsheet")
  require(gsheet)
}
if(!require(readr)){
  install.packages("readr")
  require(readr)
}
if(!require(plotrix)){
  install.packages("plotrix")
  require(plotrix)
}
if(!require(rgeos)){
  install.packages("rgeos")
  require(rgeos)
}


if(!require(rpostgis)){
  install.packages("rpostgis")
  require(rpostgis)
}
if(!require(janitor)){
  install.packages("janitor")
  require(janitor)
}

if(!require(dotenv)){
  install.packages("dotenv")
  require(dotenv)
}

if(!require(data.table)){
  install.packages("data.table")
  require(data.table)
} #to be removed 

default_file = ".env"

if(file.exists("geoserver_sdi_lab.env")){
  default_file <- "geoserver_sdi_lab.env"
} # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

load_dot_env(file =default_file) # to be replaced by the one used

# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/cwp_grids.R")

lapply(paste0("jobs/",list("tunaatlas_qa_dbmodel+codelists", "tunaatlas_qa_mappings","tunaatlas_qa_datasets_ccsbt", "tunaatlas_qa_datasets_iccat",
            "tunaatlas_qa_datasets_wcpfc", "tunaatlas_qa_datasets_iattc","tunaatlas_qa_datasets_iotc")),dir.create)

executeWorkflow("tunaatlas_qa_dbmodel+codelists.json", dir = "jobs/tunaatlas_qa_dbmodel+codelists")#works
executeWorkflow("tunaatlas_qa_mappings.json", dir = "jobs/tunaatlas_qa_mappings")


executeWorkflow("tunaatlas_qa_datasets_iccat.json", dir = "jobs/tunaatlas_qa_datasets_iccat") # ok
executeWorkflow("tunaatlas_qa_datasets_ccsbt.json", dir = "jobs/tunaatlas_qa_datasets_ccsbt") # ok
executeWorkflow("tunaatlas_qa_datasets_wcpfc.json", dir = "jobs/tunaatlas_qa_datasets_wcpfc") # ok 
executeWorkflow("tunaatlas_qa_datasets_iattc.json", dir = "jobs/tunaatlas_qa_datasets_iattc") # ok
executeWorkflow("tunaatlas_qa_datasets_iotc.json", dir = "jobs/tunaatlas_qa_datasets_iotc") # ok

lapply(paste0(paste0("jobs/",list("tunaatlas_qa_datasets_ccsbt", "tunaatlas_qa_datasets_iccat",
                           "tunaatlas_qa_datasets_wcpfc", "tunaatlas_qa_datasets_iattc","tunaatlas_qa_datasets_iotc")),"_effort"),dir.create)



executeWorkflow("tunaatlas_qa_datasets_ccsbt_effort.json", dir = "jobs/tunaatlas_qa_datasets_ccsbt_effort") #ok
executeWorkflow("tunaatlas_qa_datasets_wcpfc_effort.json", dir = "jobs/tunaatlas_qa_datasets_wcpfc_effort") #ok
executeWorkflow("tunaatlas_qa_datasets_iattc_effort.json", dir = "jobs/tunaatlas_qa_datasets_iattc_effort") # ok
executeWorkflow("tunaatlas_qa_datasets_iotc_effort.json", dir = "jobs/tunaatlas_qa_datasets_iotc_effort") # ok
executeWorkflow("tunaatlas_qa_datasets_iccat_effort.json", dir = "jobs/tunaatlas_qa_datasets_iccat_effort") # ok


lapply(paste0("jobs/",list("tunaatlas_qa_global_datasets_catch","tunaatlas_qa_global_datasets_effort")),dir.create)


executeWorkflow("tunaatlas_qa_global_datasets_catch.json", dir = "jobs/tunaatlas_qa_global_datasets_catch")

executeWorkflow("tunaatlas_qa_global_datasets_effort.json", dir = "jobs/tunaatlas_qa_global_datasets_effort")

dir.create("jobs/tunaatlas_qa_services")
executeWorkflow("tunaatlas_qa_services.json")
