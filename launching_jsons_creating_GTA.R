
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
# load_dot_env(file = "catch_server.env")


load_dot_env(file = "geoserver_sdi_lab.env")

executeWorkflow("tunaatlas_qa_dbmodel+codelists.json")#works
executeWorkflow("tunaatlas_qa_mappings.json")


executeWorkflow("tunaatlas_qa_datasets_iccat.json") # ok
executeWorkflow("tunaatlas_qa_datasets_ccsbt.json") # ok
executeWorkflow("tunaatlas_qa_datasets_wcpfc.json") # ok 
executeWorkflow("tunaatlas_qa_datasets_iattc.json") # ok
executeWorkflow("tunaatlas_qa_datasets_iotc.json") # ok


executeWorkflow("tunaatlas_qa_datasets_ccsbt_effort.json") #ok
executeWorkflow("tunaatlas_qa_datasets_wcpfc_effort.json") #ok
executeWorkflow("tunaatlas_qa_datasets_iattc_effort.json") # ok
executeWorkflow("tunaatlas_qa_datasets_iotc_effort.json") # ok
executeWorkflow("tunaatlas_qa_datasets_iccat_effort.json") # ok

executeWorkflow("tunaatlas_qa_global_datasets_catch.json")

executeWorkflow("tunaatlas_qa_global_datasets_effort.json")
executeWorkflow("tunaatlas_qa_services.json")
