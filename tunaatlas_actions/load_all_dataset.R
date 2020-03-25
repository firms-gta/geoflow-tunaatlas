load_all_dataset <- function(entity, config, options){
  
  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_actions/load_codelist.R")
  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_actions/load_dataset.R")
  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_actions/load_mapping.R")
  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_actions/create_codelist_materialized_view")
  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_actions/create_mappings_materialized_view")
  codelist_pid <- entity$identifiers[["id"]]
  
  if(!grepl(pattern = "mapping",x=codelist_pid) && !grepl(pattern = "level0", x=codelist_pid)){
    load_codelist(entity, config, options)
    create_codelist_materialized_view(entity, config, options)
  }else if(grepl(pattern = "mapping",x=codelist_pid)){
    load_mapping(entity, config, options)
    create_mappings_materialized_view(entity, config, options)
  } else if(grepl(pattern = "level0",x=codelist_pid)){
    load_dataset(entity, config, options)
  }
	
}