load_all_dataset <- function(action,entity, config){
  opts <- action$options
  source(geoflow::get_config_resource_path(config, "./tunaatlas_actions/load_codelist.R"))
  source(geoflow::get_config_resource_path(config, "./tunaatlas_actions/load_dataset.R"))
  source(geoflow::get_config_resource_path(config, "./tunaatlas_actions/load_mapping.R"))
  source(geoflow::get_config_resource_path(config, "./tunaatlas_actions/create_codelist_materialized_view.R"))
  source(geoflow::get_config_resource_path(config, "./tunaatlas_actions/create_mappings_materialized_view.R"))
  codelist_pid <- entity$identifiers[["id"]]
  
  if(!grepl(pattern = "mapping",x=codelist_pid) && !grepl(pattern = "level0", x=codelist_pid)){
    load_codelist(entity, config, opts)
    create_codelist_materialized_view(entity, config, opts)
  }else if(grepl(pattern = "mapping",x=codelist_pid)){
    load_mapping(entity, config, opts)
    create_mappings_materialized_view(entity, config, opts)
  } else if(grepl(pattern = "level0",x=codelist_pid)){
    load_dataset(entity, config, opts)
  }
  
}