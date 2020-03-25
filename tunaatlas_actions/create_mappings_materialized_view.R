create_mappings_materialized_view <- function(entity, config, options){
  
  # set connection to database and other parameters from configuration
  CON = config$software$output$dbi
  entities = config$getEntities()
  last_entity = entities[[length(entities)]]
  dataset_pid <- entity$identifiers[["id"]]
  
  #whent the last entity has been processed we launch the creation of materialized views from normal views
  if(dataset_pid == last_entity$identifiers[["id"]]){
    
    config$logger.info(sprintf("\n Last entity, action create_mappings_materialized_view will be launched '%s' ",dataset_pid))
    sql_metadata<-paste("SELECT * FROM metadata.metadata WHERE dataset_type = 'mapping';")
    list_metadata<-dbGetQuery(CON, sql_metadata)
    schemas <-unique(gsub("\\..*","",list_metadata$database_table_name))
    config$logger.info(sprintf("\n List schemas => '%s' ",schemas))
    
    for(v in 1:length(schemas)){
      config$logger.info(sprintf("\n Case of schema '%s' ",schemas[v]))
      
      sql_views<-paste0("SELECT * FROM information_schema.tables WHERE table_schema IN ('",schemas[v],"') AND table_type='VIEW' AND table_name LIKE '%_mapping_view';")
      list_views<-dbGetQuery(CON, sql_views)
      config$logger.info(sprintf("\n SQL list views '%s' ",list_views))
      
      if(!is.null(list_views$table_name[1])){
        
        config$logger.info(sprintf("\n Usual case"))
        view_name <- paste0(list_views$table_schema[1],".",list_views$table_name[1])
        view_def <- dbGetQuery(CON,paste0("SELECT definition FROM (select pg_get_viewdef('",view_name,"', true) AS definition) AS view_def"))
        dbGetQuery(CON,paste0("DROP VIEW IF EXISTS ",view_name,";"))
      }
      else{
        config$logger.info(sprintf("\n Problemos"))
      }
      
      sql_mat_view <- paste0("CREATE MATERIALIZED VIEW ",view_name," AS ",view_def$definition)
      config$logger.info(sprintf("\n SQL mat view '%s' ",sql_mat_view))
      create_materialized_view <- dbGetQuery(CON,sql_mat_view) 
      
    }
    
  }else{
    config$logger.info(sprintf("\n Waiting for last entity to launch this action '%s' ",dataset_pid))
  }
  
}