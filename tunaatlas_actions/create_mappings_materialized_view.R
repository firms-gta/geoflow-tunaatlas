create_mappings_materialized_view <- function(entity, config, options){
  
  # set connection to database and other parameters from configuration
  CON = config$software$output$dbi
  entities = config$getEntities()
  last_entity = entities[[length(entities)]]
  dataset_pid <- entity$identifiers[["id"]]
  
  #whent the last entity has been processed we launch the creation of materialized views from normal views
  if(dataset_pid == last_entity$identifiers[["id"]]){
    
    config$logger.info(sprintf("\n Last entity, action create_mappings_materialized_view will be launched '%s' ",dataset_pid))
    views<-dbGetQuery(CON, "SELECT * FROM information_schema.tables WHERE table_type='VIEW' AND table_name LIKE '%_mapping_view';")
    config$logger.info(sprintf("\n List views => '%s' ",views))
    
    for(v in 1:nrow(views)){
      config$logger.info(sprintf("\n Case of schema '%s' ",views$table_name[v]))
      this_view <- paste0(views$table_schema[v],".",views$table_name[v])
      view_def <- dbGetQuery(CON,paste0("SELECT definition FROM (select pg_get_viewdef('",this_view,"', true) AS definition) AS view_def"))
      view_comment <- dbGetQuery(CON,paste0("select c.relname table_name, pg_catalog.obj_description(c.oid) as comment from pg_catalog.pg_class c where c.relname = '",views$table_name[v],"';"))
      view_columns_comments <- dbGetQuery(CON,paste0("SELECT c.column_name, pgd.description FROM pg_catalog.pg_statio_all_tables as st inner join pg_catalog.pg_description pgd on (pgd.objoid=st.relid) inner join information_schema.columns c on (pgd.objsubid=c.ordinal_position and c.table_schema=st.schemaname and c.table_name=st.relname and c.table_name = '",views$table_name[v],"' and c.table_schema = '",views$table_schema[v],"');"))
      dbGetQuery(CON,paste0("DROP VIEW IF EXISTS ",this_view,";"))
      
      config$logger.info(sprintf("\n Replace view '%s' by materialized view",views$table_name[v]))
      
      sql_mat_view <- paste0("CREATE MATERIALIZED VIEW ",this_view," AS ",view_def$definition)
      config$logger.info(sprintf("\n SQL mat view '%s' ",sql_mat_view))
      create_materialized_view <- dbGetQuery(CON,sql_mat_view) 
      
      config$logger.info(sprintf("\n Add comments for view '%s'",views$table_name[v]))
      
      for(c in 1:nrow(view_columns_comments)){
        sql_comment <- paste0("COMMENT ON COLUMN ",view_columns_comments$column_name[c]," IS '",view_columns_comments$description[c],"';")
        config$logger.info(sprintf("\n %s",sql_comment))
        dbGetQuery(CON,sql_comment)
      }
    }
  }else{
    config$logger.info(sprintf("\n Waiting for last entity to launch this action '%s' ",dataset_pid))
  }
  
}
