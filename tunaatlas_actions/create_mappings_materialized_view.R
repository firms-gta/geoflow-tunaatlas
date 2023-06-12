create_mappings_materialized_view <- function(action,entity, config){
  opts <- action$options
  # set connection to database and other parameters from configuration
  CON = config$software$output$dbi
  entities = config$getEntities()
  last_entity = entities[[length(entities)]]
  dataset_pid <- entity$identifiers[["id"]]
  
  #whent the last entity has been processed we launch the creation of materialized views from normal views
  if(dataset_pid == last_entity$identifiers[["id"]]){
    
    config$logger.info(sprintf("\n Last entity, action create_mappings_materialized_view will be launched '%s' ",dataset_pid))
    views<-dbGetQuery(CON, "SELECT * FROM information_schema.tables WHERE table_type='VIEW' AND table_name LIKE '%_mapping_view' and table_schema NOT IN('pg_catalog','public');")
    config$logger.info(sprintf("\n List views => '%s' ",views))
    
    for(v in 1:nrow(views)){
      config$logger.info(sprintf("\n Case of schema '%s' ",views$table_name[v]))
      
      this_view <- paste0(views$table_schema[v],".",views$table_name[v])
      view_def <- dbGetQuery(CON,paste0("SELECT definition FROM (select pg_get_viewdef('",this_view,"', true) AS definition) AS view_def"))
      view_comment <- dbGetQuery(CON,paste0("SELECT c.relname table_name, pg_catalog.obj_description(c.oid) as comment 
                                            FROM pg_catalog.pg_class c where c.relname = '",views$table_name[v],"';"))
      sql_columns_comments <- paste0("SELECT a.attname As column_name,  d.description
   FROM pg_class As c
    INNER JOIN pg_attribute As a ON c.oid = a.attrelid
   LEFT JOIN pg_namespace n ON n.oid = c.relnamespace
   LEFT JOIN pg_tablespace t ON t.oid = c.reltablespace
   LEFT JOIN pg_description As d ON (d.objoid = c.oid AND d.objsubid = a.attnum)
   WHERE  c.relkind IN('r', 'v') AND  n.nspname = '",views$table_schema[v],"' AND c.relname = '",views$table_name[v],"'
   ORDER BY n.nspname, c.relname, a.attname ;")
      view_columns_comments <- dbGetQuery(CON,sql_columns_comments)
      
      dbGetQuery(CON,paste0("DROP VIEW IF EXISTS ",this_view,";"))
      
      config$logger.info(sprintf("\n Replace view '%s' by materialized view",views$table_name[v]))
      sql_mat_view <- paste0("CREATE MATERIALIZED VIEW ",this_view," AS ",view_def$definition)
      config$logger.info(sprintf("\n SQL mat view '%s' ",sql_mat_view))
      create_materialized_view <- dbGetQuery(CON,sql_mat_view) 
      
      config$logger.info(sprintf("\n Add comments for view '%s' and related columns ",views$table_name[v]))
      sql_view_comment <- paste0("COMMENT ON MATERIALIZED VIEW ",this_view," IS '",view_comment$comment[1],"';")
      dbGetQuery(CON,sql_view_comment)
      
      for(c in 1:nrow(view_columns_comments)){
        this_column <-paste0(this_view,".",view_columns_comments$column_name[c])
        sql_comment <- paste0("COMMENT ON COLUMN ",this_column," IS '",view_columns_comments$description[c],"';")
        config$logger.info(sprintf("\n %s",sql_comment))
        dbGetQuery(CON,sql_comment)
      }
      
    }
  }else{
    config$logger.info(sprintf("\n Waiting for last entity to launch this action '%s' ",dataset_pid))
  }
  
}
