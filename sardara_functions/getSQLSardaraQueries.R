getSQLSardaraQueries <-  function (con, dataset_metadata) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/get_codelist_of_dimension.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/list_dataset_available_dimensions.R")
  if (nrow(dataset_metadata) == 0) {
    stop("There is no dataset that corresponds to your query")
  }
  static_metadata_table_name <- dataset_metadata$database_table_name
  static_metadata_id <- dataset_metadata$id_metadata
  static_metadata_permanent_id <- dataset_metadata$persistent_identifier
  static_metadata_dataset_origin_institution <- dataset_metadata$source
  static_metadata_table_type <- dataset_metadata$dataset_type
  static_metadata_table_description <- dataset_metadata$description
  static_metadata_dataset_name <- dataset_metadata$identifier
  static_metadata_table_sql_query <- dataset_metadata$sql_query_dataset_extraction
  static_metadata_table_dataset_title <- dataset_metadata$title
  static_metadata_table_view_name <- dataset_metadata$database_view_name
  SQL <- list()
  SQL$dynamic_metadata_count_features = NULL
  SQL$SRID = NULL
  SQL$dynamic_metadata_spatial_Extent = NULL
  SQL$dynamic_metadata_temporal_Extent = NULL
  SQL$query_wfs_wms_aggregated_layer = NULL
  SQL$query_materialized_view_sardara = NULL
  SQL$lineage <- paste("SELECT md_mp.*, md.database_table_name, md.database_view_name, md.dataset_type, md.identifier, md.title FROM metadata.metadata_mapping md_mp, metadata.metadata md WHERE md_mp.metadata_mapping_id_from IN ('", 
                       static_metadata_id, "')  AND  metadata_mapping_id_from=md.id_metadata;", 
                       sep = "")
  SQL$query_dynamic_list_keywords <- paste("\n  (WITH id_metadata_genealogy AS (\n  SELECT ", 
                                           static_metadata_id, " UNION SELECT metadata_mapping_id_to\n  FROM metadata.metadata_mapping\n  WHERE metadata_mapping_id_from=", 
                                           static_metadata_id, "\n  )\n  SELECT DISTINCT source as keyword, 'ORIGIN_INSTITUTIONS' as thesaurus \n  FROM metadata.metadata  \n  WHERE id_metadata IN \n  (SELECT * FROM id_metadata_genealogy)\n   ) ", 
                                           sep = "")
  if (static_metadata_table_type == "codelist") {
    all_column_names <- dbGetQuery(con, paste0("SELECT column_name from information_schema.columns where table_schema||'.'||table_name='", 
                                               static_metadata_table_name, "'"))
    all_column_names_vector <- as.vector(all_column_names$column_name)
    all_column_names_vector <- setdiff(all_column_names_vector, 
                                       "geom")
    all_column_names <- paste(as.character(all_column_names_vector), 
                              collapse = ", ", sep = "")
    if (substr(static_metadata_table_name, 1, 4) == "area") {
      table_geometry_information <- dbGetQuery(con, paste0("select * from geometry_columns where f_table_schema='area' and f_table_name='", 
                                                           substring(static_metadata_table_name, 6), "'"))
      SQL$query_wfs_wms <- paste0("SELECT ", all_column_names, 
                                  ",", table_geometry_information$f_geometry_column, 
                                  " as the_geom FROM ", static_metadata_table_name)
      SQL$query_CSV <- paste0("SELECT ", all_column_names, 
                              ",", "st_astext(", table_geometry_information$f_geometry_column, 
                              ") as geom_wkt FROM ", static_metadata_table_name)
    }
    else {
      SQL$query_CSV <- paste("SELECT ", all_column_names, 
                             " FROM ", static_metadata_table_name, sep = "")
      SQL$query_wfs_wms <- SQL$query_CSV
    }
    SQL$query_CSV_with_labels = SQL$query_CSV
  }
  else if (static_metadata_table_type == "mapping") {
    dimension <- sub("\\..*", "", static_metadata_table_name)
    SQL$query_CSV <- paste0("SELECT \nsub1.codesource as src_code,\n--sub1.label as src_label,\n                        sub2.codetarget as trg_code,\n                        --sub2.codetarget as trg_label,\n                        sub1.db_tablesource as src_codingsystem,\n                        sub2.db_tabletarget as trg_codingsystem\n                        FROM\n                        ( SELECT ", 
                            dimension, ".id_", dimension, " AS db_idsource,\n                        ", 
                            dimension, ".codesource_", dimension, " AS codesource,\n                        ", 
                            dimension, ".tablesource_", dimension, " AS db_tablesource\n                        FROM ", 
                            dimension, ".", dimension, "\n                        JOIN metadata.metadata ON metadata.id_metadata = ", 
                            dimension, ".id_metadata\n                        WHERE ", 
                            dimension, ".tablesource_", dimension, " = (SELECT distinct src.tablesource_", 
                            dimension, "\n                        FROM ", static_metadata_table_name, 
                            "\n                        JOIN ", dimension, ".", 
                            dimension, " src ON src.id_", dimension, "=", dimension, 
                            "_mapping.", dimension, "_mapping_id_from\n                        WHERE ", 
                            dimension, "_mapping.id_metadata=", static_metadata_id, 
                            ")::text) sub1\n                        LEFT JOIN ( SELECT ", 
                            dimension, "_mapping.", dimension, "_mapping_id_from,\n                        ", 
                            dimension, "_mapping.", dimension, "_mapping_id_to AS db_idtarget,\n                        ", 
                            dimension, ".codesource_", dimension, " AS codetarget,\n                        ", 
                            dimension, ".tablesource_", dimension, " AS db_tabletarget\n                        FROM ", 
                            static_metadata_table_name, "\n                        JOIN ", 
                            dimension, ".", dimension, " ON ", dimension, ".id_", 
                            dimension, " = ", dimension, "_mapping.", dimension, 
                            "_mapping_id_to\n                        JOIN metadata.metadata ON metadata.id_metadata = ", 
                            dimension, ".id_metadata\n                        WHERE  ", 
                            dimension, "_mapping.id_metadata=", static_metadata_id, 
                            " AND ", dimension, ".tablesource_", dimension, 
                            " = (SELECT distinct trg.tablesource_", dimension, 
                            "\n                        FROM ", static_metadata_table_name, 
                            "\n                        JOIN ", dimension, ".", 
                            dimension, " trg ON trg.id_", dimension, "=", dimension, 
                            "_mapping.", dimension, "_mapping_id_to\n                        WHERE ", 
                            dimension, "_mapping.id_metadata=", static_metadata_id, 
                            ")::text) sub2 ON sub1.db_idsource = sub2.", dimension, 
                            "_mapping_id_from\n                        ORDER BY sub2.db_tabletarget,sub1.codesource")
    SQL$query_wfs_wms <- SQL$query_CSV
    SQL$query_CSV_with_labels = SQL$query_CSV
  }
  else if (static_metadata_table_type == "raw_dataset") {
    db_dimensions_parameters<-read.csv("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/data/db_dimensions_parameters.csv",stringsAsFactors = F,strip.white=TRUE)
    dataset_available_dimensions <- list_dataset_available_dimensions(con, 
                                                                      dataset_metadata)
    tables_views_materializedviews <- dbGetQuery(con, "\n    SELECT table_schema||'.'||table_name FROM information_schema.tables\n    union\n    SELECT oid::regclass::text FROM   pg_class WHERE  relkind = 'm'")$`?column?`
    where_clause_gear <- NULL
    where_clause_species <- NULL
    if ("gear" %in% dataset_available_dimensions) {
      codelist_identifier <-  get_codelist_of_dimension(con, 
                                                                   dataset_metadata, "gear")$identifier
      if (codelist_identifier %in% c("isscfg_revision_1", 
                                     "gear_iotc", "gear_iccat", "gear_iattc", "gear_wcpfc")) {
        db_dimensions_parameters$sql_select_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension == 
                                                                          "gear")] <- paste0(db_dimensions_parameters$sql_select_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension == 
                                                                                                                                                               "gear")], "gear_group,")
        db_dimensions_parameters$sql_select_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                "gear")] <- paste0(db_dimensions_parameters$sql_select_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                           "gear")], "geargroup_label.codesource_gear as gear_group,")
        db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                       "gear")] <- paste0(db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                                         "gear")], "geargroup_label.source_label as gear_group_label,")
        db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension == 
                                                                                 "gear")] <- paste0(db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                             "gear")], "gear_group_label,")
        db_dimensions_parameters$sql_join_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                              "gear")] <- paste0(db_dimensions_parameters$sql_join_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                       "gear")], " LEFT OUTER JOIN gear.gear_mapping ON gear_mapping.gear_mapping_id_from=tab.id_gear LEFT JOIN gear.gear_labels geargroup_label ON geargroup_label.id_gear=gear_mapping.gear_mapping_id_to ")
        switch(codelist_identifier, isscfg_revision_1 = {
          geargroup_mapping_identifier <- "codelist_mapping_isscfg_revision_1_geargroup_tunaatlas"
        }, gear_iotc = {
          geargroup_mapping_identifier <- "codelist_mapping_gear_iotc_geargroup_iotc"
        }, gear_iccat = {
          geargroup_mapping_identifier <- "codelist_mapping_gear_iccat_geargroup_iccat"
        }, gear_iattc = {
          geargroup_mapping_identifier <- "codelist_mapping_gear_iattc_geargroup_iattc"
        }, gear_wcpfc = {
          geargroup_mapping_identifier <- "codelist_mapping_gear_wcpfc_geargroup_wcpfc"
        })
        where_clause_gear <- paste0("(gear_mapping.id_metadata=(SELECT id_metadata FROM metadata.metadata WHERE identifier='", 
                                    geargroup_mapping_identifier, "' ) OR  gear_mapping.gear_mapping_id_from=0) AND ")
      }
    }
    if ("species" %in% dataset_available_dimensions) {
      codelist_identifier <-  get_codelist_of_dimension(con, 
                                                                   dataset_metadata, "species")$identifier
      if (codelist_identifier == "species_asfis") {
        db_dimensions_parameters$sql_select_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension == 
                                                                          "species")] <- paste0(db_dimensions_parameters$sql_select_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                  "species")], "species_group,")
        db_dimensions_parameters$sql_select_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                "species")] <- paste0(db_dimensions_parameters$sql_select_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                              "species")], "speciesgroup_label.codesource_species as species_group,")
        db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                       "species")] <- paste0(db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                                            "species")], "speciesgroup_label.source_label as species_group_label,")
        db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension == 
                                                                                 "species")] <- paste0(db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                                "species")], "species_group_label,")
        db_dimensions_parameters$sql_join_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                              "species")] <- paste0(db_dimensions_parameters$sql_join_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension == 
                                                                                                                                                                          "species")], " LEFT OUTER JOIN species.species_mapping ON species_mapping.species_mapping_id_from=tab.id_species LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species=species_mapping.species_mapping_id_to ")
        speciesgroup_mapping_identifier <- "codelist_mapping_species_asfis_speciesgroup_tunaatlas"
        where_clause_species <- paste0("(species_mapping.id_metadata=(SELECT id_metadata FROM metadata.metadata WHERE identifier='", 
                                       speciesgroup_mapping_identifier, "' ) OR  species_mapping.species_mapping_id_from=0) AND ")
      }
    }
    if (tolower(static_metadata_table_view_name) %in% tables_views_materializedviews) {
      columns_csv_wms_wfs <- strsplit(paste(db_dimensions_parameters$sql_select_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension %in% 
                                                                                                              dataset_available_dimensions)], collapse = ""), 
                                      split = ",")[[1]]
      columns_netcdf <- strsplit(paste(db_dimensions_parameters$sql_select_netcdf_from_view[which(db_dimensions_parameters$dimension %in% 
                                                                                                    dataset_available_dimensions)], collapse = ""), 
                                 split = ",")[[1]]
      columns_csv_wms_wfs_with_labels <- c(columns_csv_wms_wfs, 
                                           strsplit(paste(db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_view[which(db_dimensions_parameters$dimension %in% 
                                                                                                                                   dataset_available_dimensions)], collapse = ""), 
                                                    split = ",")[[1]])
      join_clause <- " LEFT JOIN area.area_labels USING (id_area) "
      where_clause <- NULL
      tab_name <- static_metadata_table_view_name
    }
    else {
      columns_csv_wms_wfs <- strsplit(paste(db_dimensions_parameters$sql_select_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension %in% 
                                                                                                                    dataset_available_dimensions)], collapse = ""), 
                                      split = ",")[[1]]
      columns_netcdf <- strsplit(paste(db_dimensions_parameters$sql_select_netcdf_from_fact_table[which(db_dimensions_parameters$dimension %in% 
                                                                                                          dataset_available_dimensions)], collapse = ""), 
                                 split = ",")[[1]]
      columns_csv_wms_wfs_with_labels <- c(columns_csv_wms_wfs, 
                                           strsplit(paste(db_dimensions_parameters$sql_select_labels_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension %in% 
                                                                                                                                         dataset_available_dimensions)], collapse = ""), 
                                                    split = ",")[[1]])
      join_clause <- db_dimensions_parameters$sql_join_csv_wms_wfs_from_fact_table[which(db_dimensions_parameters$dimension %in% 
                                                                                           dataset_available_dimensions)]
      where_clause <- paste0("WHERE ", where_clause_gear, 
                             where_clause_species, "tab.id_metadata=", static_metadata_id)
      tab_name <- paste(static_metadata_table_name, " tab", 
                        sep = " ")
    }
    select_query_csv_wms_wfs <- paste(columns_csv_wms_wfs, 
                                      collapse = ",", sep = "")
    select_query_netcdf <- paste(columns_netcdf, collapse = ",", 
                                 sep = "")
    select_query_csv_wms_wfs_with_labels <- paste(columns_csv_wms_wfs_with_labels, 
                                                  collapse = ",", sep = "")
    join_clause <- paste(join_clause, collapse = " ", sep = "")
    geo_identifier_column <- dbGetQuery(con, paste0("select distinct(tablesource_area) from ", 
                                                    dataset_metadata$database_table_name, " tab join area.area tab_link on tab_link.id_area=tab.id_area where tab.id_area<>0 and tab.id_metadata=", 
                                                    dataset_metadata$id_metadata))$tablesource_area
    if (tolower(static_metadata_table_view_name) %in% tables_views_materializedviews) {
      column_names_and_types_dataset <- dbGetQuery(con, 
                                                   paste0("SELECT a.attname,pg_catalog.format_type(a.atttypid, a.atttypmod)\n                                                      FROM pg_attribute a\n                                                      JOIN pg_class t on a.attrelid = t.oid\n                                                      JOIN pg_namespace s on t.relnamespace = s.oid\n                                                      WHERE a.attnum > 0 \n                                                      AND NOT a.attisdropped\n                                                      AND s.nspname||'.'||t.relname = '", 
                                                          tolower(dataset_metadata$database_view_name), 
                                                          "' \n                                                      ORDER BY a.attnum"))
      columns_wms_wfs_where_clause <- setdiff(columns_csv_wms_wfs, 
                                              c("time_start", "time_end"))
      if (geo_identifier_column %in% c("area_wkt", "cwp_grid", 
                                       "areas_tuna_rfmos_task2")) {
        columns_wms_wfs_where_clause <- setdiff(columns_wms_wfs_where_clause, 
                                                "geographic_identifier")
      }
      where_query_wms_wfs <- NULL
      for (i in 1:length(columns_wms_wfs_where_clause)) {
        columns_wms_wfs_where_clause[i] <- gsub(",", 
                                                "", columns_wms_wfs_where_clause[i])
        if (column_names_and_types_dataset$format_type[which(column_names_and_types_dataset$attname == 
                                                             columns_wms_wfs_where_clause[i])] %in% c("integer", 
                                                                                                      "numeric", "real")) {
          cast_numeric <- "::numeric"
        }
        else {
          cast_numeric <- NULL
        }
        where_query_wms_wfs <- paste(where_query_wms_wfs, 
                                     paste0(columns_wms_wfs_where_clause[i], " IN ( SELECT regexp_split_to_table(regexp_replace('%", 
                                            columns_wms_wfs_where_clause[i], "%',' ', '+', 'g'),E'\\\\+')", 
                                            cast_numeric, " )"), sep = " AND ")
      }
      if (any(columns_csv_wms_wfs == "time_start")) {
        where_query_wms_wfs <- paste0(where_query_wms_wfs, 
                                      " AND time_start>='%time_start%' AND time_end<='%time_end%' ")
      }
      where_query_wms_wfs <- substring(where_query_wms_wfs, 
                                       6)
    }
    if (grepl("nominal_catch", static_metadata_dataset_name) | 
        grepl("eez", static_metadata_dataset_name)) {
      geo_attributes <- ",st_astext(ST_Envelope(geom)) as geom_wkt,st_x(ST_Centroid(geom)) as longitude,st_y(ST_Centroid(geom)) as latitude"
      geo_attributes_NetCDF <- ",area_labels.source_label as geographic_identifier_label,st_astext(ST_Envelope(geom)) as geom_wkt"
    }
    else {
      geo_attributes <- ",st_astext(ST_Envelope(geom)) as geom_wkt,st_x(ST_Centroid(geom)) as longitude,st_y(ST_Centroid(geom)) as latitude"
      geo_attributes_NetCDF <- ",st_astext(ST_Envelope(geom)) as geom_wkt"
    }
    SQL$query_CSV <- paste("SELECT ", select_query_csv_wms_wfs, 
                           geo_attributes, ",measurement_value FROM ", tab_name, join_clause, 
                           where_clause, sep = " ")
    SQL$query_CSV_with_labels <- paste("SELECT ", select_query_csv_wms_wfs_with_labels, 
                                       geo_attributes, ",measurement_value FROM ", tab_name, join_clause, 
                                       where_clause, sep = " ")
    SQL$query_NetCDF <- paste("SELECT ", select_query_netcdf, 
                              geo_attributes_NetCDF, ",measurement_value FROM ", tab_name, 
                              join_clause, where_clause, sep = " ")
    if (tolower(static_metadata_table_view_name) %in% tables_views_materializedviews) {
      if (geo_identifier_column == "area_wkt") {
        select_query_csv_wms_wfs = gsub(",geographic_identifier", 
                                        "", select_query_csv_wms_wfs)
      }
      SQL$query_wfs_wms <- paste("SELECT ", select_query_csv_wms_wfs, 
                                 ",longitude,latitude,measurement_value,tab_geom.geom as the_geom FROM ", 
                                 static_metadata_table_view_name, " LEFT OUTER JOIN area.area_labels tab_geom USING (id_area) WHERE ", 
                                 where_query_wms_wfs, sep = "")
      SQL$query_wfs_wms_aggregated_layer <- paste("SELECT measurement_value,tab_geom.codesource_area as geographic_identifier,tab_geom.geom as the_geom from ( SELECT CASE '%aggregation_method%' WHEN 'sum' THEN sum(measurement_value) WHEN 'avg' THEN sum(measurement_value)/(select DATE_PART('year', '%time_end%'::date) - DATE_PART('year', '%time_start%'::date) ) END as measurement_value,id_area FROM ", 
                                                  static_metadata_table_view_name, " WHERE ", 
                                                  where_query_wms_wfs, " AND geographic_identifier<>'UNK' group by id_area) tab   LEFT OUTER JOIN area.area_labels tab_geom USING (id_area) ", 
                                                  sep = "")
    }
    else {
      SQL$query_wfs_wms <- "You must create a table or a materialized view out of the dataset prior to the creation of the WMS/WFS"
      SQL$query_wfs_wms_aggregated_layer <- "You must create a table or a materialized view out of the dataset prior to the creation of the WMS/WFS"
    }
    dataset_available_dimensions <- setdiff(dataset_available_dimensions, 
                                            c("time", "area", "sizeclass"))
    if (!(geo_identifier_column %in% c("area_wkt", "areas_tuna_rfmos_task2", 
                                       "cwp_grid"))) {
      dataset_available_dimensions <- c(dataset_available_dimensions, 
                                        "geographic_identifier")
    }
    for (i in 1:length(dataset_available_dimensions)) {
      SQL$query_dynamic_list_keywords <- paste0(SQL$query_dynamic_list_keywords, 
                                                " UNION (SELECT distinct ", dataset_available_dimensions[i], 
                                                "_label as keyword, '", toupper(dataset_available_dimensions[i]), 
                                                "' as thesaurus from ", dataset_metadata$database_view_name, 
                                                " where ", dataset_available_dimensions[i], 
                                                "_label is not null and  ", dataset_available_dimensions[i], 
                                                "_label<>'''' and  ", dataset_available_dimensions[i], 
                                                "_label<>'')")
    }
    SQL$query_dynamic_list_keywords <- paste0(SQL$query_dynamic_list_keywords, 
                                              " order by thesaurus,keyword")
    SQL$query_dynamic_metadata_count_features <- paste("SELECT count(*) FROM ", 
                                                       static_metadata_table_name, " c WHERE c.id_metadata = ", 
                                                       static_metadata_id, ";", sep = "")
    SQL$query_dynamic_metadata_spatial_Extent <- paste("SELECT spatial_coverage FROM metadata.metadata WHERE id_metadata = ", 
                                                       static_metadata_id, ";", sep = "")
    SQL$query_dynamic_metadata_temporal_Extent <- paste("SELECT temporal_coverage FROM metadata.metadata WHERE id_metadata = ", 
                                                        static_metadata_id, ";", sep = "")
    SQL$query_dynamic_metadata_get_SRID <- paste("SELECT ST_SRID(geom) AS SRID FROM ", 
                                                 static_metadata_table_name, " c LEFT JOIN area.area_labels USING (id_area)  WHERE c.id_metadata = ", 
                                                 static_metadata_id, " LIMIT 1;", sep = "")
  }
  return(SQL)
}
