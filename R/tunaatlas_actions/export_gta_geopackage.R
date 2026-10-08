# Export of the Tuna Atlas database to GeoPackage
# -----------------------------------------------------------------------------
# The GeoPackage is a copy of the relational model: every table of every
# schema (fact tables, dimensions, labels, source code lists, grids, metadata).
# The materialized views of fact_tables (flat datasets) can be added on top.
#
# Arguments of export_gta_geopackage():
#   identifiers   NULL  : the whole database
#                 c(..) : fact tables and metadata restricted to these datasets
#                         (all other tables are still copied entirely)
#   include_views TRUE  : also export the materialized views of fact_tables
#                         (only those of the listed datasets if identifiers
#                         is given)
#
# Writing goes through GDAL (ogr2ogr) via sf::gdal_utils: data are streamed
# from PostgreSQL to the file, without being loaded into R.
#
# Note: GDAL copies tables and data, not the foreign key constraints. The
# relations are kept through the id_* columns, which have the same name in
# the fact tables and in the dimension tables.
# -----------------------------------------------------------------------------

library(DBI)
library(sf)

# Schemas that are not part of the data model
GTA_EXCLUDED_SCHEMAS <- c("pg_catalog", "information_schema", "topology",
                          "tiger", "tiger_data")

# GDAL connection string
gta_pg_dsn <- function(host, port, dbname, user, password) {
  sprintf("PG:host=%s port=%s dbname=%s user=%s password=%s",
          host, port, dbname, user, password)
}

# Identifiers of the datasets listed in metadata.metadata_dcmi.
# The "Identifier" column uses the geoflow format: "id:xxx_\nid_version:yyy"
gta_dcmi_identifiers <- function(con) {
  x <- dbGetQuery(con, 'SELECT "Identifier" FROM metadata.metadata_dcmi')[[1]]
  first_line <- vapply(strsplit(x, "\n"), function(l) trimws(l[1]), character(1))
  unique(sub("_$", "", sub("^id:", "", first_line)))
}

export_gta_geopackage <- function(con, dsn, out, identifiers = NULL,
                                  include_views = TRUE) {
  
  if (file.exists(out)) unlink(out)
  
  add_layer <- function(layer, sql, multipolygon = FALSE) {
    opts <- c("-f", "GPKG",
              if (file.exists(out)) "-update",
              "-nln", layer,
              "-sql", sql,
              if (multipolygon) c("-nlt", "MULTIPOLYGON", "-a_srs", "EPSG:4326"))
    sf::gdal_utils("vectortranslate", source = dsn, destination = out,
                   options = opts)
    message("exported: ", layer)
  }
  
  # --- Optional filters on datasets ------------------------------------------
  where_meta <- ""
  where_fact <- ""
  if (!is.null(identifiers)) {
    ids_sql <- paste(as.character(dbQuoteLiteral(con, identifiers)),
                     collapse = ", ")
    where_meta <- sprintf(" WHERE identifier IN (%s)", ids_sql)
    where_fact <- sprintf(
      " WHERE id_metadata IN (SELECT id_metadata FROM metadata.metadata%s)",
      where_meta)
  }
  
  # --- 1. All the tables of the database -------------------------------------
  tables <- dbGetQuery(con, sprintf(
    "SELECT table_schema, table_name
     FROM information_schema.tables
     WHERE table_type = 'BASE TABLE'
       AND table_schema NOT IN (%s)
       AND table_name <> 'spatial_ref_sys'
     ORDER BY 1, 2",
    paste(sprintf("'%s'", GTA_EXCLUDED_SCHEMAS), collapse = ", ")))
  
  # Fact tables that can be filtered by dataset (they have an id_metadata column)
  fact_with_meta <- dbGetQuery(con,
                               "SELECT table_name FROM information_schema.columns
     WHERE table_schema = 'fact_tables' AND column_name = 'id_metadata'")$table_name
  
  # Materialized views of fact_tables to export (flat datasets, with geometry)
  views <- character(0)
  if (include_views) {
    views <- dbGetQuery(con,
                        "SELECT matviewname FROM pg_matviews WHERE schemaname = 'fact_tables'
       ORDER BY 1")$matviewname
    if (!is.null(identifiers)) {
      missing <- setdiff(identifiers, views)
      if (length(missing) > 0)
        warning("No materialized view for: ", paste(missing, collapse = ", "))
      views <- intersect(views, identifiers)
    }
    message("materialized views to export: ",
            if (length(views) > 0) paste(views, collapse = ", ") else "none")
  }
  
  # GeoPackage has no schemas: the table name is used as layer name, prefixed
  # by the schema when the same name exists in several schemas or is already
  # used by a materialized view (the view keeps the plain dataset name)
  dup <- duplicated(tables$table_name) | duplicated(tables$table_name, fromLast = TRUE) |
    tables$table_name %in% views
  tables$layer <- ifelse(dup,
                         paste(tables$table_schema, tables$table_name, sep = "_"),
                         tables$table_name)
  
  for (i in seq_len(nrow(tables))) {
    schema <- tables$table_schema[i]
    name <- tables$table_name[i]
    where <- ""
    if (schema == "fact_tables" && name %in% fact_with_meta) where <- where_fact
    if (schema == "metadata" && name == "metadata") where <- where_meta
    add_layer(tables$layer[i],
              sprintf('SELECT * FROM "%s"."%s"%s', schema, name, where))
  }
  
  # --- 2. Materialized views: flat datasets, with geometry (optional) --------
  if (include_views) {
    for (v in views) {
      cols <- dbGetQuery(con, sprintf(
        "SELECT attname, quote_ident(attname) AS col
         FROM pg_attribute
         WHERE attrelid = 'fact_tables.%s'::regclass
           AND attnum > 0 AND NOT attisdropped
         ORDER BY attnum", v))
      # Drop the internal id and the WKT text (replaced by a real geometry)
      keep <- paste(cols$col[!cols$attname %in% c("geom_wkt", "id_area")],
                    collapse = ", ")
      if ("geom_wkt" %in% cols$attname) {
        add_layer(v, sprintf(
          "SELECT %s, ST_GeomFromText(NULLIF(geom_wkt, ''), 4326) AS geom
           FROM fact_tables.%s", keep, v), multipolygon = TRUE)
      } else {
        add_layer(v, sprintf("SELECT %s FROM fact_tables.%s", keep, v))
      }
    }
  }
  
  invisible(out)
}

# onend hook for geoflow: export the database to two GeoPackage files
onend_export_gta_geopackage <- function(config, software, software_config) {
  
  con <- config$software$output$dbi
  p <- config$software$output$dbi_config$parameters
  dsn <- gta_pg_dsn(p$host, p$port, p$dbname, p$user, p$password)
  
  # 1) Whole database
  export_gta_geopackage(con, dsn,
                        file.path(config$job, "global_tuna_atlas_full.gpkg"))
  
  # 2) Only the datasets listed in metadata.metadata_dcmi
  export_gta_geopackage(con, dsn,
                        file.path(config$job, "global_tuna_atlas.gpkg"),
                        identifiers = gta_dcmi_identifiers(con))
  
  # geoflow executes the returned value as SQL: return a statement with no result set
  return("DO $$ BEGIN NULL; END $$;")
}

# -----------------------------------------------------------------------------
# Manual usage
# -----------------------------------------------------------------------------
# con <- dbConnect(RPostgres::Postgres(), host = "localhost", port = 15430,
#                  dbname = "gta", user = "gta", password = "gta")
# dsn <- gta_pg_dsn("localhost", 15430, "gta", "gta", "gta")
#
# # Whole database, tables only (no materialized views)
# export_gta_geopackage(con, dsn, "global_tuna_atlas_full.gpkg",
#                       include_views = FALSE)
#
# # Only the datasets listed in metadata.metadata_dcmi
# export_gta_geopackage(con, dsn, "global_tuna_atlas.gpkg",
#                       identifiers = gta_dcmi_identifiers(con))
#
# dbDisconnect(con)
