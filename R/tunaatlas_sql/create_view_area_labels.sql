
-- prerequisite
-- ALTER TABLE area.irregular_areas_task2_iotc ALTER column geom type geometry(GEOMETRY, 4326); -- missing

DROP VIEW IF EXISTS area.area_labels;

CREATE MATERIALIZED VIEW area.area_labels AS 
WITH vue AS (
SELECT 0 AS id_area,
    'UNK'::text AS codesource_area,
    NULL::text AS tablesource_area,
    'Unknown'::character varying AS source_label,
    'Inconnu'::character varying AS source_french_label,
    'Desconocido'::character varying AS source_spanish_label,
NULL::geometry AS geom
UNION
SELECT 
area.id_area,
area.codesource_area,
area.tablesource_area,
NULL::character varying as source_label,
NULL::character varying as source_french_label,
NULL::character varying as source_spanish_label,
geom 
FROM 
area.area_wkt tab 
JOIN area.area ON 
area.codesource_area=tab.code::text WHERE area.tablesource_area='area_wkt'::text
)
 SELECT vue.id_area,
    vue.codesource_area,
    vue.tablesource_area,
    vue.source_label,
    vue.source_french_label,
    vue.source_spanish_label,
    st_setsrid(vue.geom, 4326) AS geom
   FROM vue
;


ALTER TABLE area.area_labels
  OWNER TO "%db_admin%";
GRANT ALL ON TABLE area.area_labels TO "%db_admin%";

CREATE INDEX area_labels_id_area_idx  ON area.area_labels  (id_area);
CREATE INDEX area_labels_codesource_area_idx  ON area.area_labels  (codesource_area);

COMMENT ON MATERIALIZED VIEW "area"."area_labels" IS '"area_labels" materialized view which fasters the access to information often needed in data access queries. View gathering all the codes and labels of the code lists available for the dimension area (spatial code lists). View gathering all the codes and labels of the code lists available for the dimension area (spatial code lists)';
COMMENT ON COLUMN "area"."area_labels"."id_area" IS '"id_area" is the identifier (primary key) of the area.';
COMMENT ON COLUMN "area"."area_labels"."codesource_area" IS '"codesource_area" gives the geometry of the area as text (WKT format).';
COMMENT ON COLUMN "area"."area_labels"."tablesource_area" IS '"tablesource_area" gives the name of the physical table in the schema where this area is taken from.';
COMMENT ON COLUMN "area"."area_labels"."source_label" IS '"source_label" gives the label as it is delivered by the orginal dataset.';
COMMENT ON COLUMN "area"."area_labels"."source_french_label" IS '"source_french_label" gives the label of the area in french.';
COMMENT ON COLUMN "area"."area_labels"."source_spanish_label" IS '"source_spanish_label" gives the label of the area in spanish.';
COMMENT ON COLUMN "area"."area_labels"."geom" IS '"geom" is the geometry stored by Postgis (SFS format).';
