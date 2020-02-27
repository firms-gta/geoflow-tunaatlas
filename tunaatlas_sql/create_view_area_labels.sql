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


COMMENT ON MATERIALIZED VIEW area.area_labels IS 'View gathering all the codes and labels of the code lists available for the dimension area (spatial code lists)';
